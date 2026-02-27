{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Examples.ScenesNavigation
  ( runConcept
  ) where

import Prelude

import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import qualified Engine.Data.ECS as E
import qualified Engine.Data.Program as S
import qualified Engine.Data.Route.Simple as RouteS
import qualified Engine.Data.Scene as Scene

type SceneId = String

hostSender :: SceneId
hostSender = "/host"

sceneMainMenu :: SceneId
sceneMainMenu = "/main-menu"

sceneOptions :: SceneId
sceneOptions = "/options"

sceneGame :: SceneId
sceneGame = "/game"

data Target
  = ToScene SceneId
  | ToRouter
  deriving (Eq, Show)

data Msg
  = UiOpenOptions
  | UiCloseTop
  | UiStartGame
  | UiBackToMenu
  | NavOpenOptions
  | NavBack
  | NavGotoGame
  | NavGotoMenu
  deriving (Eq, Show)

data Envelope = Envelope
  { envFrom :: !SceneId
  , envTarget :: !Target
  , envMsg :: !Msg
  } deriving (Eq, Show)

newtype TickCount = TickCount Int
  deriving (Eq, Show)

newtype C
  = CTickCount TickCount
  deriving (Generic)

instance E.ComponentId C

newtype TickRow = TickRow
  { trTickCount :: TickCount
  } deriving (Generic)

type World = E.World C
type Program a = S.ProgramM C Envelope a

data Host = Host
  { hostHistory :: !(Scene.History SceneId)
  , hostScenes :: !(Map SceneId (Scene.SceneRuntime C Envelope))
  , hostMailbox :: ![Envelope]
  }

frameDt :: Double
frameDt = 0.016

tickProg :: Program ()
tickProg =
  void . S.await $
    S.each @TickRow $ \(TickRow (TickCount n)) ->
      S.set (TickCount (n + 1))

awaitMsg :: Msg -> Program ()
awaitMsg msg = void (S.await (eventIs msg))

sendRouterMsg :: SceneId -> Msg -> Program ()
sendRouterMsg from msg =
  S.send [Envelope from ToRouter msg]

mainMenuOpenProg :: Program ()
mainMenuOpenProg = do
  awaitMsg UiOpenOptions
  sendRouterMsg sceneMainMenu NavOpenOptions

mainMenuStartProg :: Program ()
mainMenuStartProg = do
  awaitMsg UiStartGame
  sendRouterMsg sceneMainMenu NavGotoGame

optionsCloseProg :: Program ()
optionsCloseProg = do
  awaitMsg UiCloseTop
  sendRouterMsg sceneOptions NavBack

gameBackProg :: Program ()
gameBackProg = do
  awaitMsg UiBackToMenu
  sendRouterMsg sceneGame NavGotoMenu

eventIs :: Msg -> Envelope -> Bool
eventIs m env = envMsg env == m

mkRuntime :: [Program ()] -> Scene.SceneRuntime C Envelope
mkRuntime scenePrograms =
  let (_, w1) = E.spawn (TickCount 0) (E.emptyWorld :: World)
      g1 =
        S.graph $ do
          _ <- S.program tickProg
          traverse_ S.program scenePrograms
  in Scene.mkScene w1 g1

sceneRoutes :: RouteS.Routes C Envelope
sceneRoutes =
  RouteS.route @"/main-menu" (\_ () -> mkRuntime [mainMenuOpenProg, mainMenuStartProg]) (Just ())
    RouteS.:> RouteS.route @"/options" (\_ () -> mkRuntime [optionsCloseProg]) (Just ())
    RouteS.:> RouteS.route @"/game" (\_ () -> mkRuntime [gameBackProg]) (Just ())
    RouteS.:> RouteS.EmptyRoutes

sceneRouter :: RouteS.Router C Envelope
sceneRouter =
  case RouteS.createRouter sceneRoutes of
    Left err ->
      error ("invalid scene router: " <> err)
    Right router ->
      router

sceneOrder :: [SceneId]
sceneOrder = [sceneMainMenu, sceneOptions, sceneGame]

sceneRuntimeMap :: Map SceneId (Scene.SceneRuntime C Envelope)
sceneRuntimeMap =
  Map.fromList
    [ (sid, mkSceneAt sid)
    | sid <- sceneOrder
    ]
  where
    mkSceneAt sid =
      case RouteS.enterPath sceneRouter sid of
        Just rt -> rt
        Nothing -> error ("missing scene route runtime for " <> sid)

acceptsScene :: SceneId -> Envelope -> Bool
acceptsScene sid env =
  case envTarget env of
    ToScene dst -> sid == dst
    ToRouter -> False

activeSceneIds :: Scene.History SceneId -> [SceneId]
activeSceneIds h = Scene.locationSegments (Scene.current h)

isActiveScene :: SceneId -> Scene.History SceneId -> Bool
isActiveScene sid h = sid `elem` Scene.locationSegments (Scene.current h)

gotoScenePath :: Scene.GotoMode -> SceneId -> Scene.History SceneId -> Scene.History SceneId
gotoScenePath mode sid h =
  RouteS.gotoPath mode sid sceneRouter h

stepHost :: Double -> [Envelope] -> Host -> (Host, [Envelope])
stepHost dt external host0 =
  let inbound = hostMailbox host0 <> external
      activeOrder = activeSceneIds (hostHistory host0)
      (scenes1, outbox) =
        foldl' (stepOne inbound) (hostScenes host0, []) activeOrder
      history1 = applyRouterEvents (hostHistory host0) outbox
      mailbox1 = filter ((/= ToRouter) . envTarget) outbox
      host1 =
        host0
          { hostHistory = history1
          , hostScenes = scenes1
          , hostMailbox = mailbox1
          }
  in (host1, outbox)
  where
    stepOne inbound (sceneMap, outAcc) sid =
      case Map.lookup sid sceneMap of
        Nothing -> (sceneMap, outAcc)
        Just rt0 ->
          let inbox = filter (acceptsScene sid) inbound
              (rt1, out1) = Scene.runScene dt inbox rt0
          in (Map.insert sid rt1 sceneMap, outAcc <> out1)

applyRouterEvents :: Scene.History SceneId -> [Envelope] -> Scene.History SceneId
applyRouterEvents h0 outbox =
  foldl'
    applyOne
    h0
    [envMsg env | env <- outbox, envTarget env == ToRouter]
  where
    applyOne h cmd =
      case cmd of
        NavOpenOptions ->
          if isActiveScene sceneOptions h
            then h
            else gotoScenePath Scene.Push sceneOptions h
        NavBack ->
          Scene.back h
        NavGotoGame ->
          gotoScenePath Scene.Push sceneGame h
        NavGotoMenu ->
          gotoScenePath Scene.Push sceneMainMenu h
        _ ->
          h

initialHost :: Host
initialHost =
  Host
      { hostHistory = Scene.history [sceneMainMenu]
      , hostScenes = sceneRuntimeMap
      , hostMailbox = []
      }

fromHost :: SceneId -> Msg -> Envelope
fromHost dst = Envelope hostSender (ToScene dst)

sceneTickCount :: Host -> SceneId -> Int
sceneTickCount host sid =
  case Map.lookup sid (hostScenes host) of
    Nothing -> 0
    Just rt ->
      case E.runq (E.query @TickRow @C) (Scene.sceneRuntimeWorld rt) of
        (_, TickRow (TickCount n)) : _ -> n
        [] -> 0

renderPath :: Scene.History SceneId -> String
renderPath h = unlines (go segs)
  where
    segs = Scene.locationSegments (Scene.current h)
    go xs =
      [ "path=" <> show xs
      , "path-render=" <> intercalate " > " xs
      , "canGoBack=" <> show (Scene.canGoBack h) <> ", canGoForward=" <> show (Scene.canGoForward h)
      ]

scriptedInputs :: [[Envelope]]
scriptedInputs =
  [ [fromHost sceneMainMenu UiOpenOptions]
  , [fromHost sceneOptions UiCloseTop]
  , [fromHost sceneMainMenu UiStartGame]
  , [fromHost sceneGame UiBackToMenu]
  , []
  ]

renderTickLine :: Host -> String
renderTickLine host =
  let active = activeSceneIds (hostHistory host)
      parts =
        [sid <> "=" <> show (sceneTickCount host sid) | sid <- active]
  in "ticks: " <> intercalate ", " parts

runConcept :: IO ()
runConcept = go 1 initialHost scriptedInputs
  where
    go :: Int -> Host -> [[Envelope]] -> IO ()
    go _ _ [] = pure ()
    go frameIx host0 (inputs : rest) = do
      let (host1, outbox) = stepHost frameDt inputs host0
          activeNow = activeSceneIds (hostHistory host1)
      putStrLn ("frame " <> show frameIx)
      putStrLn ("  inputs=" <> show (map envMsg inputs))
      putStrLn ("  outbox=" <> show (map envMsg outbox))
      putStrLn ("  active=" <> show activeNow)
      putStrLn ("  " <> renderTickLine host1)
      putStrLn (renderPath (hostHistory host1))
      go (frameIx + 1) host1 rest
