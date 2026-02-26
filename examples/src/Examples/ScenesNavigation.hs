{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Examples.ScenesNavigation
  ( runConcept
  ) where

import Prelude

import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import qualified Engine.Data.ECS as E
import qualified Engine.Data.Program as S
import qualified Engine.Data.Scene as Scene

type SceneId = String

sceneRoot :: SceneId
sceneRoot = "/root"

sceneMainMenu :: SceneId
sceneMainMenu = "/main-menu"

sceneOptions :: SceneId
sceneOptions = "/options"

sceneGame :: SceneId
sceneGame = "/game"

data Target
  = ToScene SceneId
  | ToRouter
  | Broadcast
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

data C
  = CTickCount TickCount
  deriving (Generic)

instance E.ComponentId C

data TickRow = TickRow
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
tickProg = do
  _ <- S.await $
    S.each @TickRow $ \(TickRow (TickCount n)) ->
      S.set (TickCount (n + 1))
  pure ()

mainMenuOpenProg :: Program ()
mainMenuOpenProg = do
  _ <- S.await (eventIs UiOpenOptions)
  S.send [Envelope sceneMainMenu ToRouter NavOpenOptions]

mainMenuStartProg :: Program ()
mainMenuStartProg = do
  _ <- S.await (eventIs UiStartGame)
  S.send [Envelope sceneMainMenu ToRouter NavGotoGame]

optionsCloseProg :: Program ()
optionsCloseProg = do
  _ <- S.await (eventIs UiCloseTop)
  S.send [Envelope sceneOptions ToRouter NavBack]

gameBackProg :: Program ()
gameBackProg = do
  _ <- S.await (eventIs UiBackToMenu)
  S.send [Envelope sceneGame ToRouter NavGotoMenu]

eventIs :: Msg -> Envelope -> Bool
eventIs m env = envMsg env == m

mkRuntime :: SceneId -> Scene.SceneRuntime C Envelope
mkRuntime sid =
  let (_, w1) = E.spawn (TickCount 0) (E.emptyWorld :: World)
      g1 =
        S.graph $ do
          _ <- S.program tickProg
          case sid of
            "/main-menu" -> do
              _ <- S.program mainMenuOpenProg
              _ <- S.program mainMenuStartProg
              pure ()
            "/options" -> do
              _ <- S.program optionsCloseProg
              pure ()
            "/game" -> do
              _ <- S.program gameBackProg
              pure ()
            _ -> pure ()
  in Scene.sceneRuntime w1 g1

acceptsScene :: SceneId -> Envelope -> Bool
acceptsScene sid env =
  case envTarget env of
    ToScene dst -> sid == dst
    ToRouter -> False
    Broadcast -> True

activeSceneIds :: Scene.History SceneId -> [SceneId]
activeSceneIds h =
  filter (/= sceneRoot) (Scene.locationSegments (Scene.current h))

stepRuntime :: Double -> [Envelope] -> Scene.SceneRuntime C Envelope -> (Scene.SceneRuntime C Envelope, [Envelope])
stepRuntime dt inbox rt0 =
  Scene.runScene dt inbox rt0

stepHost :: Double -> [Envelope] -> Host -> (Host, [Envelope])
stepHost dt external host0 =
  let inbound = hostMailbox host0 <> external
      sceneOrder = activeSceneIds (hostHistory host0)
      (scenes1, outbox) =
        foldl' (stepOne inbound) (hostScenes host0, []) sceneOrder
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
              (rt1, out1) = stepRuntime dt inbox rt0
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
          case reverse (Scene.locationSegments (Scene.current h)) of
            top : _ | top == sceneOptions -> h
            _ -> Scene.goto Scene.Push sceneOptions h
        NavBack ->
          Scene.back h
        NavGotoGame ->
          Scene.goto Scene.Push sceneGame h
        NavGotoMenu ->
          Scene.goto Scene.Push sceneMainMenu h
        _ ->
          h

initialHost :: Host
initialHost =
  let runtimeMap =
        Map.fromList
          [ (sceneMainMenu, mkRuntime sceneMainMenu)
          , (sceneOptions, mkRuntime sceneOptions)
          , (sceneGame, mkRuntime sceneGame)
          ]
  in Host
      { hostHistory = Scene.history [sceneMainMenu]
      , hostScenes = runtimeMap
      , hostMailbox = []
      }

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
  [ [Envelope sceneRoot (ToScene sceneMainMenu) UiOpenOptions]
  , [Envelope sceneRoot (ToScene sceneOptions) UiCloseTop]
  , [Envelope sceneRoot (ToScene sceneMainMenu) UiStartGame]
  , [Envelope sceneRoot (ToScene sceneGame) UiBackToMenu]
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
