{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Examples.ScenesNavigation
  ( runConcept
  ) where

import Prelude

import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.List (elemIndex, intercalate)
import qualified Engine.Data.ECS as E
import qualified Engine.Data.Program as S
import qualified Engine.Data.Router as Route
import qualified Engine.Data.Scene as Scene
import GHC.Generics (Generic)

type SceneId = String

type ScenePaths = '[ "/main-menu", "/main-menu/options", "/game" ]

type SceneState = Scene.SceneRuntime C Envelope

type SceneRuntime = Route.Runtime SceneState Envelope ScenePaths

hostSender :: SceneId
hostSender = "/host"

sceneMainMenu :: SceneId
sceneMainMenu = "/main-menu"

sceneOptions :: SceneId
sceneOptions = "/main-menu/options"

sceneGame :: SceneId
sceneGame = "/game"

data Target
  = ToScene SceneId
  | ToBelow
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

frameDt :: Double
frameDt = 0.016

tickProg :: Program ()
tickProg =
  void . S.await $
    S.each @TickRow $ \(TickRow (TickCount n)) ->
      S.set (TickCount (n + 1))

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
  S.send
    [ Envelope sceneOptions ToBelow UiStartGame
    , Envelope sceneOptions ToRouter NavBack
    ]

gameBackProg :: Program ()
gameBackProg = do
  _ <- S.await (eventIs UiBackToMenu)
  S.send [Envelope sceneGame ToRouter NavGotoMenu]

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

acceptsScene :: SceneId -> Envelope -> Bool
acceptsScene sid env =
  case envTarget env of
    ToScene dst -> sid == dst
    _ -> False

mkSceneRoute :: forall path. SceneId -> [Program ()] -> Route.StepRoute path SceneState Envelope ()
mkSceneRoute sid scenePrograms =
  Route.stepRoute
    (\_ () -> mkRuntime scenePrograms)
    (\_ dt inbox rt0 ->
      let sceneInbox = filter (acceptsScene sid) inbox
          (rt1, out1) = Scene.runScene dt sceneInbox rt0
      in (rt1, out1)
    )
    (Just ())

sceneRoutes :: Route.StepRoutes SceneState Envelope ScenePaths
sceneRoutes =
  mkSceneRoute @"/main-menu" sceneMainMenu [mainMenuOpenProg, mainMenuStartProg]
    Route.:>> mkSceneRoute @"/main-menu/options" sceneOptions [optionsCloseProg]
    Route.:>> mkSceneRoute @"/game" sceneGame [gameBackProg]
    Route.:>> Route.EmptyStepRoutes

sceneRuntime :: SceneRuntime
sceneRuntime =
  case Route.create sceneRoutes "/main-menu" of
    Left err -> error ("invalid scene router: " <> err)
    Right rt -> rt

data Host = Host
  { hostRuntime :: !SceneRuntime
  , hostMailbox :: ![Envelope]
  }

belowScenes :: [SceneId] -> SceneId -> [SceneId]
belowScenes active sender =
  case elemIndex sender active of
    Just senderIx -> take senderIx active
    Nothing -> []

expandRelativeTargets :: [SceneId] -> [Envelope] -> [Envelope]
expandRelativeTargets active outbox =
  concatMap expandOne outbox
  where
    expandOne env =
      case envTarget env of
        ToBelow ->
          [ env
              { envTarget = ToScene sid
              }
          | sid <- belowScenes active (envFrom env)
          ]
        _ -> [env]

isActiveScene :: SceneId -> SceneRuntime -> Bool
isActiveScene sid rt = sid `elem` Scene.locationSegments (Route.current rt)

applyRouterEvents :: SceneRuntime -> [Envelope] -> SceneRuntime
applyRouterEvents rt0 outbox =
  foldl'
    applyOne
    rt0
    [envMsg env | env <- outbox, envTarget env == ToRouter]
  where
    applyOne rt cmd =
      case cmd of
        NavOpenOptions ->
          if isActiveScene sceneOptions rt
            then rt
            else Route.navigate (Route.Goto Scene.Push "/main-menu/options") rt
        NavBack ->
          Route.navigate Route.Back rt
        NavGotoGame ->
          Route.navigate (Route.Goto Scene.Push "/game") rt
        NavGotoMenu ->
          Route.navigate (Route.Goto Scene.Push "/main-menu") rt
        _ ->
          rt

stepHost :: Double -> [Envelope] -> Host -> (Host, [Envelope])
stepHost dt external host0 =
  let activeBefore = Scene.locationSegments (Route.current (hostRuntime host0))
      inbound = expandRelativeTargets activeBefore (hostMailbox host0 <> external)
      (rt1, outbox0) = Route.step dt inbound (hostRuntime host0)
      activeAfter = Scene.locationSegments (Route.current rt1)
      outbox1 = expandRelativeTargets activeAfter outbox0
      rt2 = applyRouterEvents rt1 outbox1
      mailbox1 = filter ((/= ToRouter) . envTarget) outbox1
      host1 =
        host0
          { hostRuntime = rt2
          , hostMailbox = mailbox1
          }
  in (host1, outbox1)

initialHost :: Host
initialHost =
  Host
    { hostRuntime = sceneRuntime
    , hostMailbox = []
    }

fromHost :: SceneId -> Msg -> Envelope
fromHost dst = Envelope hostSender (ToScene dst)

renderPath :: SceneRuntime -> String
renderPath rt =
  unlines
    [ "path=" <> show segs
    , "path-render=" <> intercalate " > " segs
    , "canGoBack=" <> show (Route.canGoBack rt) <> ", canGoForward=" <> show (Route.canGoForward rt)
    ]
  where
    segs = Scene.locationSegments (Route.current rt)

scriptedInputs :: [[Envelope]]
scriptedInputs =
  [ [fromHost sceneMainMenu UiOpenOptions]
  , [fromHost sceneOptions UiCloseTop]
  , []
  , [fromHost sceneGame UiBackToMenu]
  , []
  ]

runConcept :: IO ()
runConcept = go 1 initialHost scriptedInputs
  where
    go :: Int -> Host -> [[Envelope]] -> IO ()
    go _ _ [] = pure ()
    go frameIx host0 (inputs : rest) = do
      let (host1, outbox) = stepHost frameDt inputs host0
          activeNow = Scene.locationSegments (Route.current (hostRuntime host1))
      putStrLn ("frame " <> show frameIx)
      putStrLn ("  inputs=" <> show (map envMsg inputs))
      putStrLn ("  outbox=" <> show (map envMsg outbox))
      putStrLn ("  active=" <> show activeNow)
      putStrLn (renderPath (hostRuntime host1))
      go (frameIx + 1) host1 rest
