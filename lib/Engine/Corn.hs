{-# LANGUAGE RecordWildCards #-}

module Engine.Corn
  ( RouteCodec(..)
  , RouteTable
  , routeTable
  , encodeBy
  , decodeBy
  , Nav(..)
  , Cmd(..)
  , RouteEvent(..)
  , Frame(..)
  , Scene
  , scene
  , sceneWith
  , SceneHooks(..)
  , noHooks
  , Plugin
  , plugin
  , pluginWith
  , PluginHooks(..)
  , noPluginHooks
  , Game
  , game
  , gameWith
  , withPlugin
  , withPlugins
  , Runtime
  , start
  , step
  , current
  , currentPath
  , model
  , canGoBack
  , canGoForward
  , isRunning
  ) where

import Prelude

import Engine.Data.FRP (DTime)
import qualified Engine.Data.Scene as Scene

-- | Route values can be encoded/decoded for host integration (URL, save-state, etc).
class RouteCodec route where
  encodeRoute :: route -> String
  decodeRoute :: String -> Maybe route

newtype RouteTable route = RouteTable [(route, String)]

routeTable :: [(route, String)] -> RouteTable route
routeTable = RouteTable

encodeBy :: (Eq route, Show route) => RouteTable route -> route -> String
encodeBy (RouteTable table) routeId =
  case lookup routeId table of
    Just encoded -> encoded
    Nothing -> error ("encodeBy: missing route mapping for " <> show routeId)

decodeBy :: RouteTable route -> String -> Maybe route
decodeBy (RouteTable table) pathText =
  case [routeId | (routeId, routePath) <- table, routePath == pathText] of
    routeId : _ -> Just routeId
    [] -> Nothing

data Nav route
  = Push route
  | Replace route
  | Back
  | Forward
  deriving (Eq, Show)

data Cmd route msg
  = Emit msg
  | Navigate (Nav route)
  | Quit
  deriving (Eq, Show)

data RouteEvent
  = Entered
  | Exited
  | BecameTop
  | LeftTop
  deriving (Eq, Show)

data Frame route = Frame
  { frameDt :: !DTime
  , frameRoute :: !route
  , framePath :: ![route]
  , frameEvents :: ![RouteEvent]
  , frameCanGoBack :: !Bool
  , frameCanGoForward :: !Bool
  } deriving (Eq, Show)

data Scene model route msg = Scene
  { sceneEnter :: route -> model -> (model, [Cmd route msg])
  , sceneStep :: Frame route -> [msg] -> model -> (model, [Cmd route msg])
  , sceneExit :: route -> model -> (model, [Cmd route msg])
  }

data SceneHooks model route msg = SceneHooks
  { onEnter :: route -> model -> (model, [Cmd route msg])
  , onExit :: route -> model -> (model, [Cmd route msg])
  }

noHooks :: SceneHooks model route msg
noHooks =
  SceneHooks
    { onEnter = \_ mdl -> (mdl, [])
    , onExit = \_ mdl -> (mdl, [])
    }

scene ::
  (Frame route -> [msg] -> model -> (model, [Cmd route msg])) ->
  Scene model route msg
scene = sceneWith noHooks

sceneWith ::
  SceneHooks model route msg ->
  (Frame route -> [msg] -> model -> (model, [Cmd route msg])) ->
  Scene model route msg
sceneWith hooks stepFn =
  Scene
    { sceneEnter = onEnter hooks
    , sceneStep = stepFn
    , sceneExit = onExit hooks
    }

data Plugin model route msg = Plugin
  { pluginEnter :: route -> model -> (model, [Cmd route msg])
  , pluginStep :: Frame route -> [msg] -> model -> (model, [Cmd route msg])
  , pluginExit :: route -> model -> (model, [Cmd route msg])
  }

data PluginHooks model route msg = PluginHooks
  { onPluginEnter :: route -> model -> (model, [Cmd route msg])
  , onPluginExit :: route -> model -> (model, [Cmd route msg])
  }

noPluginHooks :: PluginHooks model route msg
noPluginHooks =
  PluginHooks
    { onPluginEnter = \_ mdl -> (mdl, [])
    , onPluginExit = \_ mdl -> (mdl, [])
    }

plugin ::
  (Frame route -> [msg] -> model -> (model, [Cmd route msg])) ->
  Plugin model route msg
plugin = pluginWith noPluginHooks

pluginWith ::
  PluginHooks model route msg ->
  (Frame route -> [msg] -> model -> (model, [Cmd route msg])) ->
  Plugin model route msg
pluginWith hooks stepFn =
  Plugin
    { pluginEnter = onPluginEnter hooks
    , pluginStep = stepFn
    , pluginExit = onPluginExit hooks
    }

data Game route model msg = Game
  { gameInitialRoute :: !route
  , gameInitialModel :: !model
  , gameSceneFor :: route -> Scene model route msg
  , gamePlugins :: [Plugin model route msg]
  }

game ::
  route ->
  model ->
  (route -> Scene model route msg) ->
  Game route model msg
game initialRoute initialModel sceneFor =
  gameWith [] initialRoute initialModel sceneFor

gameWith ::
  [Plugin model route msg] ->
  route ->
  model ->
  (route -> Scene model route msg) ->
  Game route model msg
gameWith plugins initialRoute initialModel sceneFor =
  Game
    { gameInitialRoute = initialRoute
    , gameInitialModel = initialModel
    , gameSceneFor = sceneFor
    , gamePlugins = plugins
    }

withPlugin ::
  Plugin model route msg ->
  Game route model msg ->
  Game route model msg
withPlugin p gameDef =
  gameDef
    { gamePlugins = gamePlugins gameDef <> [p]
    }

withPlugins ::
  [Plugin model route msg] ->
  Game route model msg ->
  Game route model msg
withPlugins ps gameDef =
  gameDef
    { gamePlugins = gamePlugins gameDef <> ps
    }

data Runtime route model msg = Runtime
  { rtHistory :: !(Scene.History route)
  , rtModel :: !model
  , rtPrevPath :: ![route]
  , rtRunning :: !Bool
  , rtGame :: !(Game route model msg)
  }

start :: Game route model msg -> Runtime route model msg
start g =
  Runtime
    { rtHistory = Scene.historyAt (Scene.path [gameInitialRoute g])
    , rtModel = gameInitialModel g
    , rtPrevPath = []
    , rtRunning = True
    , rtGame = g
    }

current :: Runtime route model msg -> route
current runtime =
  case reverse (currentPath runtime) of
    top : _ -> top
    [] -> gameInitialRoute (rtGame runtime)

currentPath :: Runtime route model msg -> [route]
currentPath = Scene.locationSegments . Scene.current . rtHistory

model :: Runtime route model msg -> model
model = rtModel

canGoBack :: Runtime route model msg -> Bool
canGoBack = Scene.canGoBack . rtHistory

canGoForward :: Runtime route model msg -> Bool
canGoForward = Scene.canGoForward . rtHistory

isRunning :: Runtime route model msg -> Bool
isRunning = rtRunning

step ::
  Eq route =>
  DTime ->
  [msg] ->
  Runtime route model msg ->
  (Runtime route model msg, [msg])
step dt inbox runtime0 =
  if not (rtRunning runtime0)
    then (runtime0, [])
    else
      let pathNow = currentPath runtime0
          topNow = lastMaybe pathNow
          topPrev = lastMaybe (rtPrevPath runtime0)
          entered = [routeId | routeId <- pathNow, routeId `notElem` rtPrevPath runtime0]
          exited = [routeId | routeId <- rtPrevPath runtime0, routeId `notElem` pathNow]
          exitedInOrder = reverse [routeId | routeId <- rtPrevPath runtime0, routeId `elem` exited]
          sceneFor = gameSceneFor (rtGame runtime0)
          plugins = gamePlugins (rtGame runtime0)
          (modelAfterExit, cmdExit) =
            foldl'
              (\(mdl, cmds) routeId ->
                let sceneDef = sceneFor routeId
                    (mdl1, cmds1) = sceneExit sceneDef routeId mdl
                    (mdl2, cmds2) = runPluginExit plugins routeId mdl1
                in (mdl2, cmds <> cmds1 <> cmds2)
              )
              (rtModel runtime0, [])
              exitedInOrder
          (modelAfterEnter, cmdEnter) =
            foldl'
              (\(mdl, cmds) routeId ->
                if routeId `elem` entered
                  then
                    let sceneDef = sceneFor routeId
                        (mdl1, cmds1) = sceneEnter sceneDef routeId mdl
                        (mdl2, cmds2) = runPluginEnter plugins routeId mdl1
                    in (mdl2, cmds <> cmds1 <> cmds2)
                  else (mdl, cmds)
              )
              (modelAfterExit, [])
              pathNow
          (modelAfterFrame, cmdFrame) =
            foldl'
              (\(mdl, cmds) routeId ->
                let frame =
                      Frame
                        { frameDt = dt
                        , frameRoute = routeId
                        , framePath = pathNow
                        , frameEvents = routeEventsFor routeId entered exited topPrev topNow
                        , frameCanGoBack = Scene.canGoBack (rtHistory runtime0)
                        , frameCanGoForward = Scene.canGoForward (rtHistory runtime0)
                        }
                    sceneDef = sceneFor routeId
                    (mdl1, cmds1) = sceneStep sceneDef frame inbox mdl
                    (mdl2, cmds2) = runPluginStep plugins frame inbox mdl1
                in (mdl2, cmds <> cmds1 <> cmds2)
              )
              (modelAfterEnter, [])
              pathNow
          (history1, running1, outMsgs) =
            applyCommands (rtHistory runtime0) True (cmdExit <> cmdEnter <> cmdFrame)
          runtime1 =
            runtime0
              { rtHistory = history1
              , rtModel = modelAfterFrame
              , rtPrevPath = pathNow
              , rtRunning = running1
              }
      in (runtime1, outMsgs)

applyCommands ::
  Eq route =>
  Scene.History route ->
  Bool ->
  [Cmd route msg] ->
  (Scene.History route, Bool, [msg])
applyCommands history0 running0 cmds =
  foldl' stepOne (history0, running0, []) cmds
  where
    stepOne state@(historyNow, runningNow, outMsgs) command
      | not runningNow = state
      | otherwise =
          case command of
            Emit msg ->
              (historyNow, runningNow, outMsgs <> [msg])
            Navigate nav ->
              (applyNav nav historyNow, runningNow, outMsgs)
            Quit ->
              (historyNow, False, outMsgs)

runPluginEnter ::
  [Plugin model route msg] ->
  route ->
  model ->
  (model, [Cmd route msg])
runPluginEnter plugins routeId mdl0 =
  foldl'
    (\(mdl, cmds) one ->
      let (mdl1, cmds1) = pluginEnter one routeId mdl
      in (mdl1, cmds <> cmds1)
    )
    (mdl0, [])
    plugins

runPluginStep ::
  [Plugin model route msg] ->
  Frame route ->
  [msg] ->
  model ->
  (model, [Cmd route msg])
runPluginStep plugins frame inbox mdl0 =
  foldl'
    (\(mdl, cmds) one ->
      let (mdl1, cmds1) = pluginStep one frame inbox mdl
      in (mdl1, cmds <> cmds1)
    )
    (mdl0, [])
    plugins

runPluginExit ::
  [Plugin model route msg] ->
  route ->
  model ->
  (model, [Cmd route msg])
runPluginExit plugins routeId mdl0 =
  foldl'
    (\(mdl, cmds) one ->
      let (mdl1, cmds1) = pluginExit one routeId mdl
      in (mdl1, cmds <> cmds1)
    )
    (mdl0, [])
    plugins

applyNav :: Eq route => Nav route -> Scene.History route -> Scene.History route
applyNav nav historyNow =
  case nav of
    Push routeId ->
      let pathNow = Scene.locationSegments (Scene.current historyNow)
      in Scene.gotoAt Scene.Push (Scene.path (pathNow <> [routeId])) historyNow
    Replace routeId ->
      let pathNow = Scene.locationSegments (Scene.current historyNow)
          prefix = if null pathNow then [] else init pathNow
      in Scene.gotoAt Scene.Replace (Scene.path (prefix <> [routeId])) historyNow
    Back ->
      Scene.back historyNow
    Forward ->
      Scene.forward historyNow

routeEventsFor ::
  Eq route =>
  route ->
  [route] ->
  [route] ->
  Maybe route ->
  Maybe route ->
  [RouteEvent]
routeEventsFor routeId entered exited topPrev topNow =
  enterEvt <> exitEvt <> topEvt
  where
    enterEvt =
      if routeId `elem` entered
        then [Entered]
        else []
    exitEvt =
      if routeId `elem` exited
        then [Exited]
        else []
    topEvt =
      becameTop <> leftTop
    becameTop =
      if topNow == Just routeId && topPrev /= Just routeId
        then [BecameTop]
        else []
    leftTop =
      if topPrev == Just routeId && topNow /= Just routeId
        then [LeftTop]
        else []

lastMaybe :: [a] -> Maybe a
lastMaybe xs =
  case reverse xs of
    [] -> Nothing
    y : _ -> Just y
