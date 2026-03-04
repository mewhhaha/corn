{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

module SceneProps
  ( scene_push_pop_roundtrip
  , scene_nested_path_focus
  , scene_replace_primary_stack
  , scene_history_push_pop_segments
  , scene_history_back_forward
  , scene_history_back_clears_forward_on_new_nav
  , scene_history_pathwith_params
  , scene_route_simple_dsl
  , scene_route_specificity_prefers_literal
  , scene_route_simple_typed_goto
  , scene_route_simple_back_forward
  , scene_route_simple_search_keys_strict
  , scene_route_simple_rejects_duplicate_leaves
  , scene_route_runtime_step_events
  , scene_route_runtime_rejects_duplicate_patterns
  , scene_route_runtime_rejects_duplicate_leaves
  , scene_corn_simple_navigation_loop
  , scene_corn_deferred_navigation_snapshot
  , scene_corn_quit_is_terminal
  , scene_corn_plugin_step_runs
  , scene_corn_layer_from_inbox_helper
  ) where

import Prelude

import qualified Data.Map.Strict as Map
import qualified Engine.Corn as Corn
import qualified Engine.Data.ECS as E
import qualified Engine.Data.Program as S
import qualified Engine.Data.Router as Route
import qualified Engine.Data.Scene as Scene
import GHC.Generics (Generic)

data Sid
  = Root
  | MainMenu
  | Options
  | Game
  | Credits
  deriving (Eq, Ord, Show)

newtype SimpleSearch = SimpleSearch
  { mode :: [String]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Route.SearchCodec)

newtype SimpleTag = SimpleTag String
  deriving stock (Eq, Show, Generic)

data SimpleC
  = CSimpleTag SimpleTag
  deriving stock (Generic)

instance E.ComponentId SimpleC

data RuntimeMsg = RuntimeMsg
  { rmRoute :: String
  , rmEvents :: [Route.RouteEvent]
  } deriving (Eq, Show)

segmentsOf :: Scene.History Sid -> [Sid]
segmentsOf = Scene.locationSegments . Scene.current

scene_push_pop_roundtrip :: Bool
scene_push_pop_roundtrip =
  let s0 = Scene.singletonStack MainMenu
      s1 = Scene.push Options s0
  in case Scene.pop s1 of
      Just (popped, rest) -> popped == Options && rest == s0
      Nothing -> False

scene_nested_path_focus :: Bool
scene_nested_path_focus =
  let h0 = Scene.historyFrom [MainMenu]
      h1 = Scene.gotoSegment Scene.Push Options h0
      h2 = Scene.back h1
  in segmentsOf h1 == [Options]
      && segmentsOf h2 == [MainMenu]

scene_replace_primary_stack :: Bool
scene_replace_primary_stack =
  let h0 = Scene.historyFrom [MainMenu]
      h1 = Scene.gotoSegment Scene.Replace Game h0
  in segmentsOf h1 == [Game]
      && not (Scene.canGoBack h1)

scene_history_push_pop_segments :: Bool
scene_history_push_pop_segments =
  let h0 = Scene.historyFrom [MainMenu]
      h1 = Scene.gotoSegment Scene.Push Options h0
      h2 = Scene.back h1
  in segmentsOf h1 == [Options]
      && segmentsOf h2 == [MainMenu]
      && not (Scene.canGoBack h2)

scene_history_back_forward :: Bool
scene_history_back_forward =
  let h0 = Scene.historyFrom [Root]
      h1 = Scene.gotoSegment Scene.Push MainMenu h0
      h2 = Scene.gotoSegment Scene.Push Game h1
      h3 = Scene.back h2
      h4 = Scene.forward h3
  in segmentsOf h3 == [MainMenu]
      && segmentsOf h4 == [Game]
      && Scene.canGoBack h4
      && not (Scene.canGoForward h4)

scene_history_back_clears_forward_on_new_nav :: Bool
scene_history_back_clears_forward_on_new_nav =
  let h0 = Scene.historyFrom [Root]
      h1 = Scene.gotoSegment Scene.Push MainMenu h0
      h2 = Scene.gotoSegment Scene.Push Game h1
      h3 = Scene.back h2
      h4 = Scene.gotoSegment Scene.Replace Credits h3
  in segmentsOf h4 == [Credits]
      && not (Scene.canGoForward h4)

scene_history_pathwith_params :: Bool
scene_history_pathwith_params =
  let loc =
        Scene.withUrlParam "id" "enemy-42" $
          Scene.withSearchParam "tab" ["stats", "loot"] $
            Scene.path [Game]
      h0 = Scene.historyAt loc
      h1 = Scene.gotoSegment Scene.Push MainMenu h0
      cur0 = Scene.current h0
      cur1 = Scene.current h1
  in Scene.locationSegments cur0 == [Game]
      && Scene.locationUrlParams cur0 == Map.fromList [("id", "enemy-42")]
      && Scene.locationSearchParams cur0 == Map.fromList [("tab", ["stats", "loot"])]
      && Scene.locationSegments cur1 == [MainMenu]
      && Scene.locationUrlParams cur1 == Map.empty
      && Scene.locationSearchParams cur1 == Map.empty

mkSimpleScene :: String -> Scene.SceneRuntime SimpleC ()
mkSimpleScene tag =
  let (_, w1) = E.spawn (SimpleTag tag) (E.emptyWorld :: E.World SimpleC)
      g1 = S.graph (pure ())
  in Scene.mkScene w1 g1

sceneTag :: Scene.SceneRuntime SimpleC () -> Maybe String
sceneTag rt =
  let q = (E.comp :: E.Query SimpleC SimpleTag)
  in case E.runq q (Scene.sceneRuntimeWorld rt) of
    (_, SimpleTag tag) : _ -> Just tag
    [] -> Nothing

activeSceneAt ::
  String ->
  Scene.History String ->
  Route.SceneMap c msg ->
  Maybe (Scene.SceneRuntime c msg)
activeSceneAt sid h scenes =
  case [rt | (sid', rt) <- Route.active h scenes, sid' == sid] of
    rt : _ -> Just rt
    [] -> Nothing

simpleRoutes :: Route.Routes SimpleC () '[ "/hello/world/{:id}" ]
simpleRoutes =
  Route.route @"/hello/world/{:id}"
    (\params (SimpleSearch tabs) ->
      let ident = Route.param @"id" params
      in mkSimpleScene (ident <> ":" <> show tabs)
    )
    (Just (SimpleSearch ["stats"]))
    Route.:> Route.EmptyRoutes

scene_route_simple_dsl :: Bool
scene_route_simple_dsl =
  case Route.createRouter simpleRoutes of
    Left _ -> False
    Right router ->
      let h0 = Scene.history @'["/start"]
          (h1, scenes1) = Route.gotoPath Scene.Push "/hello/world/42" router h0
          loc = Scene.current h1
          enteredDefault = sceneTag =<< activeSceneAt "/hello/world" h1 scenes1
          locOverride =
            loc
              { Scene.locationSearchParams = Map.fromList [("mode", ["advanced"])]
              }
          scenesOverride =
            Route.sync router (Scene.historyAt locOverride)
          enteredOverride =
            sceneTag =<< activeSceneAt "/hello/world" (Scene.historyAt locOverride) scenesOverride
      in Scene.locationSegments loc == ["/hello/world"]
          && Scene.locationUrlParams loc == Map.fromList [("id", "42")]
          && enteredDefault == Just "42:[\"stats\"]"
          && enteredOverride == Just "42:[\"advanced\"]"

specificityRoutes :: Route.Routes SimpleC () '[ "/a/b/{:id}", "/a/b/c" ]
specificityRoutes =
  Route.route @"/a/b/{:id}" (\_ () -> mkSimpleScene "dyn") (Just ())
    Route.:> Route.route @"/a/b/c" (\_ () -> mkSimpleScene "lit") (Just ())
    Route.:> Route.EmptyRoutes

scene_route_specificity_prefers_literal :: Bool
scene_route_specificity_prefers_literal =
  case Route.createRouter specificityRoutes of
    Left _ -> False
    Right router ->
      let h0 = Scene.history @'["/start"]
          (h1, _) = Route.gotoPath Scene.Push "/a/b/c" router h0
          (h2, _) = Route.gotoPath Scene.Push "/a/b/e" router h1
          loc1 = Scene.current h1
          loc2 = Scene.current h2
      in Scene.locationUrlParams loc1 == Map.empty
          && Scene.locationUrlParams loc2 == Map.fromList [("id", "e")]

typedGotoRoutes :: Route.Routes SimpleC () '[ "/main-menu", "/main-menu/options" ]
typedGotoRoutes =
  Route.route @"/main-menu" (\_ () -> mkSimpleScene "menu") (Just ())
    Route.:> Route.route @"/main-menu/options" (\_ () -> mkSimpleScene "options") (Just ())
    Route.:> Route.EmptyRoutes

scene_route_simple_typed_goto :: Bool
scene_route_simple_typed_goto =
  case Route.createRouter typedGotoRoutes of
    Left _ -> False
    Right router ->
      let h0 = Scene.history @'["/main-menu"]
          (h1, _) = Route.goto @"/main-menu/options" Scene.Push router h0
      in Scene.locationSegments (Scene.current h1) == ["/main-menu", "/main-menu/options"]

scene_route_simple_back_forward :: Bool
scene_route_simple_back_forward =
  case Route.createRouter typedGotoRoutes of
    Left _ -> False
    Right router ->
      let h0 = Scene.history @'["/main-menu"]
          (h1, _) = Route.goto @"/main-menu/options" Scene.Push router h0
          (h2, scenes2) = Route.back router h1
          (h3, scenes3) = Route.forward router h2
      in Scene.locationSegments (Scene.current h2) == ["/main-menu"]
          && hasScene "/main-menu" h2 scenes2
          && not (hasScene "/main-menu/options" h2 scenes2)
          && Scene.locationSegments (Scene.current h3) == ["/main-menu", "/main-menu/options"]
          && hasScene "/main-menu/options" h3 scenes3

scene_route_simple_search_keys_strict :: Bool
scene_route_simple_search_keys_strict =
  case Route.createRouter simpleRoutes of
    Left _ -> False
    Right router ->
      let h0 = Scene.history @'["/start"]
          (h1, _) = Route.gotoPath Scene.Push "/hello/world/42" router h0
          badLoc =
            (Scene.current h1)
              { Scene.locationSearchParams = Map.fromList [("wrong", ["x"])]
              }
          badScenes = Route.sync router (Scene.historyAt badLoc)
      in case activeSceneAt "/hello/world" (Scene.historyAt badLoc) badScenes of
          Nothing -> True
          Just _ -> False

duplicateSimpleRoutes :: Route.Routes SimpleC () '[ "/players/{:id}", "/players/{:slug}" ]
duplicateSimpleRoutes =
  Route.route @"/players/{:id}" (\_ () -> mkSimpleScene "id") (Just ())
    Route.:> Route.route @"/players/{:slug}" (\_ () -> mkSimpleScene "slug") (Just ())
    Route.:> Route.EmptyRoutes

scene_route_simple_rejects_duplicate_leaves :: Bool
scene_route_simple_rejects_duplicate_leaves =
  case Route.createRouter duplicateSimpleRoutes of
    Left _ -> True
    Right _ -> False

hasScene :: String -> Scene.History String -> Route.SceneMap c msg -> Bool
hasScene sid h scenes =
  case activeSceneAt sid h scenes of
    Just _ -> True
    Nothing -> False

runtimeRoutes :: Route.StepRoutes Int RuntimeMsg '[ "/main-menu", "/main-menu/options" ]
runtimeRoutes =
  Route.stepRoute @"/main-menu"
    (\_ () -> 0)
    (\ctx _ _ n ->
      ( n + 1
      , [RuntimeMsg "/main-menu" (Route.stepEvents ctx)]
      )
    )
    (Just ())
    Route.:>>
      Route.stepRoute @"/main-menu/options"
        (\_ () -> 0)
        (\ctx _ _ n ->
          ( n + 1
          , [RuntimeMsg "/main-menu/options" (Route.stepEvents ctx)]
          )
        )
        (Just ())
      Route.:>>
      Route.EmptyStepRoutes

scene_route_runtime_step_events :: Bool
scene_route_runtime_step_events =
  case Route.create runtimeRoutes "/main-menu" of
    Left _ -> False
    Right rt0 ->
      let (rt1, out1) = Route.step 0.016 [] rt0
          rt2 = Route.navigate (Route.Goto Scene.Push "/main-menu/options") rt1
          (rt3, out2) = Route.step 0.016 [] rt2
          rt4 = Route.navigate Route.Back rt3
          (rt5, out3) = Route.step 0.016 [] rt4
      in out1 == [RuntimeMsg "/main-menu" [Route.Entered, Route.BecameTop]]
          && out2 ==
               [ RuntimeMsg "/main-menu" [Route.LeftTop]
               , RuntimeMsg "/main-menu/options" [Route.Entered, Route.BecameTop]
               ]
          && out3 ==
               [ RuntimeMsg "/main-menu/options" [Route.Exited, Route.LeftTop]
               , RuntimeMsg "/main-menu" [Route.BecameTop]
               ]
          && Scene.locationSegments (Route.current rt5) == ["/main-menu"]
          && not (Route.canGoBack rt5)
          && Route.canGoForward rt5

duplicatePatternRoutes :: Route.StepRoutes Int () '[ "/dup", "/dup" ]
duplicatePatternRoutes =
  Route.stepRoute @"/dup" (\_ () -> 0) (\_ _ _ n -> (n + 1, [])) (Just ())
    Route.:>>
      Route.stepRoute @"/dup" (\_ () -> 0) (\_ _ _ n -> (n + 1, [])) (Just ())
      Route.:>>
      Route.EmptyStepRoutes

scene_route_runtime_rejects_duplicate_patterns :: Bool
scene_route_runtime_rejects_duplicate_patterns =
  case Route.create duplicatePatternRoutes "/dup" of
    Left _ -> True
    Right _ -> False

duplicateLeafRoutes :: Route.StepRoutes Int () '[ "/players/{:id}", "/players/{:slug}" ]
duplicateLeafRoutes =
  Route.stepRoute @"/players/{:id}" (\_ () -> 0) (\_ _ _ n -> (n + 1, [])) (Just ())
    Route.:>>
      Route.stepRoute @"/players/{:slug}" (\_ () -> 0) (\_ _ _ n -> (n + 1, [])) (Just ())
      Route.:>>
      Route.EmptyStepRoutes

scene_route_runtime_rejects_duplicate_leaves :: Bool
scene_route_runtime_rejects_duplicate_leaves =
  case Route.create duplicateLeafRoutes "/players/1" of
    Left _ -> True
    Right _ -> False

data CornRoute
  = CornMenu
  | CornOptions
  | CornGame
  deriving (Eq, Show)

cornRouteTable :: Corn.RouteTable CornRoute
cornRouteTable =
  Corn.routeTable
    [ (CornMenu, "/main-menu")
    , (CornOptions, "/main-menu/options")
    , (CornGame, "/game")
    ]

instance Corn.RouteCodec CornRoute where
  encodeRoute = Corn.encodeBy cornRouteTable
  decodeRoute = Corn.decodeBy cornRouteTable

data CornMsg
  = CornOpen
  | CornClose
  | CornStart
  | CornBack
  deriving (Eq, Show)

data CornModel = CornModel
  { cornMenuTicks :: !Int
  , cornOptionsTicks :: !Int
  , cornGameTicks :: !Int
  } deriving (Eq, Show)

cornSceneFor :: CornRoute -> Corn.Layer CornModel CornRoute CornMsg
cornSceneFor routeId =
  case routeId of
    CornMenu ->
      Corn.layer $ \_ inbox model0 ->
        let model1 = model0 {cornMenuTicks = cornMenuTicks model0 + 1}
            cmds =
              [Corn.Navigate (Corn.Push CornOptions) | CornOpen `elem` inbox]
                <> [Corn.Navigate (Corn.Push CornGame) | CornStart `elem` inbox]
        in (model1, cmds)
    CornOptions ->
      Corn.layer $ \_ inbox model0 ->
        let model1 = model0 {cornOptionsTicks = cornOptionsTicks model0 + 1}
            cmds = [Corn.Navigate Corn.Back | CornClose `elem` inbox]
        in (model1, cmds)
    CornGame ->
      Corn.layer $ \_ inbox model0 ->
        let model1 = model0 {cornGameTicks = cornGameTicks model0 + 1}
            cmds = [Corn.Navigate Corn.Back | CornBack `elem` inbox]
        in (model1, cmds)

scene_corn_simple_navigation_loop :: Bool
scene_corn_simple_navigation_loop =
  let gameDef =
        Corn.game
          CornMenu
          (CornModel 0 0 0)
          cornSceneFor
      rt0 = Corn.start gameDef
      (rt1, out1) = Corn.step 0.016 [CornOpen] rt0
      (rt2, out2) = Corn.step 0.016 [CornClose] rt1
      (rt3, out3) = Corn.step 0.016 [CornStart] rt2
      (rt4, out4) = Corn.step 0.016 [CornBack] rt3
      model4 = Corn.model rt4
  in out1 == []
      && out2 == []
      && out3 == []
      && out4 == []
      && map Corn.encodeRoute (Corn.currentPath rt1) == ["/main-menu", "/main-menu/options"]
      && map Corn.encodeRoute (Corn.currentPath rt2) == ["/main-menu"]
      && map Corn.encodeRoute (Corn.currentPath rt3) == ["/main-menu", "/game"]
      && map Corn.encodeRoute (Corn.currentPath rt4) == ["/main-menu"]
      && cornMenuTicks model4 >= 4
      && cornOptionsTicks model4 >= 1
      && cornGameTicks model4 >= 1

data CornDeferRoute
  = DeferMenu
  | DeferOptions
  deriving (Eq, Show)

data CornDeferMsg = DeferOpen
  deriving (Eq, Show)

data CornDeferModel = CornDeferModel
  { deferMenuTicks :: !Int
  , deferOptionsTicks :: !Int
  } deriving (Eq, Show)

deferSceneFor :: CornDeferRoute -> Corn.Layer CornDeferModel CornDeferRoute CornDeferMsg
deferSceneFor routeId =
  case routeId of
    DeferMenu ->
      Corn.layer $ \_ inbox model0 ->
        let model1 = model0 {deferMenuTicks = deferMenuTicks model0 + 1}
            cmds = [Corn.Navigate (Corn.Push DeferOptions) | DeferOpen `elem` inbox]
        in (model1, cmds)
    DeferOptions ->
      Corn.layer $ \_ _ model0 ->
        let model1 = model0 {deferOptionsTicks = deferOptionsTicks model0 + 1}
        in (model1, [])

scene_corn_deferred_navigation_snapshot :: Bool
scene_corn_deferred_navigation_snapshot =
  let gameDef =
        Corn.game
          DeferMenu
          (CornDeferModel 0 0)
          deferSceneFor
      rt0 = Corn.start gameDef
      (rt1, out1) = Corn.step 0.016 [DeferOpen] rt0
      (rt2, out2) = Corn.step 0.016 [] rt1
      m1 = Corn.model rt1
      m2 = Corn.model rt2
  in out1 == []
      && out2 == []
      && Corn.currentPath rt1 == [DeferMenu, DeferOptions]
      && deferMenuTicks m1 == 1
      && deferOptionsTicks m1 == 0
      && deferMenuTicks m2 == 2
      && deferOptionsTicks m2 == 1

data CornQuitRoute
  = QuitMenu
  | QuitOther
  deriving (Eq, Show)

data CornQuitMsg = QuitOut
  deriving (Eq, Show)

data CornQuitModel = CornQuitModel
  { quitMenuTicks :: !Int
  , quitOtherTicks :: !Int
  } deriving (Eq, Show)

quitSceneFor :: CornQuitRoute -> Corn.Layer CornQuitModel CornQuitRoute CornQuitMsg
quitSceneFor routeId =
  case routeId of
    QuitMenu ->
      Corn.layer $ \_ _ model0 ->
        let model1 = model0 {quitMenuTicks = quitMenuTicks model0 + 1}
            cmds =
              [ Corn.Quit
              , Corn.Navigate (Corn.Push QuitOther)
              , Corn.Emit QuitOut
              ]
        in (model1, cmds)
    QuitOther ->
      Corn.layer $ \_ _ model0 ->
        let model1 = model0 {quitOtherTicks = quitOtherTicks model0 + 1}
        in (model1, [])

scene_corn_quit_is_terminal :: Bool
scene_corn_quit_is_terminal =
  let gameDef =
        Corn.game
          QuitMenu
          (CornQuitModel 0 0)
          quitSceneFor
      rt0 = Corn.start gameDef
      (rt1, out1) = Corn.step 0.016 [] rt0
      (rt2, out2) = Corn.step 0.016 [] rt1
      m1 = Corn.model rt1
      m2 = Corn.model rt2
  in out1 == []
      && out2 == []
      && not (Corn.isRunning rt1)
      && not (Corn.isRunning rt2)
      && Corn.currentPath rt1 == [QuitMenu]
      && Corn.currentPath rt2 == [QuitMenu]
      && quitMenuTicks m1 == 1
      && quitOtherTicks m1 == 0
      && m2 == m1

data CornPluginRoute = PluginMenu
  deriving (Eq, Show)

data CornPluginModel = CornPluginModel
  { pluginSceneTicks :: !Int
  , pluginStepTicks :: !Int
  } deriving (Eq, Show)

pluginScene :: Corn.Layer CornPluginModel CornPluginRoute ()
pluginScene =
  Corn.layer $ \_ _ model0 ->
    let model1 = model0 {pluginSceneTicks = pluginSceneTicks model0 + 1}
    in (model1, [])

pluginCounter :: Corn.Plugin CornPluginModel CornPluginRoute ()
pluginCounter =
  Corn.plugin $ \_ _ model0 ->
    let model1 = model0 {pluginStepTicks = pluginStepTicks model0 + 1}
    in (model1, [])

scene_corn_plugin_step_runs :: Bool
scene_corn_plugin_step_runs =
  let gameDef =
        Corn.withPlugin pluginCounter $
          Corn.game
            PluginMenu
            (CornPluginModel 0 0)
            (\PluginMenu -> pluginScene)
      rt0 = Corn.start gameDef
      (rt1, out1) = Corn.step 0.016 [] rt0
      (rt2, out2) = Corn.step 0.016 [] rt1
      m2 = Corn.model rt2
  in out1 == []
      && out2 == []
      && pluginSceneTicks m2 == 2
      && pluginStepTicks m2 == 2

data CornHelperRoute
  = HelperMenu
  | HelperGame
  deriving (Eq, Show)

data CornHelperMsg
  = HelperStart
  | HelperBack
  | HelperIgnore
  deriving (Eq, Show)

helperCommand :: CornHelperRoute -> CornHelperMsg -> Maybe (Corn.Cmd CornHelperRoute CornHelperMsg)
helperCommand routeId msg =
  case routeId of
    HelperMenu ->
      case msg of
        HelperStart -> Just (Corn.Navigate (Corn.Push HelperGame))
        HelperBack -> Nothing
        HelperIgnore -> Nothing
    HelperGame ->
      case msg of
        HelperBack -> Just (Corn.Navigate Corn.Back)
        HelperStart -> Nothing
        HelperIgnore -> Nothing

helperLayerFor :: CornHelperRoute -> Corn.Layer () CornHelperRoute CornHelperMsg
helperLayerFor = Corn.layerFromInboxAt helperCommand

scene_corn_layer_from_inbox_helper :: Bool
scene_corn_layer_from_inbox_helper =
  let gameDef = Corn.game HelperMenu () helperLayerFor
      rt0 = Corn.start gameDef
      (rt1, out1) = Corn.step 0.016 [HelperStart] rt0
      (rt2, out2) = Corn.step 0.016 [HelperBack] rt1
  in out1 == []
      && out2 == []
      && Corn.currentPath rt1 == [HelperMenu, HelperGame]
      && Corn.currentPath rt2 == [HelperMenu]
