{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

module SceneProps
  ( scenePushPopRoundtrip
  , sceneNestedPathFocus
  , sceneReplacePrimaryStack
  , sceneHistoryPushPopSegments
  , sceneHistoryBackForward
  , sceneHistoryBackClearsForwardOnNewNav
  , sceneHistoryPathwithParams
  , sceneRouteSimpleDsl
  , sceneRouteSpecificityPrefersLiteral
  , sceneRouteSimpleTypedGoto
  , sceneRouteSimpleBackForward
  , sceneRouteSimpleSearchKeysStrict
  , sceneRouteSimpleRejectsDuplicateLeaves
  , sceneRouteRuntimeStepEvents
  , sceneRouteRuntimeRejectsDuplicatePatterns
  , sceneRouteRuntimeRejectsDuplicateLeaves
  , sceneCornSimpleNavigationLoop
  , sceneCornDeferredNavigationSnapshot
  , sceneCornQuitIsTerminal
  , sceneCornPluginStepRuns
  , sceneCornLayerFromInboxHelper
  , sceneCornIntentGameCentralizesNavigation
  ) where

import Prelude

import qualified Data.Map.Strict as Map
import qualified Engine.Corn as Corn
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

data RuntimeMsg = RuntimeMsg
  { rmRoute :: String
  , rmEvents :: [Route.RouteEvent]
  } deriving (Eq, Show)

segmentsOf :: Scene.History Sid -> [Sid]
segmentsOf = Scene.locationSegments . Scene.current

scenePushPopRoundtrip :: Bool
scenePushPopRoundtrip =
  let s0 = Scene.singletonStack MainMenu
      s1 = Scene.push Options s0
  in case Scene.pop s1 of
      Just (popped, rest) -> popped == Options && rest == s0
      Nothing -> False

sceneNestedPathFocus :: Bool
sceneNestedPathFocus =
  let h0 = Scene.historyFrom [MainMenu]
      h1 = Scene.gotoSegment Scene.Push Options h0
      h2 = Scene.back h1
  in segmentsOf h1 == [Options]
      && segmentsOf h2 == [MainMenu]

sceneReplacePrimaryStack :: Bool
sceneReplacePrimaryStack =
  let h0 = Scene.historyFrom [MainMenu]
      h1 = Scene.gotoSegment Scene.Replace Game h0
  in segmentsOf h1 == [Game]
      && not (Scene.canGoBack h1)

sceneHistoryPushPopSegments :: Bool
sceneHistoryPushPopSegments =
  let h0 = Scene.historyFrom [MainMenu]
      h1 = Scene.gotoSegment Scene.Push Options h0
      h2 = Scene.back h1
  in segmentsOf h1 == [Options]
      && segmentsOf h2 == [MainMenu]
      && not (Scene.canGoBack h2)

sceneHistoryBackForward :: Bool
sceneHistoryBackForward =
  let h0 = Scene.historyFrom [Root]
      h1 = Scene.gotoSegment Scene.Push MainMenu h0
      h2 = Scene.gotoSegment Scene.Push Game h1
      h3 = Scene.back h2
      h4 = Scene.forward h3
  in segmentsOf h3 == [MainMenu]
      && segmentsOf h4 == [Game]
      && Scene.canGoBack h4
      && not (Scene.canGoForward h4)

sceneHistoryBackClearsForwardOnNewNav :: Bool
sceneHistoryBackClearsForwardOnNewNav =
  let h0 = Scene.historyFrom [Root]
      h1 = Scene.gotoSegment Scene.Push MainMenu h0
      h2 = Scene.gotoSegment Scene.Push Game h1
      h3 = Scene.back h2
      h4 = Scene.gotoSegment Scene.Replace Credits h3
  in segmentsOf h4 == [Credits]
      && not (Scene.canGoForward h4)

sceneHistoryPathwithParams :: Bool
sceneHistoryPathwithParams =
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

data RuntimeOut = RuntimeOut
  { outRoute :: !String
  , outId :: !(Maybe String)
  , outTabs :: ![String]
  } deriving (Eq, Show)

simpleRoute :: Route.Route "/hello/world/{:id}" Int RuntimeMsg RuntimeOut SimpleSearch
simpleRoute =
  Route.route
    (\params (SimpleSearch tabs) -> length tabs + length (Route.param @"id" params))
    (\ctx _ _ n ->
      let ident = Route.param @"id" (Route.ctxParams ctx)
          tabs = mode (Route.ctxSearch ctx)
      in (n + 1, [RuntimeOut "/hello/world" (Just ident) tabs], [])
    )
    (Just (SimpleSearch ["stats"]))

simpleRoutes :: Route.Routes Int RuntimeMsg RuntimeOut '[ "/hello/world/{:id}" ]
simpleRoutes =
  Route.leaf simpleRoute
    Route.:> Route.EmptyRoutes

sceneRouteSimpleDsl :: Bool
sceneRouteSimpleDsl =
  case Route.create simpleRoutes "/hello/world/42" of
    Left _ -> False
    Right rt0 ->
      let (rt1, out1, nav1) = Route.step 0.016 [] rt0
          loc = Route.current rt1
      in Scene.locationSegments loc == ["/hello/world"]
          && Scene.locationUrlParams loc == Map.fromList [("id", "42")]
          && out1 == [RuntimeOut "/hello/world" (Just "42") ["stats"]]
          && null nav1

specificityRoutes :: Route.Routes Int RuntimeMsg RuntimeOut '[ "/a/b/{:id}", "/a/b/c" ]
specificityRoutes =
  Route.leaf
    ( Route.route
        @"/a/b/{:id}"
        (\_ () -> 0)
        (\ctx _ _ n -> (n + 1, [RuntimeOut "/a/b" (Just (Route.param @"id" (Route.ctxParams ctx))) []], []))
        (Just ())
    )
    Route.:>
      Route.leaf
        ( Route.route
            @"/a/b/c"
            (\_ () -> 0)
            (\_ _ _ n -> (n + 1, [RuntimeOut "/a/b/c" Nothing []], []))
            (Just ())
        )
      Route.:>
      Route.EmptyRoutes

sceneRouteSpecificityPrefersLiteral :: Bool
sceneRouteSpecificityPrefersLiteral =
  case Route.create specificityRoutes "/a/b/c" of
    Left _ -> False
    Right rt0 ->
      let (rt1, out1, _) = Route.step 0.016 [] rt0
          rt2 = Route.navigate (Route.Push "/a/b/e") rt1
          (rt3, out2, _) = Route.step 0.016 [] rt2
          loc2 = Route.current rt3
      in out1 == [RuntimeOut "/a/b/c" Nothing []]
          && out2 ==
               [ RuntimeOut "/a/b/c" Nothing []
               , RuntimeOut "/a/b" (Just "e") []
               ]
          && Scene.locationUrlParams loc2 == Map.fromList [("id", "e")]

menuRoute :: Route.Route "/main-menu" Int RuntimeMsg RuntimeOut ()
menuRoute =
  Route.route
    (\_ () -> 0)
    (\_ _ _ n -> (n + 1, [RuntimeOut "/main-menu" Nothing []], []))
    (Just ())

optionsRoute :: Route.Route "/main-menu/options" Int RuntimeMsg RuntimeOut ()
optionsRoute =
  Route.route
    (\_ () -> 0)
    (\_ _ _ n -> (n + 1, [RuntimeOut "/main-menu/options" Nothing []], []))
    (Just ())

nestedRoutes :: Route.Routes Int RuntimeMsg RuntimeOut '[ "/main-menu", "/main-menu/options" ]
nestedRoutes =
  Route.node menuRoute
    ( Route.leaf optionsRoute
        Route.:> Route.EmptyRoutes
    )
    Route.:> Route.EmptyRoutes

flatRoutes :: Route.Routes Int RuntimeMsg RuntimeOut '[ "/main-menu", "/main-menu/options" ]
flatRoutes =
  Route.leaf menuRoute
    Route.:> Route.leaf optionsRoute
    Route.:> Route.EmptyRoutes

sceneRouteSimpleTypedGoto :: Bool
sceneRouteSimpleTypedGoto =
  case (Route.create nestedRoutes "/main-menu", Route.create flatRoutes "/main-menu/options") of
    (Right nestedRt0, Right flatRt0) ->
      let nestedRt1 = Route.navigate (Route.Push "/main-menu/options") nestedRt0
          nestedSegments = Scene.locationSegments (Route.current nestedRt1)
          flatSegments = Scene.locationSegments (Route.current flatRt0)
      in nestedSegments == ["/main-menu", "/main-menu/options"]
          && flatSegments == ["/main-menu/options"]
    _ -> False

sceneRouteSimpleBackForward :: Bool
sceneRouteSimpleBackForward =
  case Route.create nestedRoutes "/main-menu" of
    Left _ -> False
    Right rt0 ->
      let rt1 = Route.navigate (Route.Push "/main-menu/options") rt0
          rt2 = Route.navigate Route.Back rt1
          rt3 = Route.navigate Route.Forward rt2
      in Scene.locationSegments (Route.current rt2) == ["/main-menu"]
          && Scene.locationSegments (Route.current rt3) == ["/main-menu", "/main-menu/options"]
          && Route.canGoBack rt3
          && not (Route.canGoForward rt3)

sceneRouteSimpleSearchKeysStrict :: Bool
sceneRouteSimpleSearchKeysStrict =
  case Route.create simpleRoutes "/hello/world/42" of
    Left _ -> False
    Right rt0 ->
      let rt1 = Route.navigate (Route.ReplaceWith "/hello/world/9" (Map.fromList [("mode", ["advanced"])])) rt0
          (rt2, out2, _) = Route.step 0.016 [] rt1
          rt3 = Route.navigate (Route.Replace "/hello/world/7") rt2
          (_, out3, _) = Route.step 0.016 [] rt3
      in out2 == [RuntimeOut "/hello/world" (Just "9") ["advanced"]]
          && out3 == [RuntimeOut "/hello/world" (Just "7") ["stats"]]

duplicateSimpleRoutes :: Route.Routes Int RuntimeMsg RuntimeOut '[ "/players/{:id}", "/players/{:slug}" ]
duplicateSimpleRoutes =
  Route.leaf
    (Route.route @"/players/{:id}" (\_ () -> 0) (\_ _ _ n -> (n + 1, [], [])) (Just ()))
    Route.:>
      Route.leaf
        (Route.route @"/players/{:slug}" (\_ () -> 0) (\_ _ _ n -> (n + 1, [], [])) (Just ()))
      Route.:>
      Route.EmptyRoutes

sceneRouteSimpleRejectsDuplicateLeaves :: Bool
sceneRouteSimpleRejectsDuplicateLeaves =
  case Route.create duplicateSimpleRoutes "/players/1" of
    Left _ -> True
    Right _ -> False

runtimeRoutes :: Route.Routes Int RuntimeMsg RuntimeMsg '[ "/main-menu", "/main-menu/options" ]
runtimeRoutes =
  Route.node
    ( Route.route
        (\_ () -> 0)
        (\ctx _ _ n ->
          ( n + 1
          , [RuntimeMsg "/main-menu" (Route.ctxEvents ctx)]
          , []
          )
        )
        (Just ())
    )
    ( Route.leaf
        ( Route.route
            (\_ () -> 0)
            (\ctx _ _ n ->
              ( n + 1
              , [RuntimeMsg "/main-menu/options" (Route.ctxEvents ctx)]
              , []
              )
            )
            (Just ())
        )
        Route.:> Route.EmptyRoutes
    )
    Route.:> Route.EmptyRoutes

sceneRouteRuntimeStepEvents :: Bool
sceneRouteRuntimeStepEvents =
  case Route.create runtimeRoutes "/main-menu" of
    Left _ -> False
    Right rt0 ->
      let (rt1, out1, nav1) = Route.step 0.016 [] rt0
          rt2 = Route.navigate (Route.Push "/main-menu/options") rt1
          (rt3, out2, nav2) = Route.step 0.016 [] rt2
          rt4 = Route.navigate Route.Back rt3
          (rt5, out3, nav3) = Route.step 0.016 [] rt4
      in out1 == [RuntimeMsg "/main-menu" [Route.Entered, Route.BecameTop]]
          && null nav1
          && out2 ==
               [ RuntimeMsg "/main-menu" [Route.LeftTop]
               , RuntimeMsg "/main-menu/options" [Route.Entered, Route.BecameTop]
               ]
          && null nav2
          && out3 ==
               [ RuntimeMsg "/main-menu/options" [Route.Exited, Route.LeftTop]
               , RuntimeMsg "/main-menu" [Route.BecameTop]
               ]
          && null nav3
          && Scene.locationSegments (Route.current rt5) == ["/main-menu"]
          && not (Route.canGoBack rt5)
          && Route.canGoForward rt5

duplicatePatternRoutes :: Route.Routes Int () () '[ "/dup", "/dup" ]
duplicatePatternRoutes =
  Route.leaf (Route.route @"/dup" (\_ () -> 0) (\_ _ _ n -> (n + 1, [], [])) (Just ()))
    Route.:>
      Route.leaf (Route.route @"/dup" (\_ () -> 0) (\_ _ _ n -> (n + 1, [], [])) (Just ()))
      Route.:>
      Route.EmptyRoutes

sceneRouteRuntimeRejectsDuplicatePatterns :: Bool
sceneRouteRuntimeRejectsDuplicatePatterns =
  case Route.create duplicatePatternRoutes "/dup" of
    Left _ -> True
    Right _ -> False

duplicateLeafRoutes :: Route.Routes Int () () '[ "/players/{:id}", "/players/{:slug}" ]
duplicateLeafRoutes =
  Route.leaf (Route.route @"/players/{:id}" (\_ () -> 0) (\_ _ _ n -> (n + 1, [], [])) (Just ()))
    Route.:>
      Route.leaf (Route.route @"/players/{:slug}" (\_ () -> 0) (\_ _ _ n -> (n + 1, [], [])) (Just ()))
      Route.:>
      Route.EmptyRoutes

sceneRouteRuntimeRejectsDuplicateLeaves :: Bool
sceneRouteRuntimeRejectsDuplicateLeaves =
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

sceneCornSimpleNavigationLoop :: Bool
sceneCornSimpleNavigationLoop =
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
  in null out1
      && null out2
      && null out3
      && null out4
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

sceneCornDeferredNavigationSnapshot :: Bool
sceneCornDeferredNavigationSnapshot =
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
  in null out1
      && null out2
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

sceneCornQuitIsTerminal :: Bool
sceneCornQuitIsTerminal =
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
  in null out1
      && null out2
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

sceneCornPluginStepRuns :: Bool
sceneCornPluginStepRuns =
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
  in null out1
      && null out2
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

sceneCornLayerFromInboxHelper :: Bool
sceneCornLayerFromInboxHelper =
  let gameDef = Corn.game HelperMenu () helperLayerFor
      rt0 = Corn.start gameDef
      (rt1, out1) = Corn.step 0.016 [HelperStart] rt0
      (rt2, out2) = Corn.step 0.016 [HelperBack] rt1
  in null out1
      && null out2
      && Corn.currentPath rt1 == [HelperMenu, HelperGame]
      && Corn.currentPath rt2 == [HelperMenu]

data CornIntentRoute
  = IntentMenu
  | IntentGame
  deriving (Eq, Show)

data CornIntentMsg
  = IntentStart
  | IntentBack
  | IntentQuit
  deriving (Eq, Show)

data CornIntent
  = GoIntentGame
  | GoIntentBack
  | QuitIntent
  deriving (Eq, Show)

intentFromInbox :: CornIntentRoute -> CornIntentMsg -> Maybe CornIntent
intentFromInbox routeId msg =
  case routeId of
    IntentMenu ->
      case msg of
        IntentStart -> Just GoIntentGame
        IntentQuit -> Just QuitIntent
        IntentBack -> Nothing
    IntentGame ->
      case msg of
        IntentBack -> Just GoIntentBack
        IntentQuit -> Just QuitIntent
        IntentStart -> Nothing

intentLayerFor :: CornIntentRoute -> Corn.IntentLayer () CornIntentRoute CornIntentMsg CornIntent
intentLayerFor routeId =
  Corn.intentLayer $ \_ inbox model0 ->
    ( model0
    , [intent | msg <- inbox, Just intent <- [intentFromInbox routeId msg]]
    )

interpretIntent :: CornIntentRoute -> CornIntent -> Corn.CmdEffect CornIntentRoute CornIntentMsg
interpretIntent routeId intent =
  case (routeId, intent) of
    (IntentMenu, GoIntentGame) -> Corn.One (Corn.Navigate (Corn.Push IntentGame))
    (IntentGame, GoIntentBack) -> Corn.One (Corn.Navigate Corn.Back)
    (_, QuitIntent) -> Corn.One Corn.Quit
    _ -> Corn.Ignore

sceneCornIntentGameCentralizesNavigation :: Bool
sceneCornIntentGameCentralizesNavigation =
  let gameDef = Corn.intentGame IntentMenu () intentLayerFor interpretIntent
      rt0 = Corn.start gameDef
      (rt1, out1) = Corn.step 0.016 [IntentStart] rt0
      (rt2, out2) = Corn.step 0.016 [IntentBack] rt1
      (rt3, out3) = Corn.step 0.016 [IntentQuit] rt2
  in null out1
      && null out2
      && null out3
      && Corn.currentPath rt1 == [IntentMenu, IntentGame]
      && Corn.currentPath rt2 == [IntentMenu]
      && not (Corn.isRunning rt3)
