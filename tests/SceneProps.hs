{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module SceneProps
  ( scene_push_pop_roundtrip
  , scene_nested_path_focus
  , scene_replace_primary_stack
  , scene_history_push_pop_segments
  , scene_history_back_forward
  , scene_history_back_clears_forward_on_new_nav
  , scene_history_pathwith_params
  , scene_typed_router_route
  , scene_typed_router_rejects_extra_url_keys
  , scene_route_path_prefix_includes_layout_when_present
  , scene_route_path_prefix_skips_missing_layout
  , scene_route_path_rejects_extra_url_keys
  , scene_route_auto_codec_roundtrip
  , scene_route_scene_handler_receives_validated_params
  , scene_route_specificity_prefers_literal
  ) where

import Prelude

import qualified Data.Map.Strict as Map
import qualified Engine.Data.Route as Route
import qualified Engine.Data.Scene as Scene
import GHC.Generics (Generic)

data Sid
  = Root
  | MainMenu
  | Options
  | Game
  | Credits
  deriving (Eq, Ord, Show)

newtype PlayerSearch = PlayerSearch
  { tab :: [String]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Route.SearchCodec)

newtype PlayerUrl = PlayerUrl
  { playerId :: String
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Route.UrlCodec)

data AutoSearch = AutoSearch
  { autoTab :: [String]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Route.SearchCodec)

data AutoUrl = AutoUrl
  { autoPlayerId :: String
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Route.UrlCodec)

segmentsOf :: Scene.History Sid -> [Sid]
segmentsOf = Scene.locationSegments . Scene.current

segmentsOfString :: Scene.History String -> [String]
segmentsOfString = Scene.locationSegments . Scene.current

scene_push_pop_roundtrip :: Bool
scene_push_pop_roundtrip =
  let s0 = Scene.singletonStack MainMenu
      s1 = Scene.push Options s0
  in case Scene.pop s1 of
      Just (popped, rest) -> popped == Options && rest == s0
      Nothing -> False

scene_nested_path_focus :: Bool
scene_nested_path_focus =
  let h0 = Scene.history [MainMenu]
      h1 = Scene.goto Scene.Push Options h0
      h2 = Scene.back h1
  in segmentsOf h1 == [Options]
      && segmentsOf h2 == [MainMenu]

scene_replace_primary_stack :: Bool
scene_replace_primary_stack =
  let h0 = Scene.history [MainMenu]
      h1 = Scene.goto Scene.Replace Game h0
  in segmentsOf h1 == [Game]
      && not (Scene.canGoBack h1)

scene_history_push_pop_segments :: Bool
scene_history_push_pop_segments =
  let h0 = Scene.history [MainMenu]
      h1 = Scene.goto Scene.Push Options h0
      h2 = Scene.back h1
  in segmentsOf h1 == [Options]
      && segmentsOf h2 == [MainMenu]
      && not (Scene.canGoBack h2)

scene_history_back_forward :: Bool
scene_history_back_forward =
  let h0 = Scene.history [Root]
      h1 = Scene.goto Scene.Push MainMenu h0
      h2 = Scene.goto Scene.Push Game h1
      h3 = Scene.back h2
      h4 = Scene.forward h3
  in segmentsOf h3 == [MainMenu]
      && segmentsOf h4 == [Game]
      && Scene.canGoBack h4
      && not (Scene.canGoForward h4)

scene_history_back_clears_forward_on_new_nav :: Bool
scene_history_back_clears_forward_on_new_nav =
  let h0 = Scene.history [Root]
      h1 = Scene.goto Scene.Push MainMenu h0
      h2 = Scene.goto Scene.Push Game h1
      h3 = Scene.back h2
      h4 = Scene.goto Scene.Replace Credits h3
  in segmentsOf h4 == [Credits]
      && not (Scene.canGoForward h4)

scene_history_pathwith_params :: Bool
scene_history_pathwith_params =
  let loc =
        Scene.withUrlParam "id" "enemy-42" $
          Scene.withSearchParam "tab" ["stats", "loot"] $
            Scene.path [Game]
      h0 = Scene.historyAt loc
      h1 = Scene.goto Scene.Push MainMenu h0
      cur0 = Scene.current h0
      cur1 = Scene.current h1
  in Scene.locationSegments cur0 == [Game]
      && Scene.locationUrlParams cur0 == Map.fromList [("id", "enemy-42")]
      && Scene.locationSearchParams cur0 == Map.fromList [("tab", ["stats", "loot"])]
      && Scene.locationSegments cur1 == [MainMenu]
      && Scene.locationUrlParams cur1 == Map.empty
      && Scene.locationSearchParams cur1 == Map.empty

decodeSid :: String -> Maybe Sid
decodeSid sidPath =
  case sidPath of
    "/root" -> Just Root
    "/root/mainmenu" -> Just MainMenu
    "/root/options" -> Just Options
    "/root/game" -> Just Game
    _ -> Nothing

scene_typed_router_route :: Bool
scene_typed_router_route =
  let rootRouteResult :: Either String (Route.Route Sid () Route.UrlParams)
      rootRouteResult = Route.routeWith decodeSid "/root"
      gameRouteResult :: Either String (Route.Route Sid PlayerSearch PlayerUrl)
      gameRouteResult = Route.routeWith decodeSid "/root/game/{:playerId}"
  in case (rootRouteResult, gameRouteResult) of
      (Right rootRoute, Right gameRoute) ->
        let router =
              gameRoute Route.:> rootRoute Route.:> Route.RNil
            h0 = Scene.history [Root, MainMenu]
            h1 = Route.gotoRoute Scene.Push gameRoute (PlayerSearch ["stats"]) (PlayerUrl "42") router h0
            h2 = Scene.goto Scene.Replace Root h1
        in segmentsOf h1 == [Root, Game]
            && Route.currentRoute gameRoute router h1 == Just (PlayerSearch ["stats"], PlayerUrl "42")
            && Route.currentRoute gameRoute router h2 == Nothing
      _ -> False

scene_typed_router_rejects_extra_url_keys :: Bool
scene_typed_router_rejects_extra_url_keys =
  let rootRouteResult :: Either String (Route.Route Sid () Route.UrlParams)
      rootRouteResult = Route.routeWith decodeSid "/root"
      gameRouteResult :: Either String (Route.Route Sid PlayerSearch PlayerUrl)
      gameRouteResult = Route.routeWith decodeSid "/root/game/{:playerId}"
  in case (rootRouteResult, gameRouteResult) of
      (Right rootRoute, Right gameRoute) ->
        let router =
              gameRoute Route.:> rootRoute Route.:> Route.RNil
            locBad =
              Scene.Location
                { Scene.locationSegments = [Root, Game]
                , Scene.locationSearchParams = Map.fromList [("tab", ["stats"])]
                , Scene.locationUrlParams = Map.fromList [("playerId", "42"), ("other", "x")]
                }
        in Route.matchRoute gameRoute router locBad == Nothing
      _ -> False

scene_route_path_prefix_includes_layout_when_present :: Bool
scene_route_path_prefix_includes_layout_when_present =
  let layoutRouteResult :: Either String (Route.Route String () Route.UrlParams)
      layoutRouteResult = Route.route "/layout"
      layoutItemsRouteResult :: Either String (Route.Route String () Route.UrlParams)
      layoutItemsRouteResult = Route.route "/layout/items"
  in case (layoutRouteResult, layoutItemsRouteResult) of
      (Right layoutRoute, Right layoutItemsRoute) ->
        let router =
              layoutItemsRoute Route.:> layoutRoute Route.:> Route.RNil
            h0 = Scene.history ["/root"]
            h1 = Route.gotoRoute Scene.Push layoutItemsRoute () Map.empty router h0
        in segmentsOfString h1 == ["/layout", "/layout/items"]
            && Route.currentRoute layoutItemsRoute router h1 == Just ((), Map.empty)
      _ -> False

scene_route_path_prefix_skips_missing_layout :: Bool
scene_route_path_prefix_skips_missing_layout =
  let routeResult :: Either String (Route.Route String () Route.UrlParams)
      routeResult = Route.route "/layout/items"
  in case routeResult of
      Left _ -> False
      Right layoutItemsRoute ->
        let router = layoutItemsRoute Route.:> Route.RNil
            h0 = Scene.history ["/root"]
            h1 = Route.gotoRoute Scene.Push layoutItemsRoute () Map.empty router h0
        in segmentsOfString h1 == ["/layout/items"]
            && Route.currentRoute layoutItemsRoute router h1 == Just ((), Map.empty)

scene_route_path_rejects_extra_url_keys :: Bool
scene_route_path_rejects_extra_url_keys =
  let layoutRouteResult :: Either String (Route.Route String () Route.UrlParams)
      layoutRouteResult = Route.route "/layout"
      routeResult :: Either String (Route.Route String () Route.UrlParams)
      routeResult = Route.route "/layout/items/{:id}"
  in case (layoutRouteResult, routeResult) of
      (Right layoutRoute, Right layoutItemsRoute) ->
        let router =
              layoutItemsRoute Route.:> layoutRoute Route.:> Route.RNil
            h0 = Scene.history ["/root"]
            h1 = Route.gotoRoute Scene.Push layoutItemsRoute () (Map.fromList [("id", "42")]) router h0
            locBad =
              Scene.Location
                { Scene.locationSegments = ["/layout", "/layout/items"]
                , Scene.locationSearchParams = Map.empty
                , Scene.locationUrlParams = Map.fromList [("id", "42"), ("other", "x")]
                }
        in segmentsOfString h1 == ["/layout", "/layout/items"]
            && Route.matchRoute layoutItemsRoute router locBad == Nothing
      _ -> False

scene_route_auto_codec_roundtrip :: Bool
scene_route_auto_codec_roundtrip =
  let rootRouteResult :: Either String (Route.Route String () Route.UrlParams)
      rootRouteResult = Route.route "/root"
      gameRouteResult :: Either String (Route.Route String AutoSearch AutoUrl)
      gameRouteResult = Route.route "/root/game/{:autoPlayerId}"
  in case (rootRouteResult, gameRouteResult) of
      (Right rootRoute, Right gameRoute) ->
        let router =
              gameRoute Route.:> rootRoute Route.:> Route.RNil
            h0 = Scene.history ["/start"]
            h1 = Route.gotoRoute Scene.Push gameRoute (AutoSearch ["stats"]) (AutoUrl "42") router h0
        in segmentsOfString h1 == ["/root", "/root/game"]
            && Route.currentRoute gameRoute router h1 == Just (AutoSearch ["stats"], AutoUrl "42")
      _ -> False

scene_route_scene_handler_receives_validated_params :: Bool
scene_route_scene_handler_receives_validated_params =
  let rootRouteResult :: Either String (Route.Route String () Route.UrlParams)
      rootRouteResult = Route.route "/root"
      gameRouteResult :: Either String (Route.Route String AutoSearch AutoUrl)
      gameRouteResult = Route.route "/root/game/{:autoPlayerId}"
  in case (rootRouteResult, gameRouteResult) of
      (Right rootRoute, Right gameRoute) ->
        let router =
              gameRoute Route.:> rootRoute Route.:> Route.RNil
            h0 = Scene.history ["/start"]
            h1 = Route.gotoRoute Scene.Push gameRoute (AutoSearch ["stats"]) (AutoUrl "42") router h0
            renderScene =
              Route.onRoute
                gameRoute
                router
                (\(AutoSearch tabs) (AutoUrl pid) -> pid <> ":" <> show tabs)
            locBad =
              Scene.Location
                { Scene.locationSegments = ["/root", "/root/game"]
                , Scene.locationSearchParams = Map.fromList [("autoTab", ["stats"])]
                , Scene.locationUrlParams = Map.fromList [("autoPlayerId", "42"), ("extra", "x")]
                }
            badResult =
              Route.onRouteAt
                gameRoute
                router
                (\_ _ -> "should-not-run")
                locBad
        in renderScene h1 == Just "42:[\"stats\"]"
            && badResult == Nothing
      _ -> False

scene_route_specificity_prefers_literal :: Bool
scene_route_specificity_prefers_literal =
  let exactRouteResult :: Either String (Route.Route String () Route.UrlParams)
      exactRouteResult = Route.route "/a/b/c"
      dynamicRouteResult :: Either String (Route.Route String () Route.UrlParams)
      dynamicRouteResult = Route.route "/a/b/{:id}"
  in case (exactRouteResult, dynamicRouteResult) of
      (Right exactRoute, Right dynamicRoute) ->
        let router = dynamicRoute Route.:> exactRoute Route.:> Route.RNil
            h0 = Scene.history ["/start"]
            h1 = Route.gotoPath Scene.Push "/a/b/c" router h0
            h2 = Route.gotoPath Scene.Push "/a/b/e" router h1
        in Route.currentRoute exactRoute router h1 == Just ((), Map.empty)
            && Route.currentRoute dynamicRoute router h1 == Nothing
            && Route.currentRoute dynamicRoute router h2 == Just ((), Map.fromList [("id", "e")])
            && Route.currentRoute exactRoute router h2 == Nothing
      _ -> False
