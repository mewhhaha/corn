{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Engine.Data.Route.Simple
  ( Params
  , param
  , route
  , Route(..)
  , Routes(EmptyRoutes, (:>))
  , Router
  , createRouter
  , resolvePath
  , gotoPath
  , enterPath
  , enterPathWithSearch
  , enterAt
  ) where

import Prelude

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Engine.Data.Route as Core
import qualified Engine.Data.Scene as Scene

-- URL params validated by the matched route pattern.
newtype Params (path :: Symbol) = Params
  (Map String String)

param :: forall key path. KnownSymbol key => Params path -> String
param (Params urlMap) =
  let key = symbolVal (Proxy @key)
  in case Map.lookup key urlMap of
      Just value -> value
      Nothing -> error ("missing route param :" <> key)

data Route (path :: Symbol) c msg search = Route
  { enter :: Params path -> search -> Scene.SceneRuntime c msg
  , params :: Maybe search
  }

route ::
  (Params path -> search -> Scene.SceneRuntime c msg) ->
  Maybe search ->
  Route path c msg search
route enterFn defaults =
  Route
    { enter = enterFn
    , params = defaults
    }

data Routes c msg where
  EmptyRoutes :: Routes c msg
  (:>) ::
    (KnownSymbol path, Core.SearchCodec search) =>
    Route path c msg search ->
    Routes c msg ->
    Routes c msg
infixr 5 :>

data Entry c msg where
  Entry ::
    (KnownSymbol path, Core.SearchCodec search) =>
    { entryMeta :: Core.Meta String search Core.UrlParams
    , entryEnter :: Params path -> search -> Scene.SceneRuntime c msg
    , entryDefaults :: Maybe search
    } ->
    Entry c msg

data Router c msg = Router
  { routerCompiled :: Core.CompiledTree c msg String
  , routerEntries :: [Entry c msg]
  }

buildEntry ::
  forall path search c msg.
  (KnownSymbol path, Core.SearchCodec search) =>
  Route path c msg search ->
  Either String (Entry c msg)
buildEntry routeDef = do
  let patternText = symbolVal (Proxy @path)
  routeMeta <- Core.route @search @Core.UrlParams patternText
  pure
    Entry
      { entryMeta = routeMeta
      , entryEnter = enter routeDef
      , entryDefaults = params routeDef
      }

collectEntries :: Routes c msg -> Either String [Entry c msg]
collectEntries routes =
  case routes of
    EmptyRoutes -> Right []
    routeDef :> rest -> do
      one <- buildEntry routeDef
      more <- collectEntries rest
      pure (one : more)

entryTree :: Entry c msg -> Core.RouteTree c msg String
entryTree (Entry routeMeta enterFn _) =
  let mkScene search urlMap =
        enterFn (Params urlMap) search
  in Core.routeLeaf routeMeta mkScene

createRouter :: Routes c msg -> Either String (Router c msg)
createRouter routes = do
  entries <- collectEntries routes
  let compiled = Core.compileTree (map entryTree entries)
  pure
    Router
      { routerCompiled = compiled
      , routerEntries = entries
      }

resolvePath :: Router c msg -> String -> Maybe (Scene.Location String)
resolvePath router pathText =
  Core.resolveCompiledPath (routerCompiled router) pathText

gotoPath ::
  Scene.GotoMode ->
  String ->
  Router c msg ->
  Scene.History String ->
  Scene.History String
gotoPath mode pathText router h =
  Core.gotoCompiledPath mode pathText (routerCompiled router) h

runEntryAtWith ::
  Maybe Core.SearchParams ->
  Entry c msg ->
  Scene.Location String ->
  Maybe (Scene.SceneRuntime c msg)
runEntryAtWith maybeSearch (Entry routeMeta enterFn defaults) loc =
  let
      searchMap =
        case maybeSearch of
          Just supplied -> supplied
          Nothing ->
            case defaults of
              Just defaultSearch -> Core.encodeSearchCodec defaultSearch
              Nothing -> Map.empty
      locWithSearch =
        loc
          { Scene.locationSearchParams = searchMap
          }
      singletonRouter = routeMeta Core.:> Core.RNil
  in case Core.matchRoute routeMeta singletonRouter locWithSearch of
      Nothing -> Nothing
      Just (searchParams, urlParams) ->
        Just (enterFn (Params urlParams) searchParams)

enterAt :: Router c msg -> Scene.Location String -> Maybe (Scene.SceneRuntime c msg)
enterAt router loc =
  enterAtWithSearch Nothing router loc

enterAtWithSearch ::
  Maybe Core.SearchParams ->
  Router c msg ->
  Scene.Location String ->
  Maybe (Scene.SceneRuntime c msg)
enterAtWithSearch maybeSearch router loc =
  go (routerEntries router)
  where
    go entries =
      case entries of
        [] -> Nothing
        entry : rest ->
          case runEntryAtWith maybeSearch entry loc of
            Just rt -> Just rt
            Nothing -> go rest

enterPath ::
  Router c msg ->
  String ->
  Maybe (Scene.SceneRuntime c msg)
enterPath router pathText = do
  loc <- resolvePath router pathText
  enterAt router loc

enterPathWithSearch ::
  Router c msg ->
  String ->
  Core.SearchParams ->
  Maybe (Scene.SceneRuntime c msg)
enterPathWithSearch router pathText searchParams = do
  loc <- resolvePath router pathText
  enterAtWithSearch (Just searchParams) router loc
