{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

module Engine.Corn.App
  ( SearchParams
  , UrlParams
  , SearchCodec
  , Simple.RouteEvent(..)
  , Params
  , param
  , Simple.Ctx(..)
  , Layer
  , layer
  , RouteTree
  , leaf
  , stack
  , Simple.Routes(EmptyRoutes, (:>))
  , Simple.Nav(..)
  , Path(..)
  , Runtime
  , create
  , navigate
  , step
  , current
  , currentPath
  , canGoBack
  , canGoForward
  ) where

import Prelude

import GHC.TypeLits (KnownSymbol)
import Engine.Corn.App.Path (Path(..))
import Engine.Data.Route.Simple (SearchCodec)
import qualified Engine.Data.Route.Simple as Simple
import qualified Engine.Data.Scene as Scene

type SearchParams = Simple.SearchParams
type UrlParams = Simple.UrlParams
type Params = Simple.Params
type Ctx = Simple.Ctx
type Layer path st msg out search = Simple.Route path st msg out search
type RouteTree st msg out paths = Simple.RouteTree st msg out paths
type Runtime st msg out paths = Simple.Runtime st msg out paths
type Nav = Simple.Nav

layer ::
  (Params path -> search -> st) ->
  (Ctx path search -> Double -> [msg] -> st -> (st, [out], [Nav])) ->
  Maybe search ->
  Layer path st msg out search
layer = Simple.route

leaf :: (KnownSymbol path, SearchCodec search) => Layer path st msg out search -> RouteTree st msg out '[path]
leaf = Simple.leaf

stack ::
  (KnownSymbol path, SearchCodec search) =>
  Layer path st msg out search ->
  Simple.Routes st msg out childPaths ->
  RouteTree st msg out (path ': childPaths)
stack = Simple.node

param :: forall key path. KnownSymbol key => Params path -> String
param = Simple.param @key

create :: Simple.Routes st msg out paths -> String -> Either String (Runtime st msg out paths)
create = Simple.create

navigate :: Nav -> Runtime st msg out paths -> Runtime st msg out paths
navigate = Simple.navigate

step ::
  Double ->
  [msg] ->
  Runtime st msg out paths ->
  (Runtime st msg out paths, [out], [Nav])
step = Simple.step

current :: Runtime st msg out paths -> Path String
current rt =
  let loc = Simple.current rt
  in
    Path
      { pathSegments = Scene.locationSegments loc
      , pathSearchParams = Scene.locationSearchParams loc
      , pathUrlParams = Scene.locationUrlParams loc
      }

currentPath :: Runtime st msg out paths -> [String]
currentPath = pathSegments . current

canGoBack :: Runtime st msg out paths -> Bool
canGoBack = Simple.canGoBack

canGoForward :: Runtime st msg out paths -> Bool
canGoForward = Simple.canGoForward
