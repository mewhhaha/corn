{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Engine.Data.Route.Simple
  ( SearchParams
  , UrlParams
  , SearchCodec
  , HasPath
  , RouteEvent(..)
  , StepCtx(..)
  , StepRoute
  , stepRoute
  , StepRoutes(EmptyStepRoutes, (:>>))
  , StepRouter
  , Nav(..)
  , Runtime
  , create
  , navigate
  , step
  , current
  , canGoBack
  , canGoForward
  , SceneMap
  , active
  , Params
  , param
  , route
  , Route(..)
  , Routes(EmptyRoutes, (:>))
  , Router
  , createRouter
  , sync
  , goto
  , gotoPath
  , back
  , forward
  ) where

import Prelude

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Proxy (Proxy(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Engine.Data.FRP (DTime)
import Engine.Data.Route (SearchCodec)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Engine.Data.Route as Core
import qualified Engine.Data.Scene as Scene

type SearchParams = Core.SearchParams

type UrlParams = Core.UrlParams

type SceneMap c msg = Map String (Scene.SceneRuntime c msg)

class HasPath (path :: Symbol) (paths :: [Symbol])

instance {-# OVERLAPPING #-} HasPath path (path ': paths)

instance HasPath path paths => HasPath path (other ': paths)

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

data RouteEvent
  = Entered
  | Exited
  | BecameTop
  | LeftTop
  deriving (Eq, Show)

data StepCtx (path :: Symbol) search = StepCtx
  { stepParams :: !(Params path)
  , stepSearch :: !search
  , stepEvents :: ![RouteEvent]
  }

data StepRoute (path :: Symbol) st msg search = StepRoute
  { stepInit :: Params path -> search -> st
  , stepFn :: StepCtx path search -> DTime -> [msg] -> st -> (st, [msg])
  , stepDefaults :: Maybe search
  }

stepRoute ::
  (Params path -> search -> st) ->
  (StepCtx path search -> DTime -> [msg] -> st -> (st, [msg])) ->
  Maybe search ->
  StepRoute path st msg search
stepRoute initFn stepFn defaults =
  StepRoute
    { stepInit = initFn
    , stepFn = stepFn
    , stepDefaults = defaults
    }

data StepRoutes st msg (paths :: [Symbol]) where
  EmptyStepRoutes :: StepRoutes st msg '[]
  (:>>) ::
    (KnownSymbol path, Core.SearchCodec search) =>
    StepRoute path st msg search ->
    StepRoutes st msg paths ->
    StepRoutes st msg (path ': paths)
infixr 5 :>>

data StepEntry st msg where
  StepEntry ::
    (KnownSymbol path, Core.SearchCodec search) =>
    { stepEntryMeta :: Core.Meta String search Core.UrlParams
    , stepEntryInit :: Params path -> search -> st
    , stepEntryFn :: StepCtx path search -> DTime -> [msg] -> st -> (st, [msg])
    , stepEntryDefaults :: Maybe search
    } ->
    StepEntry st msg

data StepRouter st msg (paths :: [Symbol]) = StepRouter
  { stepRouterCompiled :: Core.CompiledTree () () String
  , stepRouterEntries :: [StepEntry st msg]
  }

data Nav
  = Goto Scene.GotoMode String
  | Back
  | Forward
  deriving (Eq, Show)

data Runtime st msg (paths :: [Symbol]) = Runtime
  { runtimeRouter :: StepRouter st msg paths
  , runtimeHistory :: Scene.History String
  , runtimeStates :: Map String st
  , runtimePrevActive :: [String]
  , runtimePrevLocs :: Map String (Scene.Location String)
  }

current :: Runtime st msg paths -> Scene.Location String
current = Scene.current . runtimeHistory

canGoBack :: Runtime st msg paths -> Bool
canGoBack = Scene.canGoBack . runtimeHistory

canGoForward :: Runtime st msg paths -> Bool
canGoForward = Scene.canGoForward . runtimeHistory

buildStepEntry ::
  forall path st msg search.
  (KnownSymbol path, Core.SearchCodec search) =>
  StepRoute path st msg search ->
  Either String (StepEntry st msg)
buildStepEntry routeDef = do
  let patternText = symbolVal (Proxy @path)
  routeMeta <- Core.route @search @Core.UrlParams patternText
  pure
    StepEntry
      { stepEntryMeta = routeMeta
      , stepEntryInit = stepInit routeDef
      , stepEntryFn = stepFn routeDef
      , stepEntryDefaults = stepDefaults routeDef
      }

collectStepEntries :: StepRoutes st msg paths -> Either String [StepEntry st msg]
collectStepEntries routes =
  case routes of
    EmptyStepRoutes -> Right []
    routeDef :>> rest -> do
      one <- buildStepEntry routeDef
      more <- collectStepEntries rest
      pure (one : more)

stepEntryTree :: StepEntry st msg -> Core.RouteTree () () String
stepEntryTree (StepEntry routeMeta _ _ _) =
  Core.routeLeaf routeMeta (\_ _ -> error "router runtime compiled matcher should not execute route leaves")

createStepRouter :: StepRoutes st msg paths -> Either String (StepRouter st msg paths)
createStepRouter routes = do
  entries <- collectStepEntries routes
  let compiled = Core.compileTree (map stepEntryTree entries)
  pure
    StepRouter
      { stepRouterCompiled = compiled
      , stepRouterEntries = entries
      }

create ::
  StepRoutes st msg paths ->
  String ->
  Either String (Runtime st msg paths)
create routes initialPath = do
  router <- createStepRouter routes
  loc <-
    case Core.resolveCompiledPath (stepRouterCompiled router) initialPath of
      Just resolved -> Right resolved
      Nothing -> Left ("router create: no route for path " <> initialPath)
  pure
    Runtime
      { runtimeRouter = router
      , runtimeHistory = Scene.historyAt loc
      , runtimeStates = Map.empty
      , runtimePrevActive = []
      , runtimePrevLocs = Map.empty
      }

navigate :: Nav -> Runtime st msg paths -> Runtime st msg paths
navigate nav rt =
  let h0 = runtimeHistory rt
      compiled = stepRouterCompiled (runtimeRouter rt)
      h1 =
        case nav of
          Goto mode pathText ->
            Core.gotoCompiledPath mode pathText compiled h0
          Back ->
            Scene.back h0
          Forward ->
            Scene.forward h0
  in rt
      { runtimeHistory = h1
      }

step ::
  DTime ->
  [msg] ->
  Runtime st msg paths ->
  (Runtime st msg paths, [msg])
step dt external rt0 =
  let inbound = external
      entries = stepRouterEntries (runtimeRouter rt0)
      currentLoc = Scene.current (runtimeHistory rt0)
      currentSegs = Scene.locationSegments currentLoc
      currentTop = lastMaybe currentSegs
      currentLocs = currentLocBySid currentLoc
      prevSegs = runtimePrevActive rt0
      prevTop = lastMaybe prevSegs
      curSet = Set.fromList currentSegs
      prevSet = Set.fromList prevSegs
      enteredSet = Set.difference curSet prevSet
      exitedSet = Set.difference prevSet curSet
      exitedOrder = reverse (filter (`Set.member` exitedSet) prevSegs)
      routeEvents sid =
        routeEventsFor sid enteredSet exitedSet prevTop currentTop
      (statesAfterExit, exitOutbox) =
        foldl'
          (runExitedEntry entries dt inbound routeEvents (runtimePrevLocs rt0))
          (runtimeStates rt0, [])
          exitedOrder
      (statesAfterActive, activeOutbox, nextLocs) =
        foldl'
          (runActiveEntry entries dt inbound routeEvents)
          (statesAfterExit, exitOutbox, Map.empty)
          currentLocs
      outbox = activeOutbox
      rt1 =
        rt0
          { runtimeStates = statesAfterActive
          , runtimePrevActive = currentSegs
          , runtimePrevLocs = nextLocs
          }
  in (rt1, outbox)

runExitedEntry ::
  [StepEntry st msg] ->
  DTime ->
  [msg] ->
  (String -> [RouteEvent]) ->
  Map String (Scene.Location String) ->
  (Map String st, [msg]) ->
  String ->
  (Map String st, [msg])
runExitedEntry entries dt inbound routeEvents prevLocs (states, outbox) sid =
  case Map.lookup sid prevLocs of
    Nothing ->
      (states, outbox)
    Just oldLoc ->
      let (states1, out1, _) =
            runStepAt entries sid oldLoc (routeEvents sid) dt inbound states
      in (states1, outbox <> out1)

runActiveEntry ::
  [StepEntry st msg] ->
  DTime ->
  [msg] ->
  (String -> [RouteEvent]) ->
  (Map String st, [msg], Map String (Scene.Location String)) ->
  (String, Scene.Location String) ->
  (Map String st, [msg], Map String (Scene.Location String))
runActiveEntry entries dt inbound routeEvents (states, outbox, locs) (sid, loc) =
  let (states1, out1, maybeResolvedLoc) =
        runStepAt entries sid loc (routeEvents sid) dt inbound states
      locs1 =
        case maybeResolvedLoc of
          Just resolvedLoc -> Map.insert sid resolvedLoc locs
          Nothing -> locs
  in (states1, outbox <> out1, locs1)

runStepAt ::
  [StepEntry st msg] ->
  String ->
  Scene.Location String ->
  [RouteEvent] ->
  DTime ->
  [msg] ->
  Map String st ->
  (Map String st, [msg], Maybe (Scene.Location String))
runStepAt entries sid loc events dt inbox states0 =
  go entries
  where
    go remaining =
      case remaining of
        [] ->
          (states0, [], Nothing)
        entry : rest ->
          case runStepEntry entries sid loc events dt inbox states0 entry of
            Just (states1, out1, resolvedLoc) ->
              (states1, out1, Just resolvedLoc)
            Nothing ->
              go rest

runStepEntry ::
  [StepEntry st msg] ->
  String ->
  Scene.Location String ->
  [RouteEvent] ->
  DTime ->
  [msg] ->
  Map String st ->
  StepEntry st msg ->
  Maybe (Map String st, [msg], Scene.Location String)
runStepEntry allEntries sid loc events dt inbox states0 (StepEntry routeMeta initFn runFn defaults) = do
  leaf <- Core.routeLeafSegment routeMeta
  if leaf /= sid
    then Nothing
    else
      let expectedSegments = resolvedStepRouteSegments allEntries routeMeta
          urlKeys = Core.routeUrlParamKeys routeMeta
          searchKeys = Core.routeSearchParamKeys routeMeta
          urlMap = selectKeys urlKeys (Scene.locationUrlParams loc)
          providedSearch = selectKeys searchKeys (Scene.locationSearchParams loc)
          searchMap =
            if Map.null providedSearch
              then
                case defaults of
                  Just defaultSearch -> Core.encodeSearchCodec defaultSearch
                  Nothing -> Map.empty
              else providedSearch
          resolvedLoc =
            Scene.Location
              { Scene.locationSegments = expectedSegments
              , Scene.locationSearchParams = searchMap
              , Scene.locationUrlParams = urlMap
              }
      in
        if Scene.locationSegments loc /= expectedSegments
            || not (sameKeyList searchKeys searchMap)
            || not (sameKeyList urlKeys urlMap)
          then Nothing
          else do
            searchParams <- Core.decodeSearchCodec searchMap
            let routeParams = Params urlMap
                state0 = Map.findWithDefault (initFn routeParams searchParams) sid states0
                ctx =
                  StepCtx
                    { stepParams = routeParams
                    , stepSearch = searchParams
                    , stepEvents = events
                    }
                (state1, out1) = runFn ctx dt inbox state0
            pure (Map.insert sid state1 states0, out1, resolvedLoc)

routeEventsFor ::
  String ->
  Set String ->
  Set String ->
  Maybe String ->
  Maybe String ->
  [RouteEvent]
routeEventsFor sid entered exited prevTop currentTop =
  enteredEvents <> exitedEvents <> topEvents
  where
    enteredEvents =
      if Set.member sid entered
        then [Entered]
        else []
    exitedEvents =
      if Set.member sid exited
        then [Exited]
        else []
    topEvents =
      becameTopEvent <> leftTopEvent
    becameTopEvent =
      if currentTop == Just sid && prevTop /= Just sid
        then [BecameTop]
        else []
    leftTopEvent =
      if prevTop == Just sid && currentTop /= Just sid
        then [LeftTop]
        else []

selectKeys :: [String] -> Map String a -> Map String a
selectKeys keys m =
  Map.restrictKeys m (Set.fromList keys)

currentLocBySid :: Scene.Location String -> [(String, Scene.Location String)]
currentLocBySid currentLoc =
  zipWith mk [1 ..] segs
  where
    segs = Scene.locationSegments currentLoc
    fullSearch = Scene.locationSearchParams currentLoc
    fullUrl = Scene.locationUrlParams currentLoc
    mk ix sid =
      ( sid
      , Scene.Location
          { Scene.locationSegments = take ix segs
          , Scene.locationSearchParams = fullSearch
          , Scene.locationUrlParams = fullUrl
          }
      )

lastMaybe :: [a] -> Maybe a
lastMaybe xs =
  case reverse xs of
    [] -> Nothing
    y : _ -> Just y

knownStepRouteLeafSegments :: [StepEntry st msg] -> Set String
knownStepRouteLeafSegments entries =
  foldr
    (\(StepEntry routeMeta _ _ _) acc ->
      case Core.routeLeafSegment routeMeta of
        Nothing -> acc
        Just leaf -> Set.insert leaf acc
    )
    Set.empty
    entries

resolvedStepRouteSegments ::
  [StepEntry st msg] ->
  Core.Meta String search Core.UrlParams ->
  [String]
resolvedStepRouteSegments entries routeMeta =
  let knownLeaves = knownStepRouteLeafSegments entries
      matched = filter (`Set.member` knownLeaves) (Core.routeSegments routeMeta)
  in
    if null matched
      then maybeToList (Core.routeLeafSegment routeMeta)
      else matched

data Routes c msg (paths :: [Symbol]) where
  EmptyRoutes :: Routes c msg '[]
  (:>) ::
    (KnownSymbol path, Core.SearchCodec search) =>
    Route path c msg search ->
    Routes c msg paths ->
    Routes c msg (path ': paths)
infixr 5 :>

data Entry c msg where
  Entry ::
    (KnownSymbol path, Core.SearchCodec search) =>
    { entryMeta :: Core.Meta String search Core.UrlParams
    , entryEnter :: Params path -> search -> Scene.SceneRuntime c msg
    , entryDefaults :: Maybe search
    } ->
    Entry c msg

data Router c msg (paths :: [Symbol]) = Router
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

collectEntries :: Routes c msg paths -> Either String [Entry c msg]
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

createRouter :: Routes c msg paths -> Either String (Router c msg paths)
createRouter routes = do
  entries <- collectEntries routes
  let compiled = Core.compileTree (map entryTree entries)
  pure
    Router
      { routerCompiled = compiled
      , routerEntries = entries
      }

sync ::
  Router c msg paths ->
  Scene.History String ->
  SceneMap c msg
sync router h =
  foldl' addScene Map.empty (zip [1 ..] currentSegments)
  where
    currentLoc = Scene.current h
    currentSegments = Scene.locationSegments currentLoc
    total = length currentSegments
    addScene acc (ix, sid) =
      case mountAtIndex ix of
          Just rt -> Map.insert sid rt acc
          Nothing -> acc
    mountAtIndex ix =
      let prefix = take ix currentSegments
          locAt =
            if ix == total
              then currentLoc
              else
                Scene.Location
                  { Scene.locationSegments = prefix
                  , Scene.locationSearchParams = Map.empty
                  , Scene.locationUrlParams = Map.empty
                  }
      in enterAt router locAt

active ::
  Scene.History String ->
  SceneMap c msg ->
  [(String, Scene.SceneRuntime c msg)]
active h sceneMap =
  mapMaybe pick (Scene.locationSegments (Scene.current h))
  where
    pick sid =
      case Map.lookup sid sceneMap of
        Just rt -> Just (sid, rt)
        Nothing -> Nothing

goto ::
  forall path paths c msg.
  (KnownSymbol path, HasPath path paths) =>
  Scene.GotoMode ->
  Router c msg paths ->
  Scene.History String ->
  (Scene.History String, SceneMap c msg)
goto mode router h =
  gotoPath mode (symbolVal (Proxy @path)) router h

gotoPath ::
  Scene.GotoMode ->
  String ->
  Router c msg paths ->
  Scene.History String ->
  (Scene.History String, SceneMap c msg)
gotoPath mode pathText router h =
  let h1 = Core.gotoCompiledPath mode pathText (routerCompiled router) h
      sceneMap1 = sync router h1
  in (h1, sceneMap1)

back ::
  Router c msg paths ->
  Scene.History String ->
  (Scene.History String, SceneMap c msg)
back router h =
  let h1 = Scene.back h
  in (h1, sync router h1)

forward ::
  Router c msg paths ->
  Scene.History String ->
  (Scene.History String, SceneMap c msg)
forward router h =
  let h1 = Scene.forward h
  in (h1, sync router h1)

runEntryAtWith ::
  Maybe Core.SearchParams ->
  [Entry c msg] ->
  Entry c msg ->
  Scene.Location String ->
  Maybe (Scene.SceneRuntime c msg)
runEntryAtWith maybeSearch allEntries (Entry routeMeta enterFn defaults) loc =
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
      expectedSegments = resolvedRouteSegments allEntries routeMeta
  in if Scene.locationSegments locWithSearch /= expectedSegments
      then Nothing
      else
        if not (sameKeyList (Core.routeSearchParamKeys routeMeta) (Scene.locationSearchParams locWithSearch))
            || not (sameKeyList (Core.routeUrlParamKeys routeMeta) (Scene.locationUrlParams locWithSearch))
          then Nothing
          else do
            searchParams <- Core.decodeSearchCodec (Scene.locationSearchParams locWithSearch)
            Just (enterFn (Params (Scene.locationUrlParams locWithSearch)) searchParams)

enterAt :: Router c msg paths -> Scene.Location String -> Maybe (Scene.SceneRuntime c msg)
enterAt router loc =
  enterAtWithSearch Nothing router loc

enterAtWithSearch ::
  Maybe Core.SearchParams ->
  Router c msg paths ->
  Scene.Location String ->
  Maybe (Scene.SceneRuntime c msg)
enterAtWithSearch maybeSearch router loc =
  go finalSearch entries entries
  where
    entries = routerEntries router
    locSearch = Scene.locationSearchParams loc
    finalSearch =
      case maybeSearch of
        Just supplied -> Just supplied
        Nothing ->
          if Map.null locSearch
            then Nothing
            else Just locSearch
    go mSearch allEntries remainingEntries =
      case remainingEntries of
        [] -> Nothing
        entry : rest ->
          case runEntryAtWith mSearch allEntries entry loc of
            Just rt -> Just rt
            Nothing -> go mSearch allEntries rest

sameKeyList :: [String] -> Map String a -> Bool
sameKeyList keys actualMap =
  Set.fromList keys == Map.keysSet actualMap

knownRouteLeafSegments :: [Entry c msg] -> Set String
knownRouteLeafSegments entries =
  foldr
    (\(Entry routeMeta _ _) acc ->
      case Core.routeLeafSegment routeMeta of
        Nothing -> acc
        Just leaf -> Set.insert leaf acc
    )
    Set.empty
    entries

resolvedRouteSegments ::
  [Entry c msg] ->
  Core.Meta String search Core.UrlParams ->
  [String]
resolvedRouteSegments entries routeMeta =
  let knownLeaves = knownRouteLeafSegments entries
      matched = filter (`Set.member` knownLeaves) (Core.routeSegments routeMeta)
  in
    if null matched
      then maybeToList (Core.routeLeafSegment routeMeta)
      else matched

{-# DEPRECATED SceneMap "Legacy sync API. Prefer Route.Runtime + Route.create/Route.step/Route.navigate." #-}
{-# DEPRECATED active "Legacy sync API. Prefer Route.step output and Route.current." #-}
{-# DEPRECATED Route "Legacy sync API. Prefer StepRoute/StepRoutes." #-}
{-# DEPRECATED route "Legacy sync API. Prefer stepRoute." #-}
{-# DEPRECATED Routes "Legacy sync API. Prefer StepRoutes." #-}
{-# DEPRECATED Router "Legacy sync API. Prefer Runtime." #-}
{-# DEPRECATED createRouter "Legacy sync API. Prefer create." #-}
{-# DEPRECATED sync "Legacy sync API. Prefer Runtime managed by step." #-}
{-# DEPRECATED goto "Legacy sync API. Prefer navigate (Goto ...)." #-}
{-# DEPRECATED gotoPath "Legacy sync API. Prefer navigate (Goto ...)." #-}
{-# DEPRECATED back "Legacy sync API. Prefer navigate Back." #-}
{-# DEPRECATED forward "Legacy sync API. Prefer navigate Forward." #-}
