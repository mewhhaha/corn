{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Engine.Data.Route.Simple
  ( SearchParams
  , UrlParams
  , SearchCodec
  , HasPath
  , RouteEvent(..)
  , Params
  , param
  , Ctx(..)
  , Route
  , route
  , RouteTree
  , leaf
  , node
  , Routes(EmptyRoutes, (:>))
  , Nav(..)
  , Runtime
  , create
  , navigate
  , step
  , current
  , canGoBack
  , canGoForward
  ) where

import Prelude

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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

data RouteEvent
  = Entered
  | Exited
  | BecameTop
  | LeftTop
  deriving (Eq, Show)

data Ctx (path :: Symbol) search = Ctx
  { ctxParams :: !(Params path)
  , ctxSearch :: !search
  , ctxEvents :: ![RouteEvent]
  }

data Route (path :: Symbol) st msg out search = Route
  { routeInit :: Params path -> search -> st
  , routeStep :: Ctx path search -> DTime -> [msg] -> st -> (st, [out], [Nav])
  , routeDefaults :: Maybe search
  }

route ::
  (Params path -> search -> st) ->
  (Ctx path search -> DTime -> [msg] -> st -> (st, [out], [Nav])) ->
  Maybe search ->
  Route path st msg out search
route initFn stepFn defaults =
  Route
    { routeInit = initFn
    , routeStep = stepFn
    , routeDefaults = defaults
    }

type family Append (xs :: [Symbol]) (ys :: [Symbol]) :: [Symbol] where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

data RouteTree st msg out (paths :: [Symbol]) where
  RouteLeaf ::
    (KnownSymbol path, Core.SearchCodec search) =>
    Route path st msg out search ->
    RouteTree st msg out '[path]
  RouteNode ::
    (KnownSymbol path, Core.SearchCodec search) =>
    Route path st msg out search ->
    Routes st msg out childPaths ->
    RouteTree st msg out (path ': childPaths)

leaf ::
  (KnownSymbol path, Core.SearchCodec search) =>
  Route path st msg out search ->
  RouteTree st msg out '[path]
leaf = RouteLeaf

node ::
  (KnownSymbol path, Core.SearchCodec search) =>
  Route path st msg out search ->
  Routes st msg out childPaths ->
  RouteTree st msg out (path ': childPaths)
node = RouteNode

data Routes st msg out (paths :: [Symbol]) where
  EmptyRoutes :: Routes st msg out '[]
  (:>) ::
    RouteTree st msg out routePaths ->
    Routes st msg out restPaths ->
    Routes st msg out (Append routePaths restPaths)
infixr 5 :>

data Entry st msg out where
  Entry ::
    (KnownSymbol path, Core.SearchCodec search) =>
    { entryMeta :: Core.Meta String search Core.UrlParams
    , entrySid :: String
    , entryStack :: [String]
    , entryInit :: Params path -> search -> st
    , entryStep :: Ctx path search -> DTime -> [msg] -> st -> (st, [out], [Nav])
    , entryDefaults :: Maybe search
    } ->
    Entry st msg out

data Nav
  = Push String
  | Replace String
  | PushWith String SearchParams
  | ReplaceWith String SearchParams
  | Back
  | Forward
  deriving (Eq, Show)

data Runtime st msg out (paths :: [Symbol]) = Runtime
  { runtimeCompiled :: Core.CompiledTree () () String
  , runtimeEntriesBySid :: Map String (Entry st msg out)
  , runtimeHistory :: Scene.History String
  , runtimeStates :: Map String st
  , runtimePrevActive :: [String]
  , runtimePrevLocs :: Map String (Scene.Location String)
  }

current :: Runtime st msg out paths -> Scene.Location String
current = Scene.current . runtimeHistory

canGoBack :: Runtime st msg out paths -> Bool
canGoBack = Scene.canGoBack . runtimeHistory

canGoForward :: Runtime st msg out paths -> Bool
canGoForward = Scene.canGoForward . runtimeHistory

buildEntry ::
  forall path st msg out search.
  (KnownSymbol path, Core.SearchCodec search) =>
  [String] ->
  Route path st msg out search ->
  Either String (Entry st msg out)
buildEntry parentStack routeDef = do
  let patternText = symbolVal (Proxy @path)
  routeMeta <- Core.route @search @Core.UrlParams patternText
  leafSid <-
    case Core.routeLeafSegment routeMeta of
      Just sid -> Right sid
      Nothing -> Left ("router create: route " <> show patternText <> " must include at least one literal segment")
  pure
    Entry
      { entryMeta = routeMeta
      , entrySid = leafSid
      , entryStack = parentStack <> [leafSid]
      , entryInit = routeInit routeDef
      , entryStep = routeStep routeDef
      , entryDefaults = routeDefaults routeDef
      }

collectTreeEntries ::
  [String] ->
  RouteTree st msg out routePaths ->
  Either String [Entry st msg out]
collectTreeEntries parentStack tree =
  case tree of
    RouteLeaf routeDef ->
      pure . pure =<< buildEntry parentStack routeDef
    RouteNode routeDef children -> do
      entry <- buildEntry parentStack routeDef
      childEntries <- collectRoutesEntries (entryStack entry) children
      pure (entry : childEntries)

collectRoutesEntries ::
  [String] ->
  Routes st msg out paths ->
  Either String [Entry st msg out]
collectRoutesEntries parentStack routes =
  case routes of
    EmptyRoutes -> Right []
    tree :> rest -> do
      one <- collectTreeEntries parentStack tree
      more <- collectRoutesEntries parentStack rest
      pure (one <> more)

entryTree :: Entry st msg out -> Core.RouteTree () () String
entryTree (Entry routeMeta _ _ _ _ _) =
  Core.routeLeaf routeMeta (\_ _ -> error "router matcher leaf should never execute scene runtime")

entryPattern :: Entry st msg out -> String
entryPattern (Entry routeMeta _ _ _ _ _) = Core.routePattern routeMeta

entryStackOf :: Entry st msg out -> [String]
entryStackOf (Entry _ _ stack _ _ _) = stack

entryBySidMap :: [Entry st msg out] -> Map String (Entry st msg out)
entryBySidMap entries =
  Map.fromList [(sid, entry) | entry@(Entry _ sid _ _ _ _) <- entries]

create ::
  Routes st msg out paths ->
  String ->
  Either String (Runtime st msg out paths)
create routes initialPath = do
  entries <- collectRoutesEntries [] routes
  validateUniqueEntries entries
  let compiled = Core.compileTree (map entryTree entries)
      bySid = entryBySidMap entries
  loc <- resolvePath compiled bySid initialPath
  pure
    Runtime
      { runtimeCompiled = compiled
      , runtimeEntriesBySid = bySid
      , runtimeHistory = Scene.historyAt loc
      , runtimeStates = Map.empty
      , runtimePrevActive = []
      , runtimePrevLocs = Map.empty
      }

navigate :: Nav -> Runtime st msg out paths -> Runtime st msg out paths
navigate nav rt =
  let h0 = runtimeHistory rt
      goto mode pathText maybeSearch =
        case resolvePathMaybe rt pathText of
          Nothing -> h0
          Just loc ->
            let loc1 =
                  case maybeSearch of
                    Just searchMap ->
                      loc
                        { Scene.locationSearchParams = searchMap
                        }
                    Nothing -> loc
            in Scene.gotoAt mode loc1 h0
      h1 =
        case nav of
          Push pathText ->
            goto Scene.Push pathText Nothing
          Replace pathText ->
            goto Scene.Replace pathText Nothing
          PushWith pathText searchMap ->
            goto Scene.Push pathText (Just searchMap)
          ReplaceWith pathText searchMap ->
            goto Scene.Replace pathText (Just searchMap)
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
  Runtime st msg out paths ->
  (Runtime st msg out paths, [out], [Nav])
step dt inbox rt0 =
  let currentLoc = Scene.current (runtimeHistory rt0)
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
      (statesAfterExit, outsAfterExit, navsAfterExit) =
        foldl'
          (runExitedEntry rt0 dt inbox routeEvents)
          (runtimeStates rt0, [], [])
          exitedOrder
      (statesAfterActive, outsAfterActive, navsAfterActive, nextLocs) =
        foldl'
          (runActiveEntry rt0 dt inbox routeEvents)
          (statesAfterExit, outsAfterExit, navsAfterExit, Map.empty)
          currentLocs
      rt1 =
        rt0
          { runtimeStates = statesAfterActive
          , runtimePrevActive = currentSegs
          , runtimePrevLocs = nextLocs
          }
  in (rt1, outsAfterActive, navsAfterActive)

runExitedEntry ::
  Runtime st msg out paths ->
  DTime ->
  [msg] ->
  (String -> [RouteEvent]) ->
  (Map String st, [out], [Nav]) ->
  String ->
  (Map String st, [out], [Nav])
runExitedEntry rt dt inbox routeEvents (states, outs, navs) sid =
  case Map.lookup sid (runtimePrevLocs rt) of
    Nothing ->
      (states, outs, navs)
    Just oldLoc ->
      case runStepAt rt sid oldLoc (routeEvents sid) dt inbox states of
        Nothing ->
          (states, outs, navs)
        Just (states1, outs1, navs1, _) ->
          (states1, outs <> outs1, navs <> navs1)

runActiveEntry ::
  Runtime st msg out paths ->
  DTime ->
  [msg] ->
  (String -> [RouteEvent]) ->
  (Map String st, [out], [Nav], Map String (Scene.Location String)) ->
  (String, Scene.Location String) ->
  (Map String st, [out], [Nav], Map String (Scene.Location String))
runActiveEntry rt dt inbox routeEvents (states, outs, navs, locs) (sid, loc) =
  case runStepAt rt sid loc (routeEvents sid) dt inbox states of
    Nothing ->
      (states, outs, navs, locs)
    Just (states1, outs1, navs1, resolvedLoc) ->
      ( states1
      , outs <> outs1
      , navs <> navs1
      , Map.insert sid resolvedLoc locs
      )

runStepAt ::
  Runtime st msg out paths ->
  String ->
  Scene.Location String ->
  [RouteEvent] ->
  DTime ->
  [msg] ->
  Map String st ->
  Maybe (Map String st, [out], [Nav], Scene.Location String)
runStepAt rt sid loc events dt inbox states0 = do
  entry <- Map.lookup sid (runtimeEntriesBySid rt)
  runStepEntry sid loc events dt inbox states0 entry

runStepEntry ::
  String ->
  Scene.Location String ->
  [RouteEvent] ->
  DTime ->
  [msg] ->
  Map String st ->
  Entry st msg out ->
  Maybe (Map String st, [out], [Nav], Scene.Location String)
runStepEntry sid loc events dt inbox states0 (Entry routeMeta _ expectedSegments initFn stepFn defaults) =
  if Scene.locationSegments loc /= expectedSegments
    then Nothing
    else
      let urlKeys = Core.routeUrlParamKeys routeMeta
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
      in
        if not (sameKeyList searchKeys searchMap)
            || not (sameKeyList urlKeys urlMap)
          then Nothing
          else do
            searchParams <- Core.decodeSearchCodec searchMap
            let routeParams = Params urlMap
                state0 = Map.findWithDefault (initFn routeParams searchParams) sid states0
                ctx =
                  Ctx
                    { ctxParams = routeParams
                    , ctxSearch = searchParams
                    , ctxEvents = events
                    }
                (state1, outs1, navs1) = stepFn ctx dt inbox state0
                resolvedLoc =
                  Scene.Location
                    { Scene.locationSegments = expectedSegments
                    , Scene.locationSearchParams = searchMap
                    , Scene.locationUrlParams = urlMap
                    }
            pure
              ( Map.insert sid state1 states0
              , outs1
              , navs1
              , resolvedLoc
              )

resolvePathMaybe :: Runtime st msg out paths -> String -> Maybe (Scene.Location String)
resolvePathMaybe rt pathText =
  case resolvePath (runtimeCompiled rt) (runtimeEntriesBySid rt) pathText of
    Right loc -> Just loc
    Left _ -> Nothing

resolvePath ::
  Core.CompiledTree () () String ->
  Map String (Entry st msg out) ->
  String ->
  Either String (Scene.Location String)
resolvePath compiled bySid pathText = do
  loc0 <-
    case Core.resolveCompiledPath compiled pathText of
      Just loc -> Right loc
      Nothing -> Left ("router navigate: no route for path " <> pathText)
  sid <-
    case lastMaybe (Scene.locationSegments loc0) of
      Just sid' -> Right sid'
      Nothing -> Left ("router navigate: route did not produce a leaf for path " <> pathText)
  entry <-
    case Map.lookup sid bySid of
      Just found -> Right found
      Nothing -> Left ("router navigate: unknown route leaf " <> show sid)
  let loc1 =
        loc0
          { Scene.locationSegments = entryStackOf entry
          }
  Right loc1

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

sameKeyList :: [String] -> Map String a -> Bool
sameKeyList keys actualMap =
  Set.fromList keys == Map.keysSet actualMap

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

duplicates :: Ord a => [a] -> [a]
duplicates xs =
  [key | (key, count) <- Map.toList (Map.fromListWith (+) [(x, 1 :: Int) | x <- xs]), count > 1]

validateUniqueEntries :: [Entry st msg out] -> Either String ()
validateUniqueEntries entries = do
  let patterns = map entryPattern entries
      duplicatePatterns = duplicates patterns
      leaves = [sid | Entry _ sid _ _ _ _ <- entries]
      duplicateLeaves = duplicates leaves
  if null duplicatePatterns
    then Right ()
    else Left ("router create: duplicate route patterns " <> show duplicatePatterns)
  if null duplicateLeaves
    then Right ()
    else Left ("router create: duplicate route leaves " <> show duplicateLeaves)
