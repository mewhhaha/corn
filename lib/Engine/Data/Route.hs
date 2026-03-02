{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Engine.Data.Route
  ( SearchParams
  , UrlParams
  , Meta(..)
  , Route(..)
  , AnyRoute(..)
  , RouteTree(..)
  , SomeRouter
  , CompiledTree(..)
  , routeBranch
  , routeLeaf
  , routeBranchAt
  , routeLeafAt
  , sceneBranchAt
  , sceneLeafAt
  , compileTree
  , resolveCompiledPath
  , gotoCompiledPath
  , Router(RNil, (:>))
  , emptyRouter
  , addRoute
  , SearchCodec(..)
  , UrlCodec(..)
  , SceneHandler
  , route
  , routeWith
  , resolvePath
  , gotoPath
  , gotoRoute
  , matchRoute
  , currentRoute
  , onRouteAt
  , onRoute
  ) where

import Prelude

import Data.Char (isAlphaNum)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import Data.Proxy (Proxy(..))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Engine.Data.Scene as Scene
import GHC.Generics hiding (Meta)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Text.Read (readMaybe)

type SearchParams = Scene.SearchParams

type UrlParams = Scene.UrlParams

data Meta sid search url = Meta
  { routePattern :: String
  , routeSegments :: [sid]
  , routeLeafSegment :: Maybe sid
  , routeUrlParamKeys :: [String]
  , routeSearchParamKeys :: [String]
  } deriving (Eq, Show)

data Route c msg sid search url = Route
  { meta :: Meta sid search url
  , enter :: search -> url -> Scene.SceneRuntime c msg
  }

data AnyRoute c msg sid where
  AnyRoute ::
    (SearchCodec search, UrlCodec url) =>
    Route c msg sid search url ->
    AnyRoute c msg sid

data RouteTree c msg sid = RouteTree
  { treeRoute :: AnyRoute c msg sid
  , treeChildren :: [RouteTree c msg sid]
  }

data SomeRouter sid where
  SomeRouter :: Router sid routes -> SomeRouter sid

data CompiledTree c msg sid = CompiledTree
  { compiledRoots :: [RouteTree c msg sid]
  , compiledRoutes :: [AnyRoute c msg sid]
  , compiledRouter :: SomeRouter sid
  }

routeBranch ::
  (SearchCodec search, UrlCodec url) =>
  Meta sid search url ->
  (search -> url -> Scene.SceneRuntime c msg) ->
  [RouteTree c msg sid] ->
  RouteTree c msg sid
routeBranch routeMeta mkScene children =
  RouteTree
    { treeRoute = AnyRoute (Route routeMeta mkScene)
    , treeChildren = children
    }

routeLeaf ::
  (SearchCodec search, UrlCodec url) =>
  Meta sid search url ->
  (search -> url -> Scene.SceneRuntime c msg) ->
  RouteTree c msg sid
routeLeaf routeMeta mkScene =
  routeBranch routeMeta mkScene []

routeBranchAt ::
  forall search url c msg.
  (SearchCodec search, UrlCodec url) =>
  String ->
  (search -> url -> Scene.SceneRuntime c msg) ->
  [RouteTree c msg String] ->
  Either String (RouteTree c msg String)
routeBranchAt patternText mkScene children = do
  routeMeta <- route @search @url patternText
  pure (routeBranch routeMeta mkScene children)

routeLeafAt ::
  forall search url c msg.
  (SearchCodec search, UrlCodec url) =>
  String ->
  (search -> url -> Scene.SceneRuntime c msg) ->
  Either String (RouteTree c msg String)
routeLeafAt patternText mkScene =
  routeBranchAt @search @url patternText mkScene []

sceneBranchAt ::
  String ->
  Scene.SceneRuntime c msg ->
  [RouteTree c msg String] ->
  Either String (RouteTree c msg String)
sceneBranchAt patternText sceneRt children =
  routeBranchAt @() @() patternText (\() () -> sceneRt) children

sceneLeafAt ::
  String ->
  Scene.SceneRuntime c msg ->
  Either String (RouteTree c msg String)
sceneLeafAt patternText sceneRt =
  sceneBranchAt patternText sceneRt []

data SomeMeta sid where
  SomeMeta ::
    (SearchCodec search, UrlCodec url) =>
    Meta sid search url ->
    SomeMeta sid

anyRouteMeta :: AnyRoute c msg sid -> SomeMeta sid
anyRouteMeta anyRoute =
  case anyRoute of
    AnyRoute routeEntry ->
      SomeMeta (meta routeEntry)

flattenTree :: [RouteTree c msg sid] -> [AnyRoute c msg sid]
flattenTree roots = concatMap go roots
  where
    go treeNode =
      treeRoute treeNode : flattenTree (treeChildren treeNode)

buildSomeRouter :: [SomeMeta sid] -> SomeRouter sid
buildSomeRouter metas =
  case metas of
    [] -> SomeRouter RNil
    SomeMeta routeMeta : rest ->
      case buildSomeRouter rest of
        SomeRouter routerRest ->
          SomeRouter (routeMeta :> routerRest)

compileTree :: [RouteTree c msg sid] -> CompiledTree c msg sid
compileTree roots =
  let routes = flattenTree roots
      metas = map anyRouteMeta routes
  in
    CompiledTree
      { compiledRoots = roots
      , compiledRoutes = routes
      , compiledRouter = buildSomeRouter metas
      }

resolveCompiledPath ::
  CompiledTree c msg String ->
  String ->
  Maybe (Scene.Location String)
resolveCompiledPath compiled pathText =
  case compiledRouter compiled of
    SomeRouter router ->
      resolvePath router pathText

gotoCompiledPath ::
  Scene.GotoMode ->
  String ->
  CompiledTree c msg String ->
  Scene.History String ->
  Scene.History String
gotoCompiledPath mode pathText compiled h =
  case compiledRouter compiled of
    SomeRouter router ->
      gotoPath mode pathText router h

data Router sid routes where
  RNil :: Router sid '[]
  (:>) ::
    (SearchCodec search, UrlCodec url) =>
    Meta sid search url ->
    Router sid routes ->
    Router sid (Meta sid search url ': routes)
infixr 5 :>

emptyRouter :: Router sid '[]
emptyRouter = RNil

addRoute ::
  (SearchCodec search, UrlCodec url) =>
  Meta sid search url ->
  Router sid routes ->
  Router sid (Meta sid search url ': routes)
addRoute = (:>)

class SearchCodec search where
  searchCodecKeys :: Proxy search -> [String]
  encodeSearchCodec :: search -> SearchParams
  decodeSearchCodec :: SearchParams -> Maybe search

  default searchCodecKeys :: (Generic search, GSearchCodec (Rep search)) => Proxy search -> [String]
  searchCodecKeys _ = gSearchCodecKeys (Proxy :: Proxy (Rep search))

  default encodeSearchCodec :: (Generic search, GSearchCodec (Rep search)) => search -> SearchParams
  encodeSearchCodec = gEncodeSearchCodec . from

  default decodeSearchCodec :: (Generic search, GSearchCodec (Rep search)) => SearchParams -> Maybe search
  decodeSearchCodec searchParams =
    if sameKeys (searchCodecKeys (Proxy :: Proxy search)) searchParams
      then to <$> gDecodeSearchCodec searchParams
      else Nothing

class UrlCodec url where
  urlCodecKeys :: Proxy url -> [String]
  encodeUrlCodec :: url -> UrlParams
  decodeUrlCodec :: UrlParams -> Maybe url

  default urlCodecKeys :: (Generic url, GUrlCodec (Rep url)) => Proxy url -> [String]
  urlCodecKeys _ = gUrlCodecKeys (Proxy :: Proxy (Rep url))

  default encodeUrlCodec :: (Generic url, GUrlCodec (Rep url)) => url -> UrlParams
  encodeUrlCodec = gEncodeUrlCodec . from

  default decodeUrlCodec :: (Generic url, GUrlCodec (Rep url)) => UrlParams -> Maybe url
  decodeUrlCodec urlParams =
    if sameKeys (urlCodecKeys (Proxy :: Proxy url)) urlParams
      then to <$> gDecodeUrlCodec urlParams
      else Nothing

class SearchFieldCodec a where
  encodeSearchFieldCodec :: a -> [String]
  decodeSearchFieldCodec :: [String] -> Maybe a

instance SearchFieldCodec [String] where
  encodeSearchFieldCodec = id
  decodeSearchFieldCodec = Just

instance SearchFieldCodec Int where
  encodeSearchFieldCodec n = [show n]
  decodeSearchFieldCodec values =
    case values of
      [v] -> readMaybe v
      _ -> Nothing

instance SearchFieldCodec Integer where
  encodeSearchFieldCodec n = [show n]
  decodeSearchFieldCodec values =
    case values of
      [v] -> readMaybe v
      _ -> Nothing

instance SearchFieldCodec Bool where
  encodeSearchFieldCodec b = [show b]
  decodeSearchFieldCodec values =
    case values of
      [v] -> readMaybe v
      _ -> Nothing

class UrlFieldCodec a where
  encodeUrlFieldCodec :: a -> String
  decodeUrlFieldCodec :: String -> Maybe a

instance UrlFieldCodec String where
  encodeUrlFieldCodec = id
  decodeUrlFieldCodec = Just

instance UrlFieldCodec Int where
  encodeUrlFieldCodec = show
  decodeUrlFieldCodec = readMaybe

instance UrlFieldCodec Integer where
  encodeUrlFieldCodec = show
  decodeUrlFieldCodec = readMaybe

instance UrlFieldCodec Bool where
  encodeUrlFieldCodec = show
  decodeUrlFieldCodec = readMaybe

class GSearchCodec f where
  gSearchCodecKeys :: Proxy f -> [String]
  gEncodeSearchCodec :: f p -> SearchParams
  gDecodeSearchCodec :: SearchParams -> Maybe (f p)

instance GSearchCodec U1 where
  gSearchCodecKeys _ = []
  gEncodeSearchCodec U1 = Map.empty
  gDecodeSearchCodec _ = Just U1

instance GSearchCodec f => GSearchCodec (M1 D meta f) where
  gSearchCodecKeys _ = gSearchCodecKeys (Proxy :: Proxy f)
  gEncodeSearchCodec (M1 x) = gEncodeSearchCodec x
  gDecodeSearchCodec m = M1 <$> gDecodeSearchCodec m

instance GSearchCodec f => GSearchCodec (M1 C meta f) where
  gSearchCodecKeys _ = gSearchCodecKeys (Proxy :: Proxy f)
  gEncodeSearchCodec (M1 x) = gEncodeSearchCodec x
  gDecodeSearchCodec m = M1 <$> gDecodeSearchCodec m

instance (GSearchCodec a, GSearchCodec b) => GSearchCodec (a :*: b) where
  gSearchCodecKeys _ =
    gSearchCodecKeys (Proxy :: Proxy a) <> gSearchCodecKeys (Proxy :: Proxy b)
  gEncodeSearchCodec (a :*: b) =
    Map.union (gEncodeSearchCodec a) (gEncodeSearchCodec b)
  gDecodeSearchCodec m = do
    a <- gDecodeSearchCodec m
    b <- gDecodeSearchCodec m
    pure (a :*: b)

instance
  (KnownSymbol name, SearchFieldCodec a) =>
  GSearchCodec (M1 S ('MetaSel ('Just name) su ss ds) (K1 i a)) where
  gSearchCodecKeys _ =
    [symbolVal (Proxy :: Proxy name)]
  gEncodeSearchCodec (M1 (K1 a)) =
    let key = symbolVal (Proxy :: Proxy name)
    in Map.singleton key (encodeSearchFieldCodec a)
  gDecodeSearchCodec m =
    let key = symbolVal (Proxy :: Proxy name)
    in do
      values <- Map.lookup key m
      decoded <- decodeSearchFieldCodec values
      pure (M1 (K1 decoded))

class GUrlCodec f where
  gUrlCodecKeys :: Proxy f -> [String]
  gEncodeUrlCodec :: f p -> UrlParams
  gDecodeUrlCodec :: UrlParams -> Maybe (f p)

instance GUrlCodec U1 where
  gUrlCodecKeys _ = []
  gEncodeUrlCodec U1 = Map.empty
  gDecodeUrlCodec _ = Just U1

instance GUrlCodec f => GUrlCodec (M1 D meta f) where
  gUrlCodecKeys _ = gUrlCodecKeys (Proxy :: Proxy f)
  gEncodeUrlCodec (M1 x) = gEncodeUrlCodec x
  gDecodeUrlCodec m = M1 <$> gDecodeUrlCodec m

instance GUrlCodec f => GUrlCodec (M1 C meta f) where
  gUrlCodecKeys _ = gUrlCodecKeys (Proxy :: Proxy f)
  gEncodeUrlCodec (M1 x) = gEncodeUrlCodec x
  gDecodeUrlCodec m = M1 <$> gDecodeUrlCodec m

instance (GUrlCodec a, GUrlCodec b) => GUrlCodec (a :*: b) where
  gUrlCodecKeys _ =
    gUrlCodecKeys (Proxy :: Proxy a) <> gUrlCodecKeys (Proxy :: Proxy b)
  gEncodeUrlCodec (a :*: b) =
    Map.union (gEncodeUrlCodec a) (gEncodeUrlCodec b)
  gDecodeUrlCodec m = do
    a <- gDecodeUrlCodec m
    b <- gDecodeUrlCodec m
    pure (a :*: b)

instance
  (KnownSymbol name, UrlFieldCodec a) =>
  GUrlCodec (M1 S ('MetaSel ('Just name) su ss ds) (K1 i a)) where
  gUrlCodecKeys _ =
    [symbolVal (Proxy :: Proxy name)]
  gEncodeUrlCodec (M1 (K1 a)) =
    let key = symbolVal (Proxy :: Proxy name)
    in Map.singleton key (encodeUrlFieldCodec a)
  gDecodeUrlCodec m =
    let key = symbolVal (Proxy :: Proxy name)
    in do
      value <- Map.lookup key m
      decoded <- decodeUrlFieldCodec value
      pure (M1 (K1 decoded))

instance SearchCodec () where
  searchCodecKeys _ = []
  encodeSearchCodec () = Map.empty
  decodeSearchCodec searchParams =
    if Map.null searchParams
      then Just ()
      else Nothing

instance UrlCodec () where
  urlCodecKeys _ = []
  encodeUrlCodec () = Map.empty
  decodeUrlCodec urlParams =
    if Map.null urlParams
      then Just ()
      else Nothing

instance UrlCodec UrlParams where
  urlCodecKeys _ = []
  encodeUrlCodec = id
  decodeUrlCodec = Just

data PatternToken
  = PatternLit String
  | PatternParam String

splitSlash :: String -> [String]
splitSlash s = go s []
  where
    go rest acc =
      case break (== '/') rest of
        (piece, "") ->
          let acc' = if null piece then acc else piece : acc
          in reverse acc'
        (piece, _ : rest') ->
          let acc' = if null piece then acc else piece : acc
          in go rest' acc'

isValidParamName :: String -> Bool
isValidParamName key =
  not (null key) && all (\c -> isAlphaNum c || c == '_') key

parseParamSegment :: String -> Either String (Maybe String)
parseParamSegment seg =
  case seg of
    ':' : key ->
      Right (Just key)
    '{' : ':' : rest ->
      case reverse rest of
        '}' : revKey -> Right (Just (reverse revKey))
        _ -> Left ("route: invalid param segment '" <> seg <> "'")
    _ ->
      Right Nothing

parsePatternToken :: String -> Either String PatternToken
parsePatternToken seg = do
  maybeKey <- parseParamSegment seg
  case maybeKey of
    Just key ->
      if isValidParamName key
        then Right (PatternParam key)
        else Left ("route: invalid param name :" <> key)
    Nothing ->
      if null seg
        then Left "route: empty path segment"
        else Right (PatternLit seg)

parsePattern :: String -> Either String [PatternToken]
parsePattern patternText =
  let segs = splitSlash patternText
  in traverse parsePatternToken segs

keysSet :: Map String a -> Set String
keysSet = Set.fromList . Map.keys

sameKeys :: [String] -> Map String a -> Bool
sameKeys expected m = Set.fromList expected == keysSet m

validateParamKeys :: [String] -> [String] -> Either String ()
validateParamKeys urlKeys searchParamKeys =
  let uniqueCount xs = Set.size (Set.fromList xs)
  in
    if uniqueCount urlKeys /= length urlKeys
      then Left "route: duplicate url param keys in pattern"
      else
        if uniqueCount searchParamKeys /= length searchParamKeys
          then Left "route: duplicate search param keys"
          else Right ()

validateCodecUrlKeys :: [String] -> [String] -> Either String ()
validateCodecUrlKeys patternUrlKeys codecUrlKeys =
  if null codecUrlKeys || Set.fromList patternUrlKeys == Set.fromList codecUrlKeys
    then Right ()
    else Left "route: url codec keys must match url params in pattern"

lastMaybe :: [a] -> Maybe a
lastMaybe xs =
  case reverse xs of
    [] -> Nothing
    y : _ -> Just y

urlKeysFromTokens :: [PatternToken] -> [String]
urlKeysFromTokens tokens =
  [key | PatternParam key <- tokens]

literalPathPrefixes :: [PatternToken] -> [String]
literalPathPrefixes tokens = reverse (go tokens [] [])
  where
    go toks revLits acc =
      case toks of
        [] -> acc
        token : rest ->
          case token of
            PatternLit lit ->
              let revLits' = lit : revLits
                  prefixPath = "/" <> intercalate "/" (reverse revLits')
              in go rest revLits' (prefixPath : acc)
            PatternParam _ ->
              go rest revLits acc

route ::
  forall search url.
  (SearchCodec search, UrlCodec url) =>
  String ->
  Either String (Meta String search url)
route = routeWith Just

routeWith ::
  forall sid search url.
  (SearchCodec search, UrlCodec url) =>
  (String -> Maybe sid) ->
  String ->
  Either String (Meta sid search url)
routeWith decodeSegment patternText = do
  tokens <- parsePattern patternText
  let prefixPaths = literalPathPrefixes tokens
      urlKeys = urlKeysFromTokens tokens
      searchKeys = searchCodecKeys (Proxy :: Proxy search)
      codecUrlKeys = urlCodecKeys (Proxy :: Proxy url)
  segs <- traverse decodePrefix prefixPaths
  validateParamKeys urlKeys searchKeys
  validateCodecUrlKeys urlKeys codecUrlKeys
  pure
    Meta
      { routePattern = patternText
      , routeSegments = segs
      , routeLeafSegment = lastMaybe segs
      , routeUrlParamKeys = urlKeys
      , routeSearchParamKeys = searchKeys
      }
  where
    decodePrefix prefix =
      case decodeSegment prefix of
        Nothing -> Left ("route: unknown path segment '" <> prefix <> "'")
        Just sid -> Right sid

specificityScore :: [PatternToken] -> (Int, Int)
specificityScore tokens =
  let literalCount = length [() | PatternLit _ <- tokens]
  in (literalCount, length tokens)

matchPatternTokens :: [PatternToken] -> [String] -> Maybe UrlParams
matchPatternTokens tokens segments =
  if length tokens /= length segments
    then Nothing
    else go tokens segments Map.empty
  where
    go toks segs acc =
      case (toks, segs) of
        ([], []) -> Just acc
        (PatternLit lit : restToks, seg : restSegs) ->
          if lit == seg
            then go restToks restSegs acc
            else Nothing
        (PatternParam key : restToks, seg : restSegs) ->
          go restToks restSegs (Map.insert key seg acc)
        _ ->
          Nothing

pickMoreSpecific ::
  Maybe ((Int, Int), Scene.Location String) ->
  Maybe ((Int, Int), Scene.Location String) ->
  Maybe ((Int, Int), Scene.Location String)
pickMoreSpecific currentBest candidate =
  case (currentBest, candidate) of
    (Nothing, x) -> x
    (x, Nothing) -> x
    (Just (bestScore, _), Just (candidateScore, _))
      | candidateScore > bestScore -> candidate
    _ -> currentBest

resolvePath ::
  Router String routes ->
  String ->
  Maybe (Scene.Location String)
resolvePath router pathText =
  snd <$> go router Nothing
  where
    inputSegs = splitSlash pathText

    go ::
      Router String routes' ->
      Maybe ((Int, Int), Scene.Location String) ->
      Maybe ((Int, Int), Scene.Location String)
    go r bestSoFar =
      case r of
        RNil ->
          bestSoFar
        (routeDef :: Meta String search url) :> rest ->
          let candidate = do
                tokens <- either (const Nothing) Just (parsePattern (routePattern routeDef))
                urlMap <- matchPatternTokens tokens inputSegs
                _ <- (decodeUrlCodec urlMap :: Maybe url)
                let loc =
                      Scene.Location
                        { Scene.locationSegments = resolvedRouteSegments router routeDef
                        , Scene.locationSearchParams = Map.empty
                        , Scene.locationUrlParams = urlMap
                        }
                pure (specificityScore tokens, loc)
          in go rest (pickMoreSpecific bestSoFar candidate)

gotoPath ::
  Scene.GotoMode ->
  String ->
  Router String routes ->
  Scene.History String ->
  Scene.History String
gotoPath mode pathText router h =
  case resolvePath router pathText of
    Nothing -> h
    Just loc -> Scene.gotoAt mode loc h

knownRouteLeafSegments :: Ord sid => Router sid routes -> Set sid
knownRouteLeafSegments router =
  case router of
    RNil -> Set.empty
    routeDef :> rest ->
      let leafSet =
            case routeLeafSegment routeDef of
              Nothing -> Set.empty
              Just sid -> Set.singleton sid
      in Set.union leafSet (knownRouteLeafSegments rest)

resolvedRouteSegments :: Ord sid => Router sid routes -> Meta sid search url -> [sid]
resolvedRouteSegments router routeDef =
  let knownLeaves = knownRouteLeafSegments router
      matched = filter (`Set.member` knownLeaves) (routeSegments routeDef)
  in
    if null matched
      then maybeToList (routeLeafSegment routeDef)
      else matched

locationForRoute ::
  (Ord sid, SearchCodec search, UrlCodec url) =>
  Router sid routes ->
  Meta sid search url ->
  search ->
  url ->
  Maybe (Scene.Location sid)
locationForRoute router routeDef searchParams urlParams =
  let searchMap = encodeSearchCodec searchParams
      urlMap = encodeUrlCodec urlParams
  in
    if sameKeys (routeSearchParamKeys routeDef) searchMap
        && sameKeys (routeUrlParamKeys routeDef) urlMap
      then
        Just
          Scene.Location
            { Scene.locationSegments = resolvedRouteSegments router routeDef
            , Scene.locationSearchParams = searchMap
            , Scene.locationUrlParams = urlMap
            }
      else Nothing

gotoRoute ::
  (Eq sid, Ord sid, SearchCodec search, UrlCodec url) =>
  Scene.GotoMode ->
  Meta sid search url ->
  search ->
  url ->
  Router sid routes ->
  Scene.History sid ->
  Scene.History sid
gotoRoute mode routeDef searchParams urlParams router h =
  case locationForRoute router routeDef searchParams urlParams of
    Nothing -> h
    Just loc -> Scene.gotoAt mode loc h

matchRoute ::
  (Eq sid, Ord sid, SearchCodec search, UrlCodec url) =>
  Meta sid search url ->
  Router sid routes ->
  Scene.Location sid ->
  Maybe (search, url)
matchRoute routeDef router loc =
  let expectedSegments = resolvedRouteSegments router routeDef
  in
    if Scene.locationSegments loc /= expectedSegments
      then Nothing
      else
        if not (sameKeys (routeSearchParamKeys routeDef) (Scene.locationSearchParams loc))
            || not (sameKeys (routeUrlParamKeys routeDef) (Scene.locationUrlParams loc))
          then Nothing
          else do
            searchParams <- decodeSearchCodec (Scene.locationSearchParams loc)
            urlParams <- decodeUrlCodec (Scene.locationUrlParams loc)
            pure (searchParams, urlParams)

currentRoute ::
  (Eq sid, Ord sid, SearchCodec search, UrlCodec url) =>
  Meta sid search url ->
  Router sid routes ->
  Scene.History sid ->
  Maybe (search, url)
currentRoute routeDef router h =
  matchRoute routeDef router (Scene.current h)

type SceneHandler search url a = search -> url -> a

onRouteAt ::
  (Eq sid, Ord sid, SearchCodec search, UrlCodec url) =>
  Meta sid search url ->
  Router sid routes ->
  SceneHandler search url a ->
  Scene.Location sid ->
  Maybe a
onRouteAt routeDef router sceneFn loc = do
  (searchParams, urlParams) <- matchRoute routeDef router loc
  pure (sceneFn searchParams urlParams)

onRoute ::
  (Eq sid, Ord sid, SearchCodec search, UrlCodec url) =>
  Meta sid search url ->
  Router sid routes ->
  SceneHandler search url a ->
  Scene.History sid ->
  Maybe a
onRoute routeDef router sceneFn h =
  onRouteAt routeDef router sceneFn (Scene.current h)

{-# DEPRECATED Meta "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED Route "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED AnyRoute "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED RouteTree "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED SomeRouter "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED CompiledTree "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED routeBranch "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED routeLeaf "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED routeBranchAt "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED routeLeafAt "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED sceneBranchAt "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED sceneLeafAt "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED compileTree "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED resolveCompiledPath "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED gotoCompiledPath "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED Router "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED emptyRouter "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED addRoute "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED UrlCodec "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED UrlFieldCodec "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED SearchFieldCodec "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED route "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED routeWith "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED resolvePath "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED gotoPath "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED gotoRoute "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED matchRoute "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED currentRoute "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED onRouteAt "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
{-# DEPRECATED onRoute "Engine.Data.Route is legacy. Use Engine.Data.Router for new routing code." #-}
