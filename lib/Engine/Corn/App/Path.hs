{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Engine.Corn.App.Path
  ( Stack
  , emptyStack
  , singletonStack
  , stackFromList
  , stackToList
  , stackNull
  , push
  , pop
  , peek
  , SearchParams
  , UrlParams
  , Path(..)
  , path
  , withSearchParam
  , withUrlParam
  , NavMode(..)
  , History
  , history
  , historyFrom
  , historyAt
  , current
  , canGoBack
  , canGoForward
  , gotoAt
  , goto
  , gotoSegment
  , back
  , forward
  ) where

import Prelude

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Engine.Data.Scene as Scene

type Stack = Scene.SceneStack
type SearchParams = Scene.SearchParams
type UrlParams = Scene.UrlParams
type History = Scene.History

data Path sid = Path
  { pathSegments :: ![sid]
  , pathSearchParams :: !SearchParams
  , pathUrlParams :: !UrlParams
  } deriving (Eq, Show)

data NavMode
  = Push
  | Replace
  deriving (Eq, Show)

toSceneMode :: NavMode -> Scene.GotoMode
toSceneMode mode =
  case mode of
    Push -> Scene.Push
    Replace -> Scene.Replace

class KnownSegments (segments :: [Symbol]) where
  knownSegments :: Proxy segments -> [String]

instance KnownSegments '[] where
  knownSegments _ = []

instance (KnownSymbol seg, KnownSegments rest) => KnownSegments (seg ': rest) where
  knownSegments _ =
    symbolVal (Proxy @seg) : knownSegments (Proxy @rest)

toScenePath :: Path sid -> Scene.Location sid
toScenePath path0 =
  Scene.Location
    { Scene.locationSegments = pathSegments path0
    , Scene.locationSearchParams = pathSearchParams path0
    , Scene.locationUrlParams = pathUrlParams path0
    }

fromScenePath :: Scene.Location sid -> Path sid
fromScenePath loc =
  Path
    { pathSegments = Scene.locationSegments loc
    , pathSearchParams = Scene.locationSearchParams loc
    , pathUrlParams = Scene.locationUrlParams loc
    }

emptyStack :: Stack a
emptyStack = Scene.emptyStack

singletonStack :: a -> Stack a
singletonStack = Scene.singletonStack

stackFromList :: [a] -> Stack a
stackFromList = Scene.stackFromList

stackToList :: Stack a -> [a]
stackToList = Scene.stackToList

stackNull :: Stack a -> Bool
stackNull = Scene.stackNull

push :: a -> Stack a -> Stack a
push = Scene.push

pop :: Stack a -> Maybe (a, Stack a)
pop = Scene.pop

peek :: Stack a -> Maybe a
peek = Scene.peek

path :: [sid] -> Path sid
path segments =
  Path
    { pathSegments = segments
    , pathSearchParams = mempty
    , pathUrlParams = mempty
    }

withSearchParam :: String -> [String] -> Path sid -> Path sid
withSearchParam key values path0 =
  path0
    { pathSearchParams = Scene.locationSearchParams (Scene.withSearchParam key values (toScenePath path0))
    }

withUrlParam :: String -> String -> Path sid -> Path sid
withUrlParam key value path0 =
  path0
    { pathUrlParams = Scene.locationUrlParams (Scene.withUrlParam key value (toScenePath path0))
    }

history :: forall segments. KnownSegments segments => History String
history = historyAt (path (knownSegments (Proxy @segments)))

historyFrom :: [sid] -> History sid
historyFrom = Scene.historyFrom

historyAt :: Path sid -> History sid
historyAt = Scene.historyAt . toScenePath

current :: History sid -> Path sid
current = fromScenePath . Scene.current

canGoBack :: History sid -> Bool
canGoBack = Scene.canGoBack

canGoForward :: History sid -> Bool
canGoForward = Scene.canGoForward

gotoAt :: Eq sid => NavMode -> Path sid -> History sid -> History sid
gotoAt mode path0 = Scene.gotoAt (toSceneMode mode) (toScenePath path0)

goto :: forall segment. KnownSymbol segment => NavMode -> History String -> History String
goto mode =
  gotoAt mode (path [symbolVal (Proxy @segment)])

gotoSegment :: Eq sid => NavMode -> sid -> History sid -> History sid
gotoSegment = Scene.gotoSegment . toSceneMode

back :: History sid -> History sid
back = Scene.back

forward :: History sid -> History sid
forward = Scene.forward
