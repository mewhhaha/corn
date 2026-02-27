module Engine.Data.Scene
  ( SceneStack
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
  , Location(..)
  , path
  , withSearchParam
  , withUrlParam
  , GotoMode(..)
  , History
  , history
  , historyAt
  , current
  , canGoBack
  , canGoForward
  , gotoAt
  , goto
  , back
  , forward
  , SceneRuntime(..)
  , mkScene
  , runScene
  ) where

import Prelude

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Engine.Data.ECS as E
import Engine.Data.FRP (DTime, Events)
import qualified Engine.Data.Program as S

newtype SceneStack a = SceneStack
  { unSceneStack :: [a]
  } deriving (Eq, Show)

emptyStack :: SceneStack a
emptyStack = SceneStack []

singletonStack :: a -> SceneStack a
singletonStack a = SceneStack [a]

stackFromList :: [a] -> SceneStack a
stackFromList = SceneStack

stackToList :: SceneStack a -> [a]
stackToList = unSceneStack

stackNull :: SceneStack a -> Bool
stackNull (SceneStack xs) = null xs

push :: a -> SceneStack a -> SceneStack a
push a (SceneStack xs) = SceneStack (a : xs)

pop :: SceneStack a -> Maybe (a, SceneStack a)
pop (SceneStack xs) =
  case xs of
    [] -> Nothing
    y : ys -> Just (y, SceneStack ys)

peek :: SceneStack a -> Maybe a
peek (SceneStack xs) =
  case xs of
    [] -> Nothing
    y : _ -> Just y

type SearchParams = Map String [String]

type UrlParams = Map String String

data Location sid = Location
  { locationSegments :: ![sid]
  , locationSearchParams :: !SearchParams
  , locationUrlParams :: !UrlParams
  } deriving (Eq, Show)

path :: [sid] -> Location sid
path segments = Location segments Map.empty Map.empty

withSearchParam :: String -> [String] -> Location sid -> Location sid
withSearchParam key values loc =
  loc
    { locationSearchParams =
        Map.insert key values (locationSearchParams loc)
    }

withUrlParam :: String -> String -> Location sid -> Location sid
withUrlParam key value loc =
  loc
    { locationUrlParams =
        Map.insert key value (locationUrlParams loc)
    }

data GotoMode
  = Push
  | Replace
  deriving (Eq, Show)

data History sid = History
  { hCurrent :: !(Location sid)
  , hBackStack :: ![Location sid]
  , hForwardStack :: ![Location sid]
  } deriving (Eq, Show)

history :: [sid] -> History sid
history = historyAt . path

historyAt :: Location sid -> History sid
historyAt loc =
  History
    { hCurrent = loc
    , hBackStack = []
    , hForwardStack = []
    }

current :: History sid -> Location sid
current = hCurrent

canGoBack :: History sid -> Bool
canGoBack h = not (null (hBackStack h))

canGoForward :: History sid -> Bool
canGoForward h = not (null (hForwardStack h))

gotoLocation :: Eq sid => GotoMode -> Location sid -> History sid -> History sid
gotoLocation mode next h =
  if next == hCurrent h
    then h
    else
      case mode of
        Push ->
          History
            { hCurrent = next
            , hBackStack = hCurrent h : hBackStack h
            , hForwardStack = []
            }
        Replace ->
          History
            { hCurrent = next
            , hBackStack = hBackStack h
            , hForwardStack = []
            }

gotoAt :: Eq sid => GotoMode -> Location sid -> History sid -> History sid
gotoAt = gotoLocation

goto :: Eq sid => GotoMode -> sid -> History sid -> History sid
goto mode segment h =
  gotoLocation mode (path [segment]) h

back :: History sid -> History sid
back h =
  case hBackStack h of
    [] -> h
    prev : rest ->
      History
        { hCurrent = prev
        , hBackStack = rest
        , hForwardStack = hCurrent h : hForwardStack h
        }

forward :: History sid -> History sid
forward h =
  case hForwardStack h of
    [] -> h
    next : rest ->
      History
        { hCurrent = next
        , hBackStack = hCurrent h : hBackStack h
        , hForwardStack = rest
        }

data SceneRuntime c msg = SceneRuntime
  { sceneRuntimeWorld :: !(E.World c)
  , sceneRuntimeGraph :: !(S.Graph c msg)
  }

mkScene :: E.World c -> S.Graph c msg -> SceneRuntime c msg
mkScene = SceneRuntime

runScene :: DTime -> Events msg -> SceneRuntime c msg -> (SceneRuntime c msg, Events msg)
runScene dt inbox rt0 =
  let (w1, out, g1) = S.run dt (sceneRuntimeWorld rt0) inbox (sceneRuntimeGraph rt0)
  in (SceneRuntime w1 g1, out)
