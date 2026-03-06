{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Examples.ScenesNavigation
  ( runConcept
  ) where

import Prelude

import Data.List (intercalate)
import qualified Engine.Corn.App as Route
import qualified Engine.Corn.App.Path as Path
import GHC.Generics (Generic)

data Msg
  = UiOpenOptions
  | UiCloseTop
  | UiStartGame
  | UiBackToMenu
  | UiForward
  deriving (Eq, Show)

newtype OptionsSearch = OptionsSearch
  { tab :: [String]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (Route.SearchCodec)

data SceneOut = SceneOut
  { outRoute :: !String
  , outTick :: !Int
  , outEvents :: ![Route.RouteEvent]
  } deriving (Eq, Show)

menuRoute :: Route.Layer "/main-menu" Int Msg SceneOut ()
menuRoute =
  Route.layer
    (\_ () -> 0)
    (\ctx _ inbox ticks0 ->
      let ticks1 = ticks0 + 1
          navs =
            [Route.Push "/main-menu/options" | UiOpenOptions `elem` inbox]
              <> [Route.Replace "/game" | UiStartGame `elem` inbox]
              <> [Route.Forward | UiForward `elem` inbox]
      in (ticks1, [SceneOut "/main-menu" ticks1 (Route.ctxEvents ctx)], navs)
    )
    (Just ())

optionsRoute :: Route.Layer "/main-menu/options" Int Msg SceneOut OptionsSearch
optionsRoute =
  Route.layer
    (\_ (OptionsSearch tabs) -> length tabs)
    (\ctx _ inbox ticks0 ->
      let ticks1 = ticks0 + 1
          navs = [Route.Back | UiCloseTop `elem` inbox]
      in (ticks1, [SceneOut "/main-menu/options" ticks1 (Route.ctxEvents ctx)], navs)
    )
    (Just (OptionsSearch ["audio"]))

gameRoute :: Route.Layer "/game" Int Msg SceneOut ()
gameRoute =
  Route.layer
    (\_ () -> 0)
    (\ctx _ inbox ticks0 ->
      let ticks1 = ticks0 + 1
          navs = [Route.Replace "/main-menu" | UiBackToMenu `elem` inbox]
      in (ticks1, [SceneOut "/game" ticks1 (Route.ctxEvents ctx)], navs)
    )
    (Just ())

routes :: Route.Routes Int Msg SceneOut '[ "/main-menu", "/main-menu/options", "/game" ]
routes =
  Route.stack menuRoute
    ( Route.leaf optionsRoute
        Route.:> Route.EmptyRoutes
    )
    Route.:> Route.leaf gameRoute
    Route.:> Route.EmptyRoutes

frameDt :: Double
frameDt = 0.016

scriptedInputs :: [[Msg]]
scriptedInputs =
  [ [UiOpenOptions]
  , [UiCloseTop]
  , [UiStartGame]
  , [UiBackToMenu]
  , [UiForward]
  ]

renderPath :: Route.Runtime Int Msg SceneOut paths -> String
renderPath runtime =
  let segs = Path.pathSegments (Route.current runtime)
  in unlines
      [ "path=" <> show segs
      , "path-render=" <> intercalate " > " segs
      , "canGoBack=" <> show (Route.canGoBack runtime) <> ", canGoForward=" <> show (Route.canGoForward runtime)
      ]

runConcept :: IO ()
runConcept =
  case Route.create routes "/main-menu" of
    Left err ->
      putStrLn ("router create failed: " <> err)
    Right rt0 ->
      go 1 rt0 scriptedInputs
  where
    go :: Int -> Route.Runtime Int Msg SceneOut '[ "/main-menu", "/main-menu/options", "/game" ] -> [[Msg]] -> IO ()
    go _ _ [] = pure ()
    go frameIx runtime0 (inputs : rest) = do
      let (runtime1, outs, navs) = Route.step frameDt inputs runtime0
          runtime2 = foldl' (flip Route.navigate) runtime1 navs
      putStrLn ("frame " <> show frameIx)
      putStrLn ("  inputs=" <> show inputs)
      putStrLn ("  outputs=" <> show outs)
      putStrLn ("  nav=" <> show navs)
      putStrLn ("  active(before-nav)=" <> show (Path.pathSegments (Route.current runtime1)))
      putStrLn ("  active(after-nav)=" <> show (Path.pathSegments (Route.current runtime2)))
      putStrLn (renderPath runtime2)
      go (frameIx + 1) runtime2 rest
