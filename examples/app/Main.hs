module Main where

import System.Environment (getArgs)
import qualified Examples.BulletHell as BulletHell
import qualified Examples.HordeSurvival as HordeSurvival
import qualified Examples.Platformer as Platformer
import qualified Examples.ScenesNavigation as ScenesNavigation

type Concept = (String, IO ())

concepts :: [Concept]
concepts =
  [ ("platformer", Platformer.runConcept)
  , ("horde-survival", HordeSurvival.runConcept)
  , ("bullet-hell", BulletHell.runConcept)
  , ("scenes-navigation", ScenesNavigation.runConcept)
  ]

runOne :: Concept -> IO ()
runOne (name, action) = do
  putStrLn ("== " <> name <> " ==")
  action

runNamed :: String -> IO ()
runNamed name =
  case lookup name concepts of
    Just action -> runOne (name, action)
    Nothing -> usage

usage :: IO ()
usage = do
  putStrLn "corn-examples: run small genre concepts with the corn engine."
  putStrLn "Usage:"
  putStrLn "  cabal run corn-examples -- all"
  putStrLn "  cabal run corn-examples -- <concept>"
  putStrLn "Concepts:"
  mapM_ (putStrLn . ("  - " <>) . fst) concepts

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> mapM_ runOne concepts
    ["all"] -> mapM_ runOne concepts
    [name] -> runNamed name
    _ -> usage
