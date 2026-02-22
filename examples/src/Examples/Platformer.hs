{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Examples.Platformer
  ( runConcept
  ) where

import GHC.Generics (Generic)
import qualified Engine.Data.ECS as E
import qualified Engine.Data.FRP as F
import qualified Engine.Data.Program as S

data Player = Player
  deriving (Eq, Show)

newtype Y = Y Double
  deriving (Eq, Show)

newtype Vy = Vy Double
  deriving (Eq, Show)

data C
  = CPlayer Player
  | CY Y
  | CVy Vy
  deriving (Generic)

instance E.ComponentId C

data PlayerRow = PlayerRow
  { prPlayer :: Player
  , prY :: Y
  , prVy :: Vy
  } deriving (Generic)

type World = E.World C

type Graph msg = S.Graph C msg

type ProgramM msg a = S.ProgramM C msg a

frameDt :: Double
frameDt = 0.016

gravity :: Double
gravity = -28

jumpImpulse :: Double
jumpImpulse = 11

jumpEvery :: Double
jumpEvery = 0.55

buildWorld :: World
buildWorld =
  let w0 = E.emptyWorld
      (_, w1) = E.spawn (Player, Y 0, Vy 0) w0
  in w1

platformerProg :: ProgramM () ()
platformerProg = do
  dt <- S.dt
  jumpNow <- fmap (not . null) (S.step (F.every jumpEvery) ())
  _ <- S.await $
    S.eachM @PlayerRow $ \(PlayerRow _ (Y y) (Vy vy)) -> do
      let vyFalling = vy + gravity * dt
          vyWithJump =
            if jumpNow && y <= 1.0e-6
              then jumpImpulse
              else vyFalling
          yRaw = y + vyWithJump * dt
          landed = yRaw <= 0
          yNext =
            if landed
              then 0
              else yRaw
          vyNext =
            if landed && vyWithJump < 0
              then 0
              else vyWithJump
      S.edit (S.set (Y yNext) <> S.set (Vy vyNext))
  pure ()

graph0 :: Graph ()
graph0 = S.graph $ do
  _ <- S.program platformerProg
  pure ()

stepFrame :: (World, Graph ()) -> (World, Graph ())
stepFrame (w0, g0) =
  let (w1, _, g1) = S.run frameDt w0 [] g0
  in (w1, g1)

playerState :: World -> (Double, Double)
playerState w =
  case E.runq (E.query @PlayerRow @C) w of
    (_, PlayerRow _ (Y y) (Vy vy)) : _ -> (y, vy)
    [] -> (0, 0)

runConcept :: IO ()
runConcept = do
  let frames = take 301 (iterate stepFrame (buildWorld, graph0))
      ys = map (fst . playerState . fst) frames
      (finalY, finalVy) = playerState (fst (last frames))
      peakY = foldl' max 0 ys
  putStrLn ("finalY=" <> show finalY <> ", finalVy=" <> show finalVy <> ", peakY=" <> show peakY)
