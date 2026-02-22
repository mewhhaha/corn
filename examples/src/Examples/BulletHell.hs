{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Examples.BulletHell
  ( runConcept
  ) where

import GHC.Generics (Generic)
import qualified Engine.Data.ECS as E
import qualified Engine.Data.FRP as F
import qualified Engine.Data.Program as S

data Emitter = Emitter
  deriving (Eq, Show)

data Bullet = Bullet
  deriving (Eq, Show)

data Pos = Pos !Double !Double
  deriving (Eq, Show)

data Vel = Vel !Double !Double
  deriving (Eq, Show)

newtype Life = Life Double
  deriving (Eq, Show)

data C
  = CEmitter Emitter
  | CBullet Bullet
  | CPos Pos
  | CVel Vel
  | CLife Life
  deriving (Generic)

instance E.ComponentId C

data EmitterRow = EmitterRow
  { erEmitter :: Emitter
  , erPos :: Pos
  } deriving (Generic)

data BulletRow = BulletRow
  { brBullet :: Bullet
  , brPos :: Pos
  , brVel :: Vel
  , brLife :: Life
  } deriving (Generic)

type World = E.World C

type Graph msg = S.Graph C msg

type ProgramM msg a = S.ProgramM C msg a

frameDt :: Double
frameDt = 0.016

spawnEvery :: Double
spawnEvery = 0.12

bulletSpeed :: Double
bulletSpeed = 22

bulletLifetime :: Double
bulletLifetime = 1.2

spawnBulletPatch :: Pos -> Vel -> S.Patch C
spawnBulletPatch pos vel =
  S.patch (\w0 -> snd (E.spawn (Bullet, pos, vel, Life bulletLifetime) (w0 :: World)))

spawnBurstPatch :: Pos -> S.Patch C
spawnBurstPatch pos =
  let directions = [Vel bulletSpeed 0, Vel (-bulletSpeed) 0, Vel 0 bulletSpeed, Vel 0 (-bulletSpeed)]
  in mconcat (map (spawnBulletPatch pos) directions)

buildWorld :: World
buildWorld =
  let w0 = E.emptyWorld
      (_, w1) = E.spawn (Emitter, Pos 0 0) w0
  in w1

bulletHellProg :: ProgramM () ()
bulletHellProg = do
  dt <- S.dt
  fireNow <- fmap (not . null) (S.step (F.every spawnEvery) ())
  emitters <- S.await $ S.collect (E.query @EmitterRow @C)
  let origin =
        case emitters of
          (_, EmitterRow _ pos) : _ -> pos
          [] -> Pos 0 0
  if fireNow
    then S.world (spawnBurstPatch origin)
    else pure ()
  _ <- S.await $
    S.eachM @BulletRow $ \(BulletRow _ (Pos x y) (Vel vx vy) (Life life)) -> do
      let lifeNext = life - dt
          posNext = Pos (x + vx * dt) (y + vy * dt)
      if lifeNext <= 0
        then S.edit (S.del @Bullet <> S.del @Vel <> S.del @Life)
        else S.edit (S.set posNext <> S.set (Life lifeNext))
  pure ()

graph0 :: Graph ()
graph0 = S.graph $ do
  _ <- S.program bulletHellProg
  pure ()

stepFrame :: (World, Graph ()) -> (World, Graph ())
stepFrame (w0, g0) =
  let (w1, _, g1) = S.run frameDt w0 [] g0
  in (w1, g1)

bulletCount :: World -> Int
bulletCount w = length (E.runq (E.query @Bullet @C) w)

runConcept :: IO ()
runConcept = do
  let frames = take 301 (iterate stepFrame (buildWorld, graph0))
      counts = map (bulletCount . fst) frames
      finalCount = bulletCount (fst (last frames))
      peakCount = foldl' max 0 counts
  putStrLn ("finalBullets=" <> show finalCount <> ", peakBullets=" <> show peakCount)
