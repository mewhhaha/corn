{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Examples.HordeSurvival
  ( runConcept
  ) where

import GHC.Generics (Generic)
import qualified Engine.Data.ECS as E
import qualified Engine.Data.Program as S

data PlayerTag = PlayerTag
  deriving (Eq, Show)

data MobTag = MobTag
  deriving (Eq, Show)

data Pos = Pos !Double !Double
  deriving (Eq, Show)

data Vel = Vel !Double !Double
  deriving (Eq, Show)

newtype Hp = Hp Int
  deriving (Eq, Show)

data C
  = CPlayerTag PlayerTag
  | CMobTag MobTag
  | CPos Pos
  | CVel Vel
  | CHp Hp
  deriving (Generic)

instance E.ComponentId C

data PlayerRow = PlayerRow
  { prPlayerTag :: PlayerTag
  , prPos :: Pos
  , prVel :: Vel
  , prHp :: Hp
  } deriving (Generic)

data MobRow = MobRow
  { mrMobTag :: MobTag
  , mrPos :: Pos
  , mrVel :: Vel
  , mrHp :: Hp
  } deriving (Generic)

type World = E.World C

type Graph msg = S.Graph C msg

type ProgramM msg a = S.ProgramM C msg a

frameDt :: Double
frameDt = 0.016

mobCount :: Int
mobCount = 120

buildWorld :: World
buildWorld =
  let w0 = E.emptyWorld
      (_, w1) = E.spawn (PlayerTag, Pos 0 0, Vel 0 0, Hp 120) w0
  in foldl' spawnMob w1 [0 .. mobCount - 1]
  where
    spawnMob w i =
      let angle = fromIntegral i * 0.35
          radius = 18 + fromIntegral (i `mod` 5)
          x = radius * cos angle
          y = radius * sin angle
          hp = 16 + (i `mod` 7)
          (_, w1) = E.spawn (MobTag, Pos x y, Vel 0 0, Hp hp) w
      in w1

hordeProg :: ProgramM () ()
hordeProg = do
  t <- S.time
  dt <- S.dt
  mobs <- S.await $ S.collect (E.query @MobRow @C)
  let px = 6 * cos (0.8 * t)
      py = 6 * sin (0.8 * t)
      playerPos = Pos px py
      playerVel = Vel (-4.8 * sin (0.8 * t)) (4.8 * cos (0.8 * t))
      contactRadius = 1.1
      hitCount =
        length
          [ ()
          | (_, MobRow _ (Pos mx my) _ _) <- mobs
          , let dx = mx - px
                dy = my - py
          , dx * dx + dy * dy <= contactRadius * contactRadius
          ]
      hitDamage = hitCount `quot` 12
  _ <- S.await $
    S.eachM @PlayerRow $ \(PlayerRow _ _ _ (Hp hp)) ->
      S.edit (S.set playerPos <> S.set playerVel <> S.set (Hp (max 0 (hp - hitDamage))))
  _ <- S.await $
    S.eachM @MobRow $ \(MobRow _ (Pos x y) _ (Hp hp)) -> do
      let dx = px - x
          dy = py - y
          dist = sqrt (dx * dx + dy * dy + 1.0e-9)
          speed = 8
          vx = speed * dx / dist
          vy = speed * dy / dist
          xNext = x + vx * dt
          yNext = y + vy * dt
          hpNext =
            if dist < 0.85
              then hp - 1
              else hp
      if hpNext <= 0
        then S.edit (S.set (Hp 0) <> S.del @MobTag <> S.del @Vel)
        else S.edit (S.set (Pos xNext yNext) <> S.set (Vel vx vy) <> S.set (Hp hpNext))
  pure ()

graph0 :: Graph ()
graph0 = S.graph $ do
  _ <- S.program hordeProg
  pure ()

stepFrame :: (World, Graph ()) -> (World, Graph ())
stepFrame (w0, g0) =
  let (w1, _, g1) = S.run frameDt w0 [] g0
  in (w1, g1)

playerSummary :: World -> (Int, Pos)
playerSummary w =
  case E.runq (E.query @PlayerRow @C) w of
    (_, PlayerRow _ pos _ (Hp hp)) : _ -> (hp, pos)
    [] -> (0, Pos 0 0)

aliveMobPositions :: World -> [Pos]
aliveMobPositions w =
  [ pos
  | (_, MobRow _ pos _ _) <- E.runq (E.query @MobRow @C) w
  ]

distanceTo :: Pos -> Pos -> Double
distanceTo (Pos x0 y0) (Pos x1 y1) =
  let dx = x1 - x0
      dy = y1 - y0
  in sqrt (dx * dx + dy * dy)

runConcept :: IO ()
runConcept = do
  let (wFinal, _) = iterate stepFrame (buildWorld, graph0) !! 180
      (playerHp, playerPos) = playerSummary wFinal
      mobs = aliveMobPositions wFinal
      alive = length mobs
      nearestDist =
        case mobs of
          [] -> 0
          p0 : ps ->
            foldl'
              (\acc pos -> min acc (distanceTo playerPos pos))
              (distanceTo playerPos p0)
              ps
  putStrLn ("playerHp=" <> show playerHp <> ", aliveMobs=" <> show alive <> ", nearestMobDist=" <> show nearestDist)
