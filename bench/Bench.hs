{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main
import Data.Bits (bit, (.|.))
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Foldable as Foldable
import qualified Engine.Corn as S
import qualified Engine.Corn.App as RouteS
import qualified Engine.Corn.App.Path as Path
import qualified Engine.Corn.FRP as F
import qualified Engine.Corn.World as E
import GHC.Generics (Generic)

data Pos = Pos {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Show)

data Vel = Vel {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Show)

data Acc = Acc {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Show)

newtype Hp = Hp Int
  deriving (Eq, Show)

data UiMsg
  = Hover Int
  | Focus Bool
  | Noise
  deriving (Eq, Show)

data C
  = CPos Pos
  | CVel Vel
  | CAcc Acc
  | CHp Hp
  deriving (Generic)

instance E.ComponentId C

type World = E.World C

type ProgramM msg a = S.ProgramM C msg a

type Graph msg = S.Graph C msg

newtype BenchSearch = BenchSearch
  { tab :: [String]
  } deriving (Eq, Show, Generic, RouteS.SearchCodec)

data SceneNode
  = SceneRoot
  | SceneMenu
  | SceneOptions
  deriving (Eq, Ord, Show)

batch :: S.Batch C String a -> S.Batch C String a
batch = id

batchUi :: S.Batch C UiMsg a -> S.Batch C UiMsg a
batchUi = id

reqPV :: E.Sig
reqPV =
  bit (E.componentBitOf @C @Pos)
    .|. bit (E.componentBitOf @C @Vel)

pongFieldHalfW :: Double
pongFieldHalfW = 50

pongFieldHalfH :: Double
pongFieldHalfH = 30

pongPaddleHalf :: Double
pongPaddleHalf = 8

pongPaddleSpeed :: Double
pongPaddleSpeed = 35

pongBallRadius :: Double
pongBallRadius = 1

pongPaddleX :: Double
pongPaddleX = pongFieldHalfW - 2

vampireFieldHalfW :: Double
vampireFieldHalfW = 120

vampireFieldHalfH :: Double
vampireFieldHalfH = 80

vampirePlayerSpeed :: Double
vampirePlayerSpeed = 30

vampireMobBaseSpeed :: Double
vampireMobBaseSpeed = 10

vampireDamageRadius :: Double
vampireDamageRadius = 4

forceEachm :: World -> Double
forceEachm w =
  Foldable.foldl'
    (\acc (E.EntityRow _ _ bag) ->
      let sumPos =
            case E.bagGet @C @Pos bag of
              Just (Pos x y) -> x + y
              Nothing -> 0
          sumVel =
            case E.bagGet @C @Vel bag of
              Just (Vel vx vy) -> vx + vy
              Nothing -> 0
      in acc + sumPos + sumVel
    )
    0
    (E.matchingRows reqPV 0 w)

clamp :: Double -> Double -> Double -> Double
clamp lo hi x = max lo (min hi x)

data BallInfo = BallInfo
  { ballPos :: Pos
  , ballVel :: Vel
  , ballAcc :: Acc
  , ballNoHp :: E.Not Hp
  } deriving (Generic)

data PaddleInfo = PaddleInfo
  { paddlePos :: Pos
  , paddleVel :: Vel
  , paddleHp :: Hp
  , paddleNoAcc :: E.Not Acc
  } deriving (Generic)

data PlayerInfo = PlayerInfo
  { playerPos :: Pos
  , playerVel :: Vel
  , playerHp :: Hp
  , playerNoAcc :: E.Not Acc
  } deriving (Generic)

data NpcInfo = NpcInfo
  { npcPos :: Pos
  , npcVel :: Vel
  , npcAcc :: Acc
  , npcHp :: Hp
  } deriving (Generic)

buildWorldPong :: World
buildWorldPong =
  let w0 = E.emptyWorld
      (_, w1) = E.spawn (Pos 0 0, Vel 35 12, Acc 0 0) w0
      (_, w2) = E.spawn (Pos (-pongPaddleX) 0, Vel 0 0, Hp (-1)) w1
      (_, w3) = E.spawn (Pos pongPaddleX 0, Vel 0 0, Hp 1) w2
  in w3

buildWorldVampire :: Int -> World
buildWorldVampire mobCount =
  let w0 = E.emptyWorld
      (_, w1) = E.spawn (Pos 0 0, Vel vampirePlayerSpeed 12, Hp 100) w0
      addMob (i, w) _ =
        let x = fromIntegral (i `mod` 200) - 100
            y = fromIntegral (i `div` 200) - 25
            speed = fromIntegral ((i `mod` 7) + 4)
            hp = 5 + (i `mod` 9)
            (_, w1') = E.spawn (Pos x y, Vel 0 0, Acc speed 0, Hp hp) w
        in (i + 1, w1')
  in snd (foldl' addMob (0 :: Int, w1) [1 .. mobCount])

qBall :: E.Query C BallInfo
qBall = E.query @BallInfo @C

qPaddle :: E.Query C PaddleInfo
qPaddle = E.query @PaddleInfo @C

qPlayer :: E.Query C PlayerInfo
qPlayer = E.query @PlayerInfo @C

qMob :: E.Query C NpcInfo
qMob = E.query @NpcInfo @C

advanceVampirePlayer :: Double -> Pos -> Vel -> (Pos, Vel)
advanceVampirePlayer dt (Pos x y) (Vel vx vy) =
  let x1 = x + vx * dt
      y1 = y + vy * dt
      (vx', x2) = clampX x1 vx
      (vy', y2) = clampY y1 vy
      clampX pos v
        | pos > vampireFieldHalfW = (-abs v, vampireFieldHalfW)
        | pos < -vampireFieldHalfW = (abs v, -vampireFieldHalfW)
        | otherwise = (v, pos)
      clampY pos v
        | pos > vampireFieldHalfH = (-abs v, vampireFieldHalfH)
        | pos < -vampireFieldHalfH = (abs v, -vampireFieldHalfH)
        | otherwise = (v, pos)
  in (Pos x2 y2, Vel vx' vy')

advanceVampireMob :: Double -> Pos -> Vel -> Acc -> Pos -> Hp -> (Pos, Vel, Hp)
advanceVampireMob dt (Pos x y) _ (Acc ax _) (Pos px py) (Hp hp) =
  let dx = px - x
      dy = py - y
      dist2 = dx * dx + dy * dy + 1.0e-6
      invDist = 1 / sqrt dist2
      speed = vampireMobBaseSpeed + ax
      vx' = dx * invDist * speed
      vy' = dy * invDist * speed
      x' = x + vx' * dt
      y' = y + vy' * dt
      hp' =
        if dist2 < vampireDamageRadius * vampireDamageRadius
          then hp - 1
          else hp
  in (Pos x' y', Vel vx' vy', Hp hp')

pongProg :: ProgramM String ()
pongProg = do
  dt <- S.dt
  paddles <- S.await $ batch $ S.collect qPaddle
  balls <- S.await $ batch $ S.collect qBall
  let (leftY, leftVy, rightY, rightVy) =
        foldl'
          (\(ly, lvy, ry, rvy) (_, PaddleInfo (Pos _ py) (Vel _ pvy) (Hp side) _) ->
            if side < 0
              then (py, pvy, ry, rvy)
              else (ly, lvy, py, pvy)
          )
          (0, 0, 0, 0)
          paddles
      ballY =
        case balls of
          (_, BallInfo (Pos _ y) _ _ _) : _ -> y
          _ -> 0
      maxDy = pongPaddleSpeed * dt
      minY = -pongFieldHalfH + pongPaddleHalf
      maxY = pongFieldHalfH - pongPaddleHalf
  _ <- S.await $ batch $
    S.eachM @PaddleInfo $ \(PaddleInfo (Pos _ y) _ (Hp side) _) -> do
      let targetY = ballY
          dy = clamp (-maxDy) maxDy (targetY - y)
          y' = clamp minY maxY (y + dy)
          x' = if side < 0 then -pongPaddleX else pongPaddleX
          vy' = if dt > 0 then dy / dt else 0
      S.edit (S.set (Pos x' y') <> S.set (Vel 0 vy'))
  _ <- S.await $ batch $
    S.eachM @BallInfo $ \(BallInfo (Pos x y) (Vel vx vy) _ _) -> do
      let x1 = x + vx * dt
          y1 = y + vy * dt
          (vyWall, yWall) = clampWall y1 vy
          hitLeft = x1 <= (-pongPaddleX + pongBallRadius)
            && abs (yWall - leftY) <= pongPaddleHalf
          hitRight = x1 >= (pongPaddleX - pongBallRadius)
            && abs (yWall - rightY) <= pongPaddleHalf
          vxP
            | hitLeft = abs vx
            | hitRight = -abs vx
            | otherwise = vx
          vyP
            | hitLeft = vyWall + leftVy * 0.2
            | hitRight = vyWall + rightVy * 0.2
            | otherwise = vyWall
          xP
            | hitLeft = -pongPaddleX + pongBallRadius
            | hitRight = pongPaddleX - pongBallRadius
            | otherwise = x1
          (vx2, x2) = clampXWall xP vxP
          clampWall pos v
            | pos > pongFieldHalfH - pongBallRadius = (-abs v, pongFieldHalfH - pongBallRadius)
            | pos < -pongFieldHalfH + pongBallRadius = (abs v, -pongFieldHalfH + pongBallRadius)
            | otherwise = (v, pos)
          clampXWall pos v
            | pos > pongFieldHalfW - pongBallRadius = (-abs v, pongFieldHalfW - pongBallRadius)
            | pos < -pongFieldHalfW + pongBallRadius = (abs v, -pongFieldHalfW + pongBallRadius)
            | otherwise = (v, pos)
      S.edit (S.set (Pos x2 yWall) <> S.set (Vel vx2 vyP))
  pure ()

vampireProg :: ProgramM String ()
vampireProg = do
  dt <- S.dt
  players <- S.await $ batch $ S.collect qPlayer
  let (p0, v0) =
        case players of
          (_, PlayerInfo p v _ _) : _ -> (p, v)
          _ -> (Pos 0 0, Vel vampirePlayerSpeed 12)
      (p1, v1) = advanceVampirePlayer dt p0 v0
  _ <- S.await $ batch $
    S.eachM @PlayerInfo $ \_ -> do
      S.edit (S.set p1 <> S.set v1)
  _ <- S.await $ batch $
    S.eachM @NpcInfo $ \(NpcInfo p v a hp) -> do
      let (p', v', hp') = advanceVampireMob dt p v a p1 hp
      S.edit (S.set p' <> S.set v' <> S.set hp')
  pure ()

graphPong :: Graph String
graphPong =
  S.graph $ do
    _ <- S.program pongProg
    pure ()

graphVampire :: Graph String
graphVampire =
  S.graph $ do
    _ <- S.program vampireProg
    pure ()

pickHover :: UiMsg -> Maybe Int
pickHover msg =
  case msg of
    Hover n -> Just n
    _ -> Nothing

pickFocus :: UiMsg -> Maybe Bool
pickFocus msg =
  case msg of
    Focus ok -> Just ok
    _ -> Nothing

stickyInlineProg :: ProgramM UiMsg ()
stickyInlineProg = do
  score <- S.await $
    (\hoverId focused -> if focused then hoverId else hoverId * 10)
      <$> S.sticky pickHover
      <*> S.sticky pickFocus
  _ <- S.await $ batchUi $
    S.each @Hp $ \_ ->
      S.set (Hp score)
  pure ()

graphStickyInline :: Graph UiMsg
graphStickyInline =
  S.graph $ do
    _ <- S.program stickyInlineProg
    pure ()

runStickyInline :: World -> Int
runStickyInline w0 =
  let (_, w1) = E.spawn (Hp 0) w0
      (w2, _, g1) = S.run 0.016 w1 [Hover 4, Noise] graphStickyInline
      (w3, _, _) = S.run 0.016 w2 [Focus False] g1
  in case E.getr @Hp w3 of
      Just (Hp n) -> n
      Nothing -> 0

frpBurstSize :: Int
frpBurstSize = 4096

mkAccBurstFns :: Int -> [Int -> Int]
mkAccBurstFns n = replicate n (+1)

mkHoldBurstEvents :: Int -> [Int]
mkHoldBurstEvents n = [1 .. n]

mkSwitchBurstEvents :: Int -> F.Events (F.Step () Int)
mkSwitchBurstEvents n = map pure [1 .. n]

runAccBurst :: Int -> Int
runAccBurst n =
  let fs = mkAccBurstFns n
      (out, _) = F.stepS (F.acc (0 :: Int)) 0.016 fs
  in out

runHoldBurst :: Int -> Int
runHoldBurst n =
  let evs = mkHoldBurstEvents n
      (out, _) = F.stepS (F.hold (0 :: Int)) 0.016 evs
  in out

runSwitchBurst :: Int -> Int
runSwitchBurst n =
  let evs = mkSwitchBurstEvents n
      source = pure (0, evs)
      (out, _) = F.stepS (F.switch source) 0.016 ()
  in out

runSceneHistoryNavCycle :: Int -> Int
runSceneHistoryNavCycle iters = go iters (Path.historyFrom [SceneRoot]) 0
  where
    go n h acc =
      if n <= 0
        then acc
        else
          let h1 = Path.gotoSegment Path.Push SceneMenu h
              h2 = Path.gotoSegment Path.Push SceneOptions h1
              h3 = Path.back h2
              h4 = Path.back h3
              h5 = Path.forward h4
              score =
                length (Path.pathSegments (Path.current h5))
                  + if Path.canGoBack h5 then 1 else 0
                  + if Path.canGoForward h5 then 1 else 0
          in go (n - 1) h5 (acc + score)

runScenePathGotoCycle :: Int -> Int
runScenePathGotoCycle iters = go iters (Path.history @'["/root"]) 0
  where
    go n h acc =
      if n <= 0
        then acc
        else
          let h1 = Path.goto @"/menu" Path.Replace h
              h2 = Path.goto @"/options" Path.Replace h1
              h3 = Path.goto @"/menu" Path.Replace h2
              segScore = length (Path.pathSegments (Path.current h3))
          in go (n - 1) h3 (acc + segScore)

runtimeBenchRoutes :: RouteS.Routes Int UiMsg UiMsg '[ "/layout", "/layout/items/{:id}" ]
runtimeBenchRoutes =
  RouteS.stack
    ( RouteS.layer
        @"/layout"
        (\_ () -> 0)
        (\_ _ _ n ->
          let n1 = n + 1
          in (n1, [Focus (odd n1)], [])
        )
        (Just ())
    )
    ( RouteS.leaf
        ( RouteS.layer
            @"/layout/items/{:id}"
            (\params (BenchSearch tabs) ->
              let item = RouteS.param @"id" params
              in item `seq` length tabs
            )
            (\_ _ _ n ->
              let n1 = n + 1
              in (n1, [Hover n1], [])
            )
            (Just (BenchSearch ["stats"]))
        )
        RouteS.:> RouteS.EmptyRoutes
    )
    RouteS.:> RouteS.EmptyRoutes

runSceneSimpleRouterEnter :: Int -> Int
runSceneSimpleRouterEnter iters =
  case RouteS.create runtimeBenchRoutes "/layout" of
    Left _ -> 0
    Right rt0 -> go iters rt0 0
  where
    go n rt acc =
      if n <= 0
        then acc
        else
          let rt1 =
                RouteS.navigate
                  (RouteS.ReplaceWith "/layout/items/42" (Map.fromList [("tab", ["stats"])]))
                  rt
              (rt2, out1, _) = RouteS.step 0.016 [] rt1
              rt3 = RouteS.navigate RouteS.Back rt2
              (rt4, out2, _) = RouteS.step 0.016 [] rt3
              score =
                length (Path.pathSegments (RouteS.current rt4))
                  + length out1
                  + length out2
          in go (n - 1) rt4 (acc + score)

runSceneRouterRuntimeStep :: Int -> Int
runSceneRouterRuntimeStep iters =
  case RouteS.create runtimeBenchRoutes "/layout" of
    Left _ -> 0
    Right rt0 -> go iters rt0 0
  where
    go n rt acc =
      if n <= 0
        then acc
        else
          let (rt1, out1, _) = RouteS.step 0.016 [] rt
              rt2 = RouteS.navigate (RouteS.Push "/layout/items/42") rt1
              (rt3, out2, _) = RouteS.step 0.016 [] rt2
              rt4 = RouteS.navigate RouteS.Back rt3
              (rt5, out3, _) = RouteS.step 0.016 [] rt4
              score =
                length (Path.pathSegments (RouteS.current rt5))
                  + length out1
                  + length out2
                  + length out3
                  + if RouteS.canGoForward rt5 then 1 else 0
          in go (n - 1) rt5 (acc + score)

data CornBenchMsg
  = CornOpen
  | CornClose
  | CornStart
  | CornBack
  deriving (Eq, Show)

data CornBenchModel = CornBenchModel
  { cornBenchMenuTicks :: !Int
  , cornBenchOptionsTicks :: !Int
  , cornBenchGameTicks :: !Int
  } deriving (Eq, Show)

cornBenchMenuScene :: RouteS.Layer "/main-menu" CornBenchModel CornBenchMsg Int ()
cornBenchMenuScene =
  RouteS.layer
    (const (const (CornBenchModel 0 0 0)))
    (\_ _ inbox model0 ->
      let model1 = model0 {cornBenchMenuTicks = cornBenchMenuTicks model0 + 1}
          cmds =
            [RouteS.Push "/main-menu/options" | CornOpen `elem` inbox]
              <> [RouteS.Push "/game" | CornStart `elem` inbox]
      in (model1, [1], cmds)
    )
    (Just ())

cornBenchOptionsScene :: RouteS.Layer "/main-menu/options" CornBenchModel CornBenchMsg Int ()
cornBenchOptionsScene =
  RouteS.layer
    (const (const (CornBenchModel 0 0 0)))
    (\_ _ inbox model0 ->
      let model1 = model0 {cornBenchOptionsTicks = cornBenchOptionsTicks model0 + 1}
          cmds = [RouteS.Back | CornClose `elem` inbox]
      in (model1, [1], cmds)
    )
    (Just ())

cornBenchGameScene :: RouteS.Layer "/game" CornBenchModel CornBenchMsg Int ()
cornBenchGameScene =
  RouteS.layer
    (const (const (CornBenchModel 0 0 0)))
    (\_ _ inbox model0 ->
      let model1 = model0 {cornBenchGameTicks = cornBenchGameTicks model0 + 1}
          cmds = [RouteS.Back | CornBack `elem` inbox]
      in (model1, [1], cmds)
    )
    (Just ())

cornBenchRoutes :: RouteS.Routes CornBenchModel CornBenchMsg Int '[ "/main-menu", "/main-menu/options", "/game" ]
cornBenchRoutes =
  RouteS.leaf cornBenchMenuScene
    RouteS.:> RouteS.leaf cornBenchOptionsScene
    RouteS.:> RouteS.leaf cornBenchGameScene
    RouteS.:> RouteS.EmptyRoutes

runSceneCornSimpleStep :: Int -> Int
runSceneCornSimpleStep iters =
  case RouteS.create cornBenchRoutes "/main-menu" of
    Left _ -> 0
    Right rt0 ->
      go iters rt0 0
  where
    go n rt acc =
      if n <= 0
        then acc
        else
          let (rt1, out1, _) = RouteS.step 0.016 [CornOpen] rt
              (rt2, out2, _) = RouteS.step 0.016 [CornClose] rt1
              (rt3, out3, _) = RouteS.step 0.016 [CornStart] rt2
              (rt4, out4, _) = RouteS.step 0.016 [CornBack] rt3
              score =
                length (RouteS.currentPath rt4)
                  + if RouteS.canGoBack rt4 then 1 else 0
                  + if RouteS.canGoForward rt4 then 1 else 0
                  + length out1
                  + length out2
                  + length out3
                  + length out4
          in go (n - 1) rt4 (acc + score)

main :: IO ()
main = defaultMain
  [ bgroup "game"
      [ let w = buildWorldPong
        in bench "rooftop-duel" $ nf (\w0 ->
            let (w1, _, _) = S.run 0.016 w0 [] graphPong
            in forceEachm w1
          ) w
      , let w = buildWorldVampire 10000
        in bench "flock-10k" $ nf (\w0 ->
            let (w1, _, _) = S.run 0.016 w0 [] graphVampire
            in forceEachm w1
          ) w
      ]
  , bgroup "program/10k"
      [ let w = buildWorldVampire 10000
        in bench "eachm" $ nf (\w0 ->
            let (w1, _, _) = S.run 0.016 w0 [] graphVampire
            in forceEachm w1
          ) w
      ]
  , bgroup "program/10k+1"
      [ let w = buildWorldVampire 10001
        in bench "eachm" $ nf (\w0 ->
            let (w1, _, _) = S.run 0.016 w0 [] graphVampire
            in forceEachm w1
          ) w
      ]
  , bgroup "program/sticky"
      [ let w = E.emptyWorld
        in bench "inline-await" $ nf runStickyInline w
      ]
  , bgroup "frp/burst"
      [ env (pure frpBurstSize) $ \n ->
          bench "acc" $ nf runAccBurst n
      , env (pure frpBurstSize) $ \n ->
          bench "hold" $ nf runHoldBurst n
      , env (pure frpBurstSize) $ \n ->
          bench "switch" $ nf runSwitchBurst n
      ]
  , bgroup "scene"
      [ bench "history/nav-cycle" $
          nf runSceneHistoryNavCycle (5000 :: Int)
      , bench "corn/simple-step" $
          nf runSceneCornSimpleStep (5000 :: Int)
      , bench "router/simple-enter" $
          nf runSceneSimpleRouterEnter (5000 :: Int)
      , bench "router/runtime-step" $
          nf runSceneRouterRuntimeStep (5000 :: Int)
      , bench "path/goto-cycle" $
          nf runScenePathGotoCycle (5000 :: Int)
      ]
  ]
