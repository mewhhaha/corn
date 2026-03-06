{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (void)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import System.Environment (getArgs)
import qualified Engine.Corn as S
import qualified Engine.Corn.World as E
import qualified Examples.BulletHell as BulletHell
import qualified Examples.HordeSurvival as HordeSurvival
import qualified Examples.Platformer as Platformer
import qualified Examples.ScenesNavigation as ScenesNavigation

data Example = Example
  { name :: String
  , summary :: String
  , run :: IO ()
  }

data Upgrade
  = UpgradeRapidFire
  | UpgradeKnifeDamage
  | UpgradeMoveSpeed
  | UpgradeHolyAura
  | UpgradeMaxHp
  deriving (Eq, Ord, Show)

data EnemyKind
  = Shambler
  | Dasher
  | Orbiter
  deriving (Eq, Ord, Show)

data SimInput = SimInput
  { move :: Vec
  , upgradePrefs :: NonEmpty Upgrade
  } deriving (Eq, Show)

newtype FrameId = FrameId Int
  deriving (Eq, Ord, Show)

data SurvivorMsg
  = FrameStart FrameId SimInput
  | EnemyPhase FrameId SimInput
  | KnifePhase FrameId SimInput
  | FirePhase FrameId SimInput
  | CombatPhaseMsg FrameId SimInput
  | UpgradePhase FrameId SimInput
  deriving (Eq, Show)

data Vec = Vec !Double !Double
  deriving (Eq, Show)

newtype Pos = Pos Vec
  deriving (Eq, Show)

newtype Vel = Vel Vec
  deriving (Eq, Show)

newtype Hp = Hp Int
  deriving (Eq, Show)

newtype MaxHp = MaxHp Int
  deriving (Eq, Show)

newtype Xp = Xp Int
  deriving (Eq, Show)

newtype Level = Level Int
  deriving (Eq, Show)

newtype PendingUpgrade = PendingUpgrade Int
  deriving (Eq, Show)

newtype MoveSpeed = MoveSpeed Double
  deriving (Eq, Show)

newtype KnifeDamage = KnifeDamage Int
  deriving (Eq, Show)

newtype KnifeCooldown = KnifeCooldown Double
  deriving (Eq, Show)

newtype KnifeInterval = KnifeInterval Double
  deriving (Eq, Show)

newtype AuraRadius = AuraRadius Double
  deriving (Eq, Show)

newtype AuraDamage = AuraDamage Int
  deriving (Eq, Show)

newtype AuraCooldown = AuraCooldown Double
  deriving (Eq, Show)

newtype RewardXp = RewardXp Int
  deriving (Eq, Show)

newtype ContactDamage = ContactDamage Int
  deriving (Eq, Show)

newtype Life = Life Double
  deriving (Eq, Show)

newtype Damage = Damage Int
  deriving (Eq, Show)

newtype SpawnTimer = SpawnTimer Double
  deriving (Eq, Show)

newtype SpawnIndex = SpawnIndex Int
  deriving (Eq, Show)

newtype EnemyBrain = EnemyBrain EnemyKind
  deriving (Eq, Show)

newtype AiTimer = AiTimer Double
  deriving (Eq, Show)

newtype UpgradeLog = UpgradeLog [Upgrade]
  deriving (Eq, Show)

data PlayerTag = PlayerTag
  deriving (Eq, Show)

data EnemyTag = EnemyTag
  deriving (Eq, Show)

data KnifeTag = KnifeTag
  deriving (Eq, Show)

data DirectorTag = DirectorTag
  deriving (Eq, Show)

data SurvivorC
  = CPlayerTag PlayerTag
  | CEnemyTag EnemyTag
  | CKnifeTag KnifeTag
  | CDirectorTag DirectorTag
  | CPos Pos
  | CVel Vel
  | CHp Hp
  | CMaxHp MaxHp
  | CXp Xp
  | CLevel Level
  | CPendingUpgrade PendingUpgrade
  | CMoveSpeed MoveSpeed
  | CKnifeDamage KnifeDamage
  | CKnifeCooldown KnifeCooldown
  | CKnifeInterval KnifeInterval
  | CAuraRadius AuraRadius
  | CAuraDamage AuraDamage
  | CAuraCooldown AuraCooldown
  | CRewardXp RewardXp
  | CContactDamage ContactDamage
  | CLife Life
  | CDamage Damage
  | CSpawnTimer SpawnTimer
  | CSpawnIndex SpawnIndex
  | CEnemyBrain EnemyBrain
  | CAiTimer AiTimer
  | CUpgradeLog UpgradeLog
  deriving (Generic)

instance E.ComponentId SurvivorC

data PlayerRow = PlayerRow
  { playerTag :: PlayerTag
  , pos :: Pos
  , hp :: Hp
  , maxHp :: MaxHp
  , xp :: Xp
  , level :: Level
  , pendingUpgrade :: PendingUpgrade
  , moveSpeed :: MoveSpeed
  , knifeDamage :: KnifeDamage
  , knifeCooldown :: KnifeCooldown
  , knifeInterval :: KnifeInterval
  , auraRadius :: AuraRadius
  , auraDamage :: AuraDamage
  , auraCooldown :: AuraCooldown
  , upgradeLog :: UpgradeLog
  } deriving (Generic)

data EnemyRow = EnemyRow
  { enemyTag :: EnemyTag
  , pos :: Pos
  , hp :: Hp
  , rewardXp :: RewardXp
  , contactDamage :: ContactDamage
  , enemyBrain :: EnemyBrain
  , aiTimer :: AiTimer
  } deriving (Generic)

data KnifeRow = KnifeRow
  { knifeTag :: KnifeTag
  , pos :: Pos
  , vel :: Vel
  , life :: Life
  , damage :: Damage
  } deriving (Generic)

data DirectorRow = DirectorRow
  { directorTag :: DirectorTag
  , spawnTimer :: SpawnTimer
  , spawnIndex :: SpawnIndex
  } deriving (Generic)

type World = E.World SurvivorC

type Graph = S.Graph SurvivorC SurvivorMsg

type Program a = S.ProgramM SurvivorC SurvivorMsg a

data FrameError
  = MissingPlayer
  | DuplicatePlayers
  | MissingDirector
  | DuplicateDirectors
  | NoPendingUpgrade
  deriving (Eq, Show)

data Single a = Single
  { entity :: E.Entity
  , value :: a
  }

newtype PendingChoices = PendingChoices Int

newtype ChosenUpgrade = ChosenUpgrade Upgrade

data FrameCtx = FrameCtx
  { input :: SimInput
  , dt :: Double
  }

data LoadedFrame = LoadedFrame
  { ctx :: FrameCtx
  , player :: Single PlayerRow
  , director :: Single DirectorRow
  }

data MovedFrame = MovedFrame
  { ctx :: FrameCtx
  , playerPos :: Pos
  }

data AttackFrame = AttackFrame
  { ctx :: FrameCtx
  , player :: Single PlayerRow
  , enemies :: [(E.Entity, EnemyRow)]
  }

data CombatFrame = CombatFrame
  { ctx :: FrameCtx
  , player :: Single PlayerRow
  , enemies :: [(E.Entity, EnemyRow)]
  , knives :: [(E.Entity, KnifeRow)]
  }

data AuraState
  = AuraInactive
  | AuraReady AuraSpec
  | AuraCooling

data AuraSpec = AuraSpec
  { radiusSq :: Double
  , damage :: Int
  }

data KnifeStrike = KnifeStrike
  { knife :: E.Entity
  , enemy :: E.Entity
  , damage :: Int
  }

data EnemyResolution
  = EnemyKilled (Single EnemyRow)
  | EnemyWounded (Single EnemyRow) Hp

data PlayerResolution = PlayerResolution
  { hp :: Hp
  , level :: Level
  , xp :: Xp
  , pendingUpgrade :: PendingUpgrade
  , auraCooldown :: Maybe AuraCooldown
  }

data CombatPlan = CombatPlan
  { player :: PlayerResolution
  , enemies :: [EnemyResolution]
  , spentKnives :: [E.Entity]
  }

newtype FrameM a = FrameM
  { runFrameM :: Program (Either FrameError a)
  }

instance Functor FrameM where
  fmap f (FrameM action) = FrameM (fmap (fmap f) action)

instance Applicative FrameM where
  pure = FrameM . pure . Right
  FrameM ff <*> FrameM fa =
    FrameM $
      ff >>= \mf ->
        fa >>= \ma ->
          pure (mf <*> ma)

instance Monad FrameM where
  FrameM action >>= f =
    FrameM $
      action >>= \case
        Left err -> pure (Left err)
        Right x -> runFrameM (f x)

collectAll :: forall a. E.Queryable SurvivorC a => Program [(E.Entity, a)]
collectAll =
  let q :: S.Query SurvivorC a
      q = S.query
      batch :: S.Pass SurvivorC SurvivorMsg [(E.Entity, a)]
      batch = S.collect q
  in S.await (S.pass batch)

selectAll :: forall a. E.Queryable SurvivorC a => World -> [(E.Entity, a)]
selectAll =
  let q :: S.Query SurvivorC a
      q = S.query
  in E.runq q

liftFrame :: Program a -> FrameM a
liftFrame action = FrameM (Right <$> action)

throwFrame :: FrameError -> FrameM a
throwFrame err = FrameM (pure (Left err))

exactlyOne :: FrameError -> FrameError -> [(E.Entity, a)] -> Either FrameError (Single a)
exactlyOne missingErr duplicateErr rows =
  case rows of
    [(entity, value)] -> Right (Single entity value)
    [] -> Left missingErr
    _ -> Left duplicateErr

collectSingle ::
  forall a.
  E.Queryable SurvivorC a =>
  FrameError ->
  FrameError ->
  FrameM (Single a)
collectSingle missingErr duplicateErr = do
  rows <- liftFrame (collectAll @a)
  case exactlyOne missingErr duplicateErr rows of
    Left err -> throwFrame err
    Right row -> pure row

requirePendingChoices :: Single PlayerRow -> FrameM PendingChoices
requirePendingChoices player =
  case player.value.pendingUpgrade of
    PendingUpgrade pendingNow
      | pendingNow > 0 -> pure (PendingChoices pendingNow)
      | otherwise -> throwFrame NoPendingUpgrade

requireChosenUpgrade :: SimInput -> FrameM ChosenUpgrade
requireChosenUpgrade input =
  pure $
    case input.upgradePrefs of
      upgrade :| _ -> ChosenUpgrade upgrade

loadFrame :: SimInput -> Double -> FrameM LoadedFrame
loadFrame input dt = do
  player <- collectSingle @PlayerRow MissingPlayer DuplicatePlayers
  director <- collectSingle @DirectorRow MissingDirector DuplicateDirectors
  pure (LoadedFrame (FrameCtx input dt) player director)

applyMovement :: LoadedFrame -> FrameM MovedFrame
applyMovement loaded = do
  let player0 = loaded.player.value
      playerEnt = loaded.player.entity
      director0 = loaded.director.value
      directorEnt = loaded.director.entity
      moveDir = vecClampUnit loaded.ctx.input.move
      Pos playerPos0 = player0.pos
      MoveSpeed playerSpeed = player0.moveSpeed
      playerPos1 = Pos (vecAdd playerPos0 (vecScale (playerSpeed * loaded.ctx.dt) moveDir))
      KnifeCooldown knifeCooldown0 = player0.knifeCooldown
      AuraCooldown auraCooldown0 = player0.auraCooldown
      playerMovePatch =
        S.at playerEnt $
          S.set playerPos1
            <> S.set (KnifeCooldown (max 0 (knifeCooldown0 - loaded.ctx.dt)))
            <> S.set (AuraCooldown (max 0 (auraCooldown0 - loaded.ctx.dt)))
      SpawnTimer spawnTimer0 = director0.spawnTimer
      SpawnIndex spawnIndex0 = director0.spawnIndex
      spawnTimer1 = spawnTimer0 - loaded.ctx.dt
      waveSize = 1 + spawnIndex0 `div` 12
      nextSpawnDelay = max 0.45 (1.0 - fromIntegral (spawnIndex0 `div` 7) * 0.025)
      directorPatch =
        if spawnTimer1 <= 0
          then
            let spawnIndices = take waveSize [spawnIndex0 ..]
                nextIndex = spawnIndex0 + waveSize
                spawned = mconcat (map (spawnEnemyPatch playerPos1) spawnIndices)
            in S.at directorEnt (S.set (SpawnTimer nextSpawnDelay) <> S.set (SpawnIndex nextIndex)) <> spawned
          else
            S.at directorEnt (S.set (SpawnTimer spawnTimer1))
  liftFrame (S.world (playerMovePatch <> directorPatch))
  pure (MovedFrame loaded.ctx playerPos1)

loadMovedFrame :: FrameCtx -> FrameM MovedFrame
loadMovedFrame ctx = do
  player <- collectSingle @PlayerRow MissingPlayer DuplicatePlayers
  pure (MovedFrame ctx player.value.pos)

advanceEnemies :: MovedFrame -> FrameM ()
advanceEnemies moved =
  liftFrame $
    runEachM @EnemyRow $ \enemy -> do
      let (enemyPos1, enemyAi1) = advanceEnemy moved.playerPos moved.ctx.dt enemy
      S.edit (S.set enemyPos1 <> S.set enemyAi1)

advanceKnives :: FrameCtx -> FrameM ()
advanceKnives ctx =
  liftFrame $
    runEachM @KnifeRow $ \knife -> do
      let Pos knifePos0 = knife.pos
          Vel knifeVel = knife.vel
          Life life0 = knife.life
          knifePos1 = Pos (vecAdd knifePos0 (vecScale ctx.dt knifeVel))
          life1 = life0 - ctx.dt
      if life1 <= 0
        then S.edit deleteKnifePatch
        else S.edit (S.set knifePos1 <> S.set (Life life1))

loadAttackFrame :: FrameCtx -> FrameM AttackFrame
loadAttackFrame ctx = do
  player <- collectSingle @PlayerRow MissingPlayer DuplicatePlayers
  enemies <- liftFrame (collectAll @EnemyRow)
  pure (AttackFrame ctx player enemies)

fireKnifeVolley :: AttackFrame -> FrameM ()
fireKnifeVolley attack = do
  let player1 = attack.player.value
      playerEnt1 = attack.player.entity
      KnifeCooldown knifeCooldown1 = player1.knifeCooldown
      KnifeInterval knifeInterval1 = player1.knifeInterval
      KnifeDamage knifeDamage1 = player1.knifeDamage
  case nearestEnemy player1.pos attack.enemies of
    Just (_, targetEnemy)
      | knifeCooldown1 <= 0 ->
          liftFrame . S.world $
            S.at playerEnt1 (S.set (KnifeCooldown knifeInterval1))
              <> spawnKnifePatch player1.pos targetEnemy.pos knifeDamage1
    _ ->
      pure ()

loadCombatFrame :: FrameCtx -> FrameM CombatFrame
loadCombatFrame ctx = do
  player <- collectSingle @PlayerRow MissingPlayer DuplicatePlayers
  enemies <- liftFrame (collectAll @EnemyRow)
  knives <- liftFrame (collectAll @KnifeRow)
  pure (CombatFrame ctx player enemies knives)

combatAuraState :: PlayerRow -> AuraState
combatAuraState player =
  let AuraRadius radius = player.auraRadius
      AuraDamage damage = player.auraDamage
      AuraCooldown cooldown = player.auraCooldown
  in if radius <= 0 || damage <= 0
      then AuraInactive
      else
        if cooldown <= 0
          then AuraReady (AuraSpec (radius * radius) damage)
          else AuraCooling

planAuraDamage :: AuraState -> Pos -> [(E.Entity, EnemyRow)] -> [(E.Entity, Int)]
planAuraDamage auraState playerPos enemies =
  case auraState of
    AuraReady aura ->
      [ (enemyEnt, aura.damage)
      | (enemyEnt, enemy) <- enemies
      , distanceSq playerPos enemy.pos <= aura.radiusSq
      ]
    AuraInactive ->
      []
    AuraCooling ->
      []

planKnifeStrike :: [(E.Entity, EnemyRow)] -> (E.Entity, KnifeRow) -> Maybe KnifeStrike
planKnifeStrike enemies (knifeEnt, knifeRow) = do
  (enemyEnt, _) <- knifeHit knifeRow enemies
  let Damage knifeDamage = knifeRow.damage
  pure (KnifeStrike knifeEnt enemyEnt knifeDamage)

planEnemyDamage :: [(E.Entity, Int)] -> [KnifeStrike] -> Map.Map E.Entity Int
planEnemyDamage auraHits knifeHits =
  Map.fromListWith (+) $
    auraHits <> map (\strike -> (strike.enemy, strike.damage)) knifeHits

planEnemyResolution :: Map.Map E.Entity Int -> (E.Entity, EnemyRow) -> Maybe EnemyResolution
planEnemyResolution enemyDamage (enemyEnt, enemyRow) =
  case Map.lookup enemyEnt enemyDamage of
    Nothing ->
      Nothing
    Just damageTaken ->
      let Hp hp0 = enemyRow.hp
          hp1 = hp0 - damageTaken
          enemy = Single enemyEnt enemyRow
      in if hp1 <= 0
          then Just (EnemyKilled enemy)
          else Just (EnemyWounded enemy (Hp hp1))

contactDamageTotal :: Pos -> [(E.Entity, EnemyRow)] -> Int
contactDamageTotal playerPos enemies =
  sum
    [ damage
    | (_, enemy) <- enemies
    , distanceSq playerPos enemy.pos <= 0.8 * 0.8
    , let ContactDamage damage = enemy.contactDamage
    ]

gainedXpFrom :: [EnemyResolution] -> Int
gainedXpFrom =
  sum
    . mapMaybe
      ( \case
          EnemyKilled enemy ->
            let RewardXp reward = enemy.value.rewardXp
            in Just reward
          EnemyWounded _ _ ->
            Nothing
      )

finishPlayerResolution :: PlayerRow -> AuraState -> [EnemyResolution] -> Int -> PlayerResolution
finishPlayerResolution player auraState enemyResolutions contactDamage =
  let Hp playerHp0 = player.hp
      Level playerLevel0 = player.level
      Xp playerXp0 = player.xp
      PendingUpgrade pending0 = player.pendingUpgrade
      gained = gainedXpFrom enemyResolutions
      (levelNow, xpNow, pendingNow) = grantXp playerLevel0 playerXp0 pending0 gained
      playerHp1 = Hp (max 0 (playerHp0 - contactDamage))
      auraCooldown1 =
        case auraState of
          AuraReady _ -> Just (AuraCooldown auraTickEvery)
          AuraInactive -> Nothing
          AuraCooling -> Nothing
  in PlayerResolution
      { hp = playerHp1
      , level = Level levelNow
      , xp = Xp xpNow
      , pendingUpgrade = PendingUpgrade pendingNow
      , auraCooldown = auraCooldown1
      }

planCombat :: CombatFrame -> CombatPlan
planCombat combat =
  let player = combat.player.value
      auraState = combatAuraState player
      auraHits = planAuraDamage auraState player.pos combat.enemies
      knifeHits = mapMaybe (planKnifeStrike combat.enemies) combat.knives
      enemyDamage = planEnemyDamage auraHits knifeHits
      enemyResolutions = mapMaybe (planEnemyResolution enemyDamage) combat.enemies
      spentKnives = map (.knife) knifeHits
      playerResolution =
        finishPlayerResolution player auraState enemyResolutions (contactDamageTotal player.pos combat.enemies)
  in CombatPlan
      { player = playerResolution
      , enemies = enemyResolutions
      , spentKnives = spentKnives
      }

applyCombatPlan :: E.Entity -> CombatPlan -> S.Patch SurvivorC
applyCombatPlan playerEnt plan =
  let playerPatch =
        S.at playerEnt $
          S.set plan.player.hp
            <> S.set plan.player.level
            <> S.set plan.player.xp
            <> S.set plan.player.pendingUpgrade
            <> maybe mempty S.set plan.player.auraCooldown
      enemyPatch =
        mconcat (map applyEnemyResolution plan.enemies)
      knifePatch =
        mconcat
          [ S.at knifeEnt deleteKnifePatch
          | knifeEnt <- plan.spentKnives
          ]
  in playerPatch <> enemyPatch <> knifePatch
  where
    applyEnemyResolution resolution =
      case resolution of
        EnemyKilled enemy ->
          S.at enemy.entity deleteEnemyPatch
        EnemyWounded enemy hp1 ->
          S.at enemy.entity (S.set hp1)

resolveCombat :: CombatFrame -> FrameM ()
resolveCombat combat =
  let plan = planCombat combat
  in liftFrame (S.world (applyCombatPlan combat.player.entity plan))

applyUpgradeChoice :: FrameCtx -> FrameM ()
applyUpgradeChoice ctx = do
  player <- collectSingle @PlayerRow MissingPlayer DuplicatePlayers
  _ <- requirePendingChoices player
  ChosenUpgrade upgrade <- requireChosenUpgrade ctx.input
  liftFrame (S.world (S.at player.entity (applyUpgradePatch player.value upgrade)))

runEachM ::
  forall a.
  (HasCallStack, Typeable a, E.Queryable SurvivorC a) =>
  (a -> S.Local SurvivorC SurvivorMsg ()) ->
  Program ()
runEachM f =
  let batch :: S.Pass SurvivorC SurvivorMsg ()
      batch = S.eachM f
  in S.await (S.pass batch)

survivorDt :: Double
survivorDt = 0.2

survivorFrames :: Int
survivorFrames = 120

spawnRadius :: Double
spawnRadius = 9.5

knifeSpeed :: Double
knifeSpeed = 11.5

knifeLifetime :: Double
knifeLifetime = 1.3

auraTickEvery :: Double
auraTickEvery = 0.45

vecAdd :: Vec -> Vec -> Vec
vecAdd (Vec ax ay) (Vec bx by) = Vec (ax + bx) (ay + by)

vecSub :: Vec -> Vec -> Vec
vecSub (Vec ax ay) (Vec bx by) = Vec (ax - bx) (ay - by)

vecScale :: Double -> Vec -> Vec
vecScale s (Vec x y) = Vec (x * s) (y * s)

vecPerp :: Vec -> Vec
vecPerp (Vec x y) = Vec (-y) x

vecLength :: Vec -> Double
vecLength (Vec x y) = sqrt (x * x + y * y)

vecNormalize :: Vec -> Vec
vecNormalize v =
  let len = vecLength v
  in if len <= 1.0e-9
      then Vec 0 0
      else vecScale (1 / len) v

vecClampUnit :: Vec -> Vec
vecClampUnit v =
  let len = vecLength v
  in if len <= 1
      then v
      else vecScale (1 / len) v

distanceSq :: Pos -> Pos -> Double
distanceSq (Pos a) (Pos b) =
  let Vec dx dy = vecSub a b
  in dx * dx + dy * dy

renderVec :: Vec -> String
renderVec (Vec x y) = "(" <> showFF x <> "," <> showFF y <> ")"

renderPos :: Pos -> String
renderPos (Pos pos) = renderVec pos

showFF :: Double -> String
showFF x = show (fromIntegral (round (x * 10) :: Int) / 10 :: Double)

nextLevelXp :: Int -> Int
nextLevelXp level = 10 + (level - 1) * 8

grantXp :: Int -> Int -> Int -> Int -> (Int, Int, Int)
grantXp level0 xp0 pending0 gained = go level0 (xp0 + gained) pending0
  where
    go levelNow xpNow pendingNow
      | xpNow >= nextLevelXp levelNow =
          go (levelNow + 1) (xpNow - nextLevelXp levelNow) (pendingNow + 1)
      | otherwise =
          (levelNow, xpNow, pendingNow)

kindForSpawn :: Int -> EnemyKind
kindForSpawn spawnIx =
  case spawnIx `mod` 8 of
    0 -> Dasher
    1 -> Orbiter
    2 -> Shambler
    3 -> Shambler
    4 -> Dasher
    5 -> Shambler
    6 -> Orbiter
    _ -> Shambler

spawnOffset :: Int -> Vec
spawnOffset spawnIx =
  let angle = fromIntegral ((spawnIx * 37) `mod` 360) * pi / 180
      radius = spawnRadius + fromIntegral (spawnIx `mod` 3)
  in Vec (cos angle * radius) (sin angle * radius)

spawnEnemyPatch :: Pos -> Int -> S.Patch SurvivorC
spawnEnemyPatch (Pos playerPos) spawnIx =
  let kind = kindForSpawn spawnIx
      spawnPos = Pos (vecAdd playerPos (spawnOffset spawnIx))
      (hp, reward, contact, aiTimer0) =
        case kind of
          Shambler -> (9, 6, 1, 0.0)
          Dasher -> (8, 7, 2, 1.4)
          Orbiter -> (12, 8, 1, 0.0)
  in S.patch (snd . E.spawn (EnemyTag, spawnPos, Hp hp, RewardXp reward, ContactDamage contact, EnemyBrain kind, AiTimer aiTimer0))

spawnKnifePatch :: Pos -> Pos -> Int -> S.Patch SurvivorC
spawnKnifePatch (Pos playerPos) (Pos targetPos) damage =
  let dir = vecNormalize (vecSub targetPos playerPos)
      knifeVel = Vel (vecScale knifeSpeed dir)
      knifePos = Pos (vecAdd playerPos (vecScale 0.8 dir))
  in S.patch (snd . E.spawn (KnifeTag, knifePos, knifeVel, Life knifeLifetime, Damage damage))

deleteEnemyPatch :: S.EntityPatch SurvivorC
deleteEnemyPatch =
  S.del @EnemyTag
    <> S.del @Pos
    <> S.del @Hp
    <> S.del @RewardXp
    <> S.del @ContactDamage
    <> S.del @EnemyBrain
    <> S.del @AiTimer

deleteKnifePatch :: S.EntityPatch SurvivorC
deleteKnifePatch =
  S.del @KnifeTag
    <> S.del @Pos
    <> S.del @Vel
    <> S.del @Life
    <> S.del @Damage

nearestEnemy :: Pos -> [(E.Entity, EnemyRow)] -> Maybe (E.Entity, EnemyRow)
nearestEnemy origin enemies =
  case enemies of
    [] -> Nothing
    row : rest -> Just (foldl' pick row rest)
  where
    pick best@(_, bestRow) candidate@(_, candidateRow) =
      if distanceSq origin candidateRow.pos < distanceSq origin bestRow.pos
        then candidate
        else best

knifeHit :: KnifeRow -> [(E.Entity, EnemyRow)] -> Maybe (E.Entity, EnemyRow)
knifeHit knife enemies =
  case nearestEnemy knife.pos enemies of
    Just row@(_, enemy)
      | distanceSq knife.pos enemy.pos <= 0.65 * 0.65 -> Just row
    _ -> Nothing

advanceEnemy :: Pos -> Double -> EnemyRow -> (Pos, AiTimer)
advanceEnemy playerPos dt enemy =
  let Pos enemyPos = enemy.pos
      toPlayer = vecNormalize (vecSub (let Pos p = playerPos in p) enemyPos)
      AiTimer timer0 = enemy.aiTimer
      EnemyBrain kind = enemy.enemyBrain
  in case kind of
      Shambler ->
        (Pos (vecAdd enemyPos (vecScale (1.9 * dt) toPlayer)), AiTimer timer0)
      Dasher ->
        let timer1 =
              if timer0 - dt <= 0
                then 1.45
                else timer0 - dt
            speed =
              if timer0 <= 0.35
                then 6.3
                else 2.4
        in (Pos (vecAdd enemyPos (vecScale (speed * dt) toPlayer)), AiTimer timer1)
      Orbiter ->
        let orbitSign =
              if sin timer0 >= 0
                then 1
                else -1
            orbitDir = vecScale orbitSign (vecPerp toPlayer)
            moveDir = vecNormalize (vecAdd (vecScale 0.75 toPlayer) (vecScale 0.95 orbitDir))
            timer1 = timer0 + dt * 2.2
        in (Pos (vecAdd enemyPos (vecScale (2.8 * dt) moveDir)), AiTimer timer1)

applyUpgradePatch :: PlayerRow -> Upgrade -> S.EntityPatch SurvivorC
applyUpgradePatch player upgrade =
  let MoveSpeed moveSpeed0 = player.moveSpeed
      KnifeDamage knifeDamage0 = player.knifeDamage
      KnifeInterval knifeInterval0 = player.knifeInterval
      AuraRadius auraRadius0 = player.auraRadius
      AuraDamage auraDamage0 = player.auraDamage
      Hp hp0 = player.hp
      MaxHp maxHp0 = player.maxHp
      PendingUpgrade pending0 = player.pendingUpgrade
      UpgradeLog upgrades0 = player.upgradeLog
      pending1 = max 0 (pending0 - 1)
      logPatch = S.set (PendingUpgrade pending1) <> S.set (UpgradeLog (upgrades0 <> [upgrade]))
  in case upgrade of
      UpgradeRapidFire ->
        logPatch <> S.set (KnifeInterval (max 0.22 (knifeInterval0 * 0.82)))
      UpgradeKnifeDamage ->
        logPatch <> S.set (KnifeDamage (knifeDamage0 + 4))
      UpgradeMoveSpeed ->
        logPatch <> S.set (MoveSpeed (moveSpeed0 + 0.8))
      UpgradeHolyAura ->
        logPatch
          <> S.set (AuraRadius (if auraRadius0 == 0 then 2.6 else auraRadius0 + 0.5))
          <> S.set (AuraDamage (if auraDamage0 == 0 then 4 else auraDamage0 + 2))
      UpgradeMaxHp ->
        let maxHp1 = maxHp0 + 14
            hp1 = min maxHp1 (hp0 + 14)
        in logPatch <> S.set (MaxHp maxHp1) <> S.set (Hp hp1)

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust pick xs =
  case xs of
    [] -> Nothing
    y : ys ->
      case pick y of
        Just z -> Just z
        Nothing -> firstJust pick ys

awaitPhase ::
  (SurvivorMsg -> Maybe (FrameId, SimInput)) ->
  Maybe FrameId ->
  Program (FrameId, SimInput)
awaitPhase pick lastSeen = do
  events <- S.await matches
  case firstJust pickFresh events of
    Just frame -> pure frame
    Nothing -> awaitPhase pick lastSeen
  where
    pickFresh msg = do
      frame@(frameId, _) <- pick msg
      if Just frameId == lastSeen then Nothing else Just frame
    matches msg =
      case pickFresh msg of
        Just _ -> True
        Nothing -> False

movementProgram :: Maybe FrameId -> Program ()
movementProgram lastSeen = do
  (frameId, input) <- awaitPhase pickStart lastSeen
  dt <- S.dt
  void . runFrameM $ do
    loaded <- loadFrame input dt
    _ <- applyMovement loaded
    pure ()
  S.send [EnemyPhase frameId input]
  movementProgram (Just frameId)
  where
    pickStart msg =
      case msg of
        FrameStart frameId input -> Just (frameId, input)
        _ -> Nothing

enemyProgram :: Maybe FrameId -> Program ()
enemyProgram lastSeen = do
  (frameId, input) <- awaitPhase pickEnemy lastSeen
  dt <- S.dt
  let ctx = FrameCtx input dt
  void (runFrameM (advanceEnemies =<< loadMovedFrame ctx))
  S.send [KnifePhase frameId input]
  enemyProgram (Just frameId)
  where
    pickEnemy msg =
      case msg of
        EnemyPhase frameId input -> Just (frameId, input)
        _ -> Nothing

knifeProgram :: Maybe FrameId -> Program ()
knifeProgram lastSeen = do
  (frameId, input) <- awaitPhase pickKnife lastSeen
  dt <- S.dt
  void (runFrameM (advanceKnives (FrameCtx input dt)))
  S.send [FirePhase frameId input]
  knifeProgram (Just frameId)
  where
    pickKnife msg =
      case msg of
        KnifePhase frameId input -> Just (frameId, input)
        _ -> Nothing

fireProgram :: Maybe FrameId -> Program ()
fireProgram lastSeen = do
  (frameId, input) <- awaitPhase pickFire lastSeen
  dt <- S.dt
  let ctx = FrameCtx input dt
  void (runFrameM (fireKnifeVolley =<< loadAttackFrame ctx))
  S.send [CombatPhaseMsg frameId input]
  fireProgram (Just frameId)
  where
    pickFire msg =
      case msg of
        FirePhase frameId input -> Just (frameId, input)
        _ -> Nothing

combatProgram :: Maybe FrameId -> Program ()
combatProgram lastSeen = do
  (frameId, input) <- awaitPhase pickCombat lastSeen
  dt <- S.dt
  let ctx = FrameCtx input dt
  void (runFrameM (resolveCombat =<< loadCombatFrame ctx))
  S.send [UpgradePhase frameId input]
  combatProgram (Just frameId)
  where
    pickCombat msg =
      case msg of
        CombatPhaseMsg frameId input -> Just (frameId, input)
        _ -> Nothing

upgradeProgram :: Maybe FrameId -> Program ()
upgradeProgram lastSeen = do
  (frameId, input) <- awaitPhase pickUpgrade lastSeen
  dt <- S.dt
  let ctx = FrameCtx input dt
  void (runFrameM (applyUpgradeChoice ctx))
  upgradeProgram (Just frameId)
  where
    pickUpgrade msg =
      case msg of
        UpgradePhase frameId input -> Just (frameId, input)
        _ -> Nothing

survivorGraph :: Graph
survivorGraph = S.graph $ do
  _ <- S.program (movementProgram Nothing)
  _ <- S.program (enemyProgram Nothing)
  _ <- S.program (knifeProgram Nothing)
  _ <- S.program (fireProgram Nothing)
  _ <- S.program (combatProgram Nothing)
  _ <- S.program (upgradeProgram Nothing)
  pure ()

survivorWorld :: World
survivorWorld =
  let w0 = E.emptyWorld
      (playerEnt, w1) = E.spawn PlayerTag w0
      w1' =
        E.set playerEnt (Pos (Vec 0 0))
          . E.set playerEnt (Hp 60)
          . E.set playerEnt (MaxHp 60)
          . E.set playerEnt (Xp 0)
          . E.set playerEnt (Level 1)
          . E.set playerEnt (PendingUpgrade 0)
          . E.set playerEnt (MoveSpeed 4.4)
          . E.set playerEnt (KnifeDamage 10)
          . E.set playerEnt (KnifeCooldown 0.0)
          . E.set playerEnt (KnifeInterval 0.75)
          . E.set playerEnt (AuraRadius 0)
          . E.set playerEnt (AuraDamage 0)
          . E.set playerEnt (AuraCooldown 0)
          . E.set playerEnt (UpgradeLog [])
          $ w1
      (_, w2) = E.spawn (DirectorTag, SpawnTimer 0.45, SpawnIndex 0) w1'
  in w2

survivorInputAt :: Int -> SimInput
survivorInputAt frameIx =
  let moveVec =
        case (frameIx `div` 15) `mod` 6 of
          0 -> Vec 1 0
          1 -> Vec 1 1
          2 -> Vec 0 1
          3 -> Vec (-1) 1
          4 -> Vec (-1) 0
          _ -> Vec 0 (-1)
      upgradePrefs
        | frameIx < 35 = UpgradeRapidFire :| [UpgradeKnifeDamage, UpgradeHolyAura, UpgradeMoveSpeed, UpgradeMaxHp]
        | frameIx < 75 = UpgradeHolyAura :| [UpgradeKnifeDamage, UpgradeRapidFire, UpgradeMoveSpeed, UpgradeMaxHp]
        | otherwise = UpgradeMoveSpeed :| [UpgradeKnifeDamage, UpgradeMaxHp, UpgradeHolyAura, UpgradeRapidFire]
  in SimInput moveVec upgradePrefs

stepSurvivor :: (World, Graph, Int) -> SimInput -> (World, Graph, Int)
stepSurvivor (w0, g0, frameIx0) input =
  let frameId = FrameId (frameIx0 + 1)
      (w1, _, g1) = S.run survivorDt w0 [FrameStart frameId input] g0
  in (w1, g1, frameIx0 + 1)

playerSummary :: World -> Maybe PlayerRow
playerSummary w =
  case selectAll @PlayerRow w of
    (_, player) : _ -> Just player
    [] -> Nothing

enemySummary :: World -> [EnemyRow]
enemySummary w =
  [ enemy
  | (_, enemy) <- selectAll @EnemyRow w
  ]

knifeSummary :: World -> [KnifeRow]
knifeSummary w =
  [ knife
  | (_, knife) <- selectAll @KnifeRow w
  ]

enemyBreakdown :: [EnemyRow] -> String
enemyBreakdown enemies =
  let count kind = length [ () | enemy <- enemies, enemy.enemyBrain == EnemyBrain kind ]
  in "shambler=" <> show (count Shambler)
      <> ", dasher=" <> show (count Dasher)
      <> ", orbiter=" <> show (count Orbiter)

renderUpgradeList :: [Upgrade] -> String
renderUpgradeList upgrades =
  case upgrades of
    [] -> "[]"
    _ -> "[" <> intercalate "," (map show upgrades) <> "]"

renderInputSummary :: SimInput -> String
renderInputSummary input =
  let upgrade :| _ = input.upgradePrefs
  in "move=" <> renderVec input.move <> ", pick=" <> show upgrade

renderFrameSummary :: Int -> SimInput -> World -> String
renderFrameSummary frameIx input world =
  case playerSummary world of
    Nothing ->
      "frame " <> show frameIx <> ": player missing"
    Just player ->
      let Hp hpNow = player.hp
          MaxHp maxHpNow = player.maxHp
          Level levelNow = player.level
          Xp xpNow = player.xp
          PendingUpgrade pendingNow = player.pendingUpgrade
          UpgradeLog upgrades = player.upgradeLog
          enemies = enemySummary world
          knives = knifeSummary world
      in "frame " <> show frameIx
          <> ": "
          <> renderInputSummary input
          <> " pos=" <> renderPos player.pos
          <> " hp=" <> show hpNow <> "/" <> show maxHpNow
          <> " lvl=" <> show levelNow
          <> " xp=" <> show xpNow
          <> " pending=" <> show pendingNow
          <> " upgrades=" <> renderUpgradeList upgrades
          <> " knives=" <> show (length knives)
          <> " enemies={" <> enemyBreakdown enemies <> "}"

runSurvivorSim :: IO ()
runSurvivorSim = do
  putStrLn "Pure survivor-style simulation:"
  putStrLn "  scripted movement + scripted upgrade preferences + deterministic enemy director"
  let inputs = map survivorInputAt [0 .. survivorFrames - 1]
      states = scanl stepSurvivor (survivorWorld, survivorGraph, 0 :: Int) inputs
      samples =
        [ (frameIx + 1, input, world)
        | (frameIx, (input, (world, _, _))) <- zip [0 :: Int ..] (zip inputs (drop 1 states))
        , frameIx < 5 || (frameIx + 1) `mod` 15 == 0 || frameIx + 1 == survivorFrames
        ]
  mapM_ (\(frameIx, input, world) -> putStrLn (renderFrameSummary frameIx input world)) samples
  putStrLn "Run `cabal run corn-examples -- scenes-navigation` for the app-layer example."

examples :: [Example]
examples =
  [ Example "survivor-sim" "scripted vampire-survivor-style sim with upgrades, enemy behaviors, and pure inputs" runSurvivorSim
  , Example "platformer" "single-actor gravity plus periodic jump impulse loop" Platformer.runConcept
  , Example "horde-survival" "moving player target with mob chase and attrition" HordeSurvival.runConcept
  , Example "bullet-hell" "periodic radial burst spawning with TTL cleanup" BulletHell.runConcept
  , Example "scenes-navigation" "Engine.Corn.App create/step/navigate example with stacked paths" ScenesNavigation.runConcept
  ]

runOne :: Example -> IO ()
runOne example = do
  putStrLn ("== " <> example.name <> " ==")
  putStrLn example.summary
  example.run

runNamed :: String -> IO ()
runNamed exampleName =
  case filter (\example -> example.name == exampleName) examples of
    example : _ -> runOne example
    [] -> usage

usage :: IO ()
usage = do
  putStrLn "corn-examples: run a focused Corn example or the full concept set."
  putStrLn "Usage:"
  putStrLn "  cabal run corn-examples"
  putStrLn "  cabal run corn-examples -- list"
  putStrLn "  cabal run corn-examples -- all"
  putStrLn "  cabal run corn-examples -- <example>"
  putStrLn "Examples:"
  mapM_ (\example -> putStrLn ("  - " <> example.name <> ": " <> example.summary)) examples

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runNamed "survivor-sim"
    ["all"] -> mapM_ runOne examples
    ["list"] -> usage
    [name] -> runNamed name
    _ -> usage
