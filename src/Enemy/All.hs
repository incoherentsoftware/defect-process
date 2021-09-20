module Enemy.All
    ( module Enemy.All.Axe
    , mkEnemyFromType
    , enemyPreloadPackFilePathsFromType
    ) where

import AppEnv
import Enemy
import Enemy.All.Axe
import Util

commonPreloadPackFilePaths =
    [ "data/particles/particles-enemy.pack"
    ] :: [FilePath]

-- NOTE: this is modified from the full source since only the axe enemy is included in this repo
mkEnemyFromType :: EnemyType -> Pos2 -> Direction -> AppEnv p (Some Enemy)
mkEnemyFromType enType pos dir = mkFn pos dir
    where
        mkFn = case enType of
            AxeEnemy          -> mkAxeEnemy
            FlyingEnemy       -> mkAxeEnemy
            GiantEnemy        -> mkAxeEnemy
            HammerEnemy       -> mkAxeEnemy
            DogEnemy          -> mkAxeEnemy
            BlobEnemy         -> mkAxeEnemy
            SpearEnemy        -> mkAxeEnemy
            TurretEnemy       -> mkAxeEnemy
            BubbleTurretEnemy -> mkAxeEnemy
            LankyEnemy        -> mkAxeEnemy
            BatEnemy          -> mkAxeEnemy
            BombEnemy         -> mkAxeEnemy
            ClawsEnemy        -> mkAxeEnemy
            FlailEnemy        -> mkAxeEnemy
            ZombieEnemy       -> mkAxeEnemy
            WallEnemy         -> mkAxeEnemy
            HopEnemy          -> mkAxeEnemy
            BossEnemy         -> mkAxeEnemy

-- NOTE: this is modified from the full source since only the axe enemy is included in this repo
enemyPreloadPackFilePathsFromType :: EnemyType -> [FilePath]
enemyPreloadPackFilePathsFromType = (commonPreloadPackFilePaths ++) . \case
    AxeEnemy          -> allAxeEnemyPreloadPackFilePaths
    FlyingEnemy       -> allAxeEnemyPreloadPackFilePaths
    GiantEnemy        -> allAxeEnemyPreloadPackFilePaths
    HammerEnemy       -> allAxeEnemyPreloadPackFilePaths
    DogEnemy          -> allAxeEnemyPreloadPackFilePaths
    BlobEnemy         -> allAxeEnemyPreloadPackFilePaths
    SpearEnemy        -> allAxeEnemyPreloadPackFilePaths
    TurretEnemy       -> allAxeEnemyPreloadPackFilePaths
    BubbleTurretEnemy -> allAxeEnemyPreloadPackFilePaths
    LankyEnemy        -> allAxeEnemyPreloadPackFilePaths
    BatEnemy          -> allAxeEnemyPreloadPackFilePaths
    BombEnemy         -> allAxeEnemyPreloadPackFilePaths
    ClawsEnemy        -> allAxeEnemyPreloadPackFilePaths
    FlailEnemy        -> allAxeEnemyPreloadPackFilePaths
    ZombieEnemy       -> allAxeEnemyPreloadPackFilePaths
    WallEnemy         -> allAxeEnemyPreloadPackFilePaths
    HopEnemy          -> allAxeEnemyPreloadPackFilePaths
    BossEnemy         -> allAxeEnemyPreloadPackFilePaths
