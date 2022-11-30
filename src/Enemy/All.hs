module Enemy.All
    ( module Enemy.All.Axe
    , module Enemy.All.Bat
    , module Enemy.All.Blob
    , module Enemy.All.Bomb
    , module Enemy.All.BubbleTurret
    , module Enemy.All.Claws
    , module Enemy.All.Dog
    , module Enemy.All.Flail
    , module Enemy.All.Flying
    , module Enemy.All.Giant
    , module Enemy.All.Hammer
    , module Enemy.All.Hop
    , module Enemy.All.Lanky
    , module Enemy.All.Spear
    , module Enemy.All.Turret
    , module Enemy.All.Wall
    , module Enemy.All.Zombie
    , mkEnemyFromType
    , enemyPreloadPackFilePathsFromType
    ) where

import AppEnv
import Enemy
import Enemy.All.Axe
import Enemy.All.Bat
import Enemy.All.Blob
import Enemy.All.Bomb
import Enemy.All.Boss
import Enemy.All.BubbleTurret
import Enemy.All.Claws
import Enemy.All.Dog
import Enemy.All.Flail
import Enemy.All.Flying
import Enemy.All.Giant
import Enemy.All.Hammer
import Enemy.All.Hop
import Enemy.All.Lanky
import Enemy.All.Spear
import Enemy.All.Turret
import Enemy.All.Wall
import Enemy.All.Zombie
import Util

commonPreloadPackFilePaths =
    [ "data/particles/particles-enemy.pack"
    ] :: [FilePath]

mkEnemyFromType :: EnemyType -> Pos2 -> Direction -> AppEnv p (Some Enemy)
mkEnemyFromType enType pos dir = mkFn pos dir
    where
        mkFn = case enType of
            AxeEnemy          -> mkAxeEnemy
            FlyingEnemy       -> mkFlyingEnemy
            GiantEnemy        -> mkGiantEnemy
            HammerEnemy       -> mkHammerEnemy
            DogEnemy          -> mkDogEnemy
            BlobEnemy         -> mkBlobEnemy
            SpearEnemy        -> mkSpearEnemy
            TurretEnemy       -> mkTurretEnemy
            BubbleTurretEnemy -> mkBubbleTurretEnemy
            LankyEnemy        -> mkLankyEnemy
            BatEnemy          -> mkBatEnemy
            BombEnemy         -> mkBombEnemy
            ClawsEnemy        -> mkClawsEnemy
            FlailEnemy        -> mkFlailEnemy
            ZombieEnemy       -> mkZombieEnemy
            WallEnemy         -> mkWallEnemy
            HopEnemy          -> mkHopEnemy
            BossEnemy         -> mkBossEnemy

enemyPreloadPackFilePathsFromType :: EnemyType -> [FilePath]
enemyPreloadPackFilePathsFromType = (commonPreloadPackFilePaths ++) . \case
    AxeEnemy          -> allAxeEnemyPreloadPackFilePaths
    FlyingEnemy       -> allFlyingEnemyPreloadPackFilePaths
    GiantEnemy        -> allGiantEnemyPreloadPackFilePaths
    HammerEnemy       -> allHammerEnemyPreloadPackFilePaths
    DogEnemy          -> allDogEnemyPreloadPackFilePaths
    BlobEnemy         -> allBlobEnemyPreloadPackFilePaths
    SpearEnemy        -> allSpearEnemyPreloadPackFilePaths
    TurretEnemy       -> allTurretEnemyPreloadPackFilePaths
    BubbleTurretEnemy -> allBubbleTurretEnemyPreloadPackFilePaths
    LankyEnemy        -> allLankyEnemyPreloadPackFilePaths
    BatEnemy          -> allBatEnemyPreloadPackFilePaths
    BombEnemy         -> allBombEnemyPreloadPackFilePaths
    ClawsEnemy        -> allClawsEnemyPreloadPackFilePaths
    FlailEnemy        -> allFlailEnemyPreloadPackFilePaths
    ZombieEnemy       -> allZombieEnemyPreloadPackFilePaths
    WallEnemy         -> allWallEnemyPreloadPackFilePaths
    HopEnemy          -> allHopEnemyPreloadPackFilePaths
    BossEnemy         -> allBossEnemyPreloadPackFilePaths
