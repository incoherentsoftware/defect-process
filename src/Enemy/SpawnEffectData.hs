module Enemy.SpawnEffectData
    ( EnemySpawnEffectData(..)
    , dummySpawnEffectData
    , readSpawnEffectData
    ) where

import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Axe
import Configs.All.Enemy.Bat
import Configs.All.Enemy.Blob
import Configs.All.Enemy.Bomb
import Configs.All.Enemy.BubbleTurret
import Configs.All.Enemy.Claws
import Configs.All.Enemy.Dog
import Configs.All.Enemy.Flail
import Configs.All.Enemy.Flying
import Configs.All.Enemy.Giant
import Configs.All.Enemy.Hammer
import Configs.All.Enemy.Hop
import Configs.All.Enemy.Lanky
import Configs.All.Enemy.Spear
import Configs.All.Enemy.Turret
import Configs.All.Enemy.Wall
import Configs.All.Enemy.Zombie
import Enemy.SpawnEffectData.Types
import Enemy.Types
import Window.Graphics.Util

dummySpawnEffectData = EnemySpawnEffectData
    { _drawScale = NonScaled
    , _offset    = Nothing
    , _inAir     = Nothing
    } :: EnemySpawnEffectData

readSpawnEffectData :: ConfigsRead m => Maybe EnemyType -> m EnemySpawnEffectData
readSpawnEffectData Nothing       = return dummySpawnEffectData
readSpawnEffectData (Just enType) = case enType of
    AxeEnemy          -> (_spawnEffectData :: AxeEnemyConfig -> EnemySpawnEffectData) <$> readEnemyConfig _axe
    FlyingEnemy       -> (_spawnEffectData :: FlyingEnemyConfig -> EnemySpawnEffectData) <$> readEnemyConfig _flying
    GiantEnemy        -> (_spawnEffectData :: GiantEnemyConfig -> EnemySpawnEffectData) <$> readEnemyConfig _giant
    HammerEnemy       -> (_spawnEffectData :: HammerEnemyConfig -> EnemySpawnEffectData) <$> readEnemyConfig _hammer
    DogEnemy          -> (_spawnEffectData :: DogEnemyConfig -> EnemySpawnEffectData) <$> readEnemyConfig _dog
    BlobEnemy         -> (_spawnEffectData :: BlobEnemyConfig -> EnemySpawnEffectData) <$> readEnemyConfig _blob
    SpearEnemy        -> (_spawnEffectData :: SpearEnemyConfig -> EnemySpawnEffectData) <$> readEnemyConfig _spear
    TurretEnemy       -> (_spawnEffectData :: TurretEnemyConfig -> EnemySpawnEffectData) <$> readEnemyConfig _turret
    BubbleTurretEnemy ->
        (_spawnEffectData :: BubbleTurretEnemyConfig -> EnemySpawnEffectData) <$> readEnemyConfig _bubbleTurret
    LankyEnemy        -> (_spawnEffectData :: LankyEnemyConfig -> EnemySpawnEffectData) <$> readEnemyConfig _lanky
    BatEnemy          -> (_spawnEffectData :: BatEnemyConfig -> EnemySpawnEffectData) <$> readEnemyConfig _bat
    BombEnemy         -> (_spawnEffectData :: BombEnemyConfig -> EnemySpawnEffectData) <$> readEnemyConfig _bomb
    ClawsEnemy        -> (_spawnEffectData :: ClawsEnemyConfig -> EnemySpawnEffectData) <$> readEnemyConfig _claws
    FlailEnemy        -> (_spawnEffectData :: FlailEnemyConfig -> EnemySpawnEffectData) <$> readEnemyConfig _flail
    ZombieEnemy       -> (_spawnEffectData :: ZombieEnemyConfig -> EnemySpawnEffectData) <$> readEnemyConfig _zombie
    WallEnemy         -> (_spawnEffectData :: WallEnemyConfig -> EnemySpawnEffectData) <$> readEnemyConfig _wall
    HopEnemy          -> (_spawnEffectData :: HopEnemyConfig -> EnemySpawnEffectData) <$> readEnemyConfig _hop
    BossEnemy         -> return dummySpawnEffectData
