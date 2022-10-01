module Enemy.DeathEffectData
    ( module Enemy.DeathEffectData.Types
    , dummyDeathEffectData
    , readDeathEffectData
    ) where

import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Axe
import Configs.All.Enemy.Bat
import Configs.All.Enemy.Blob
import Configs.All.Enemy.BubbleTurret
import Configs.All.Enemy.Claws
import Configs.All.Enemy.Dog
import Configs.All.Enemy.Flail
import Configs.All.Enemy.Flying
import Configs.All.Enemy.Hammer
import Configs.All.Enemy.Hop
import Configs.All.Enemy.Lanky
import Configs.All.Enemy.Spear
import Configs.All.Enemy.Turret
import Configs.All.Enemy.Wall
import Configs.All.Enemy.Zombie
import Enemy.DeathEffectData.Types
import Enemy.Types
import Window.Graphics.Util

dummyDeathEffectData = EnemyDeathEffectData
    { _drawScale = NonScaled
    , _offset    = Nothing
    } :: EnemyDeathEffectData

readDeathEffectData :: ConfigsRead m => Maybe EnemyType -> m EnemyDeathEffectData
readDeathEffectData Nothing       = return dummyDeathEffectData
readDeathEffectData (Just enType) = case enType of
    AxeEnemy          -> (_deathEffectData :: AxeEnemyConfig -> EnemyDeathEffectData) <$> readEnemyConfig _axe
    FlyingEnemy       -> (_deathEffectData :: FlyingEnemyConfig -> EnemyDeathEffectData) <$> readEnemyConfig _flying
    GiantEnemy        -> return dummyDeathEffectData
    HammerEnemy       -> (_deathEffectData :: HammerEnemyConfig -> EnemyDeathEffectData) <$> readEnemyConfig _hammer
    DogEnemy          -> (_deathEffectData :: DogEnemyConfig -> EnemyDeathEffectData) <$> readEnemyConfig _dog
    BlobEnemy         -> (_deathEffectData :: BlobEnemyConfig -> EnemyDeathEffectData) <$> readEnemyConfig _blob
    SpearEnemy        -> (_deathEffectData :: SpearEnemyConfig -> EnemyDeathEffectData) <$> readEnemyConfig _spear
    TurretEnemy       -> (_deathEffectData :: TurretEnemyConfig -> EnemyDeathEffectData) <$> readEnemyConfig _turret
    BubbleTurretEnemy ->
        (_deathEffectData :: BubbleTurretEnemyConfig -> EnemyDeathEffectData) <$> readEnemyConfig _bubbleTurret
    LankyEnemy        -> (_deathEffectData :: LankyEnemyConfig -> EnemyDeathEffectData) <$> readEnemyConfig _lanky
    BatEnemy          -> (_deathEffectData :: BatEnemyConfig -> EnemyDeathEffectData) <$> readEnemyConfig _bat
    BombEnemy         -> return dummyDeathEffectData
    ClawsEnemy        -> (_deathEffectData :: ClawsEnemyConfig -> EnemyDeathEffectData) <$> readEnemyConfig _claws
    FlailEnemy        -> (_deathEffectData :: FlailEnemyConfig -> EnemyDeathEffectData) <$> readEnemyConfig _flail
    ZombieEnemy       -> (_deathEffectData :: ZombieEnemyConfig -> EnemyDeathEffectData) <$> readEnemyConfig _zombie
    WallEnemy         -> (_deathEffectData :: WallEnemyConfig -> EnemyDeathEffectData) <$> readEnemyConfig _wall
    HopEnemy          -> (_deathEffectData :: HopEnemyConfig -> EnemyDeathEffectData) <$> readEnemyConfig _hop
    BossEnemy         -> return dummyDeathEffectData
