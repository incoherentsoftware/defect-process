module Enemy.HurtEffectData
    ( module Enemy.HurtEffectData.Types
    , readHurtEffectData
    ) where

import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Axe
import Configs.All.Enemy.Bat
import Configs.All.Enemy.Blob
import Configs.All.Enemy.Bomb
import Configs.All.Enemy.Boss
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
import Enemy.HurtEffectData.Types
import Enemy.Types
import Window.Graphics.Util

dummyHurtEffectData = EnemyHurtEffectData
    { _drawScale       = NonScaled
    , _strongDrawScale = NonScaled
    } :: EnemyHurtEffectData

readHurtEffectData :: ConfigsRead m => Maybe EnemyType -> m EnemyHurtEffectData
readHurtEffectData Nothing       = return dummyHurtEffectData
readHurtEffectData (Just enType) = case enType of
    AxeEnemy          -> (_hurtEffectData :: AxeEnemyConfig -> EnemyHurtEffectData) <$> readEnemyConfig _axe
    FlyingEnemy       -> (_hurtEffectData :: FlyingEnemyConfig -> EnemyHurtEffectData) <$> readEnemyConfig _flying
    GiantEnemy        -> (_hurtEffectData :: GiantEnemyConfig -> EnemyHurtEffectData) <$> readEnemyConfig _giant
    HammerEnemy       -> (_hurtEffectData :: HammerEnemyConfig -> EnemyHurtEffectData) <$> readEnemyConfig _hammer
    DogEnemy          -> (_hurtEffectData :: DogEnemyConfig -> EnemyHurtEffectData) <$> readEnemyConfig _dog
    BlobEnemy         -> (_hurtEffectData :: BlobEnemyConfig -> EnemyHurtEffectData) <$> readEnemyConfig _blob
    SpearEnemy        -> (_hurtEffectData :: SpearEnemyConfig -> EnemyHurtEffectData) <$> readEnemyConfig _spear
    TurretEnemy       -> (_hurtEffectData :: TurretEnemyConfig -> EnemyHurtEffectData) <$> readEnemyConfig _turret
    BubbleTurretEnemy ->
        (_hurtEffectData :: BubbleTurretEnemyConfig -> EnemyHurtEffectData) <$> readEnemyConfig _bubbleTurret
    LankyEnemy        -> (_hurtEffectData :: LankyEnemyConfig -> EnemyHurtEffectData) <$> readEnemyConfig _lanky
    BatEnemy          -> (_hurtEffectData :: BatEnemyConfig -> EnemyHurtEffectData) <$> readEnemyConfig _bat
    BombEnemy         -> (_hurtEffectData :: BombEnemyConfig -> EnemyHurtEffectData) <$> readEnemyConfig _bomb
    ClawsEnemy        -> (_hurtEffectData :: ClawsEnemyConfig -> EnemyHurtEffectData) <$> readEnemyConfig _claws
    FlailEnemy        -> (_hurtEffectData :: FlailEnemyConfig -> EnemyHurtEffectData) <$> readEnemyConfig _flail
    ZombieEnemy       -> (_hurtEffectData :: ZombieEnemyConfig -> EnemyHurtEffectData) <$> readEnemyConfig _zombie
    WallEnemy         -> (_hurtEffectData :: WallEnemyConfig -> EnemyHurtEffectData) <$> readEnemyConfig _wall
    HopEnemy          -> (_hurtEffectData :: HopEnemyConfig -> EnemyHurtEffectData) <$> readEnemyConfig _hop
    BossEnemy         -> (_hurtEffectData :: BossEnemyConfig -> EnemyHurtEffectData) <$> readEnemyConfig _boss
