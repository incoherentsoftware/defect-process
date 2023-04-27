module Configs.All.Enemy
    ( EnemyConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)

import Attack.Util
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
import Util

data EnemyConfig = EnemyConfig
    { _defaultHealth           :: Health
    , _sightRange              :: Distance
    , _gravity                 :: Float
    , _minHangtimeSecs         :: Secs
    , _maxHangtimeSpeedX       :: Speed
    , _minHangtimeVelY         :: VelY
    , _maxHangtimeVelY         :: VelY
    , _minWallSplatImpactSpeed :: Speed
    , _minWallSplatSecs        :: Secs
    , _minFallenSecs           :: Secs
    , _minHurtSecs             :: Secs
    , _hitstunLogBase          :: Float
    , _tauntedDamageMultiplier :: Float

    , _axe          :: AxeEnemyConfig
    , _bat          :: BatEnemyConfig
    , _blob         :: BlobEnemyConfig
    , _bomb         :: BombEnemyConfig
    , _boss         :: BossEnemyConfig
    , _bubbleTurret :: BubbleTurretEnemyConfig
    , _claws        :: ClawsEnemyConfig
    , _dog          :: DogEnemyConfig
    , _flail        :: FlailEnemyConfig
    , _flying       :: FlyingEnemyConfig
    , _giant        :: GiantEnemyConfig
    , _hammer       :: HammerEnemyConfig
    , _hop          :: HopEnemyConfig
    , _lanky        :: LankyEnemyConfig
    , _spear        :: SpearEnemyConfig
    , _turret       :: TurretEnemyConfig
    , _wall         :: WallEnemyConfig
    , _zombie       :: ZombieEnemyConfig
    }
    deriving Generic

instance FromJSON EnemyConfig where
    parseJSON = genericParseJSON aesonFieldDropUnderscore
