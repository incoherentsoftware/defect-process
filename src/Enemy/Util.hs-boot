module Enemy.Util
    ( EnemyLockOnReticleData
    , EnemyLockOnData
    , EnemyHurtEffectData
    , EnemyDeathEffectData
    , EnemySpawnEffectData
    ) where

import Data.Aeson.Types (FromJSON)

data EnemyLockOnReticleData
instance FromJSON EnemyLockOnReticleData

data EnemyLockOnData

data EnemyHurtEffectData
instance FromJSON EnemyHurtEffectData

data EnemyDeathEffectData
instance FromJSON EnemyDeathEffectData

data EnemySpawnEffectData
instance FromJSON EnemySpawnEffectData
