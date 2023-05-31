module Enemy.All.Flying.Util
    ( attackCooldownMultiplier
    ) where

import Configs.All.Enemy
import Configs.All.Enemy.Flying
import Enemy
import Enemy.All.Flying.Data

attackCooldownMultiplier :: Enemy FlyingEnemyData -> Float
attackCooldownMultiplier enemy = case enemyTauntedStatus enemy of
    EnemyTauntedInactive -> 1.0
    EnemyTauntedActive   -> _tauntedCooldownMultiplier . _flying . _config . _data $ enemy
