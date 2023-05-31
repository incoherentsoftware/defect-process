module Enemy.All.BubbleTurret.Util
    ( attackCooldownMultiplier
    ) where

import Configs.All.Enemy
import Configs.All.Enemy.BubbleTurret
import Enemy
import Enemy.All.BubbleTurret.Data

attackCooldownMultiplier :: Enemy BubbleTurretEnemyData -> Float
attackCooldownMultiplier enemy = case enemyTauntedStatus enemy of
    EnemyTauntedInactive -> 1.0
    EnemyTauntedActive   -> _tauntedCooldownMultiplier . _bubbleTurret . _config . _data $ enemy
