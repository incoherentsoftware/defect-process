module Enemy.All.Spear.Util
    ( attackCooldownMultiplier
    ) where

import Configs.All.Enemy
import Configs.All.Enemy.Spear
import Enemy
import Enemy.All.Spear.Data

attackCooldownMultiplier :: Enemy SpearEnemyData -> Float
attackCooldownMultiplier enemy = case enemyTauntedStatus enemy of
    EnemyTauntedInactive -> 1.0
    EnemyTauntedActive   -> _tauntedAtkCooldownMultiplier . _spear . _config . _data $ enemy
