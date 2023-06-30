module Enemy.All.Lanky.Util
    ( attackCooldownMultiplier
    ) where

import Configs.All.Enemy
import Configs.All.Enemy.Lanky
import Enemy
import Enemy.All.Lanky.Data

attackCooldownMultiplier :: Enemy LankyEnemyData -> Float
attackCooldownMultiplier enemy = case enemyTauntedStatus enemy of
    EnemyTauntedInactive -> 1.0
    EnemyTauntedActive   -> _tauntedAtkCooldownMultiplier . _lanky . _config . _data $ enemy
