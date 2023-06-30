module Enemy.All.Wall.Util
    ( attackCooldownMultiplier
    ) where

import Configs.All.Enemy
import Configs.All.Enemy.Wall
import Enemy
import Enemy.All.Wall.Data

attackCooldownMultiplier :: Enemy WallEnemyData -> Float
attackCooldownMultiplier enemy = case enemyTauntedStatus enemy of
    EnemyTauntedInactive -> 1.0
    EnemyTauntedActive   -> _tauntedAtkCooldownMultiplier . _wall . _config . _data $ enemy
