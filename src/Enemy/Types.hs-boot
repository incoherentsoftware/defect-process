module Enemy.Types
    ( EnemyType
    ) where

import Data.Aeson.Types (FromJSON)

data EnemyType
instance Eq EnemyType
instance Ord EnemyType
instance FromJSON EnemyType
instance Show EnemyType
