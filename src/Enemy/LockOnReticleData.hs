module Enemy.LockOnReticleData
    ( EnemyLockOnReticleData(..)
    , dummyLockOnReticleData
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)
import qualified Data.Map as M

import Util

data EnemyLockOnReticleData = EnemyLockOnReticleData
    { _scale     :: Float
    , _offset    :: Pos2
    , _offsetMap :: Maybe (M.Map String [Pos2])
    }
    deriving Generic

instance FromJSON EnemyLockOnReticleData where
    parseJSON = genericParseJSON aesonFieldDropUnderscore

dummyLockOnReticleData = EnemyLockOnReticleData
    { _scale     = 1.0
    , _offset    = zeroPos2
    , _offsetMap = Nothing
    } :: EnemyLockOnReticleData
