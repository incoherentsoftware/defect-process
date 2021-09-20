module Attack.Types
    ( Attack(..)
    ) where

import Attack.Description.Types
import Id
import Util

data Attack = Attack
    { _id               :: Id Attack
    , _refreshIds       :: [Id Attack]
    , _pos              :: Pos2
    , _dir              :: Direction
    , _angle            :: Radians
    , _launchTargetY    :: Maybe PosY
    , _done             :: Bool
    , _description      :: AttackDescription
    , _deferUpdate      :: Bool
    , _nextAttackOnDone :: Maybe Attack
    , _soundHashedId    :: HashedId
    }
