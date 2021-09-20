module Enemy.Flags
    ( EnemyFlags(..)
    , mkEnemyFlags
    , clearEnemyFlags
    ) where

import Util

data EnemyFlags = EnemyFlags
    { _dead              :: Bool
    , _touchingGround    :: Bool
    , _touchingLeftWall  :: Bool
    , _touchingRightWall :: Bool
    , _willFallOffGround :: Bool
    , _justGotHit        :: Maybe Pos2
    }

mkEnemyFlags :: EnemyFlags
mkEnemyFlags = EnemyFlags
    { _dead              = False
    , _touchingGround    = False
    , _touchingLeftWall  = False
    , _touchingRightWall = False
    , _willFallOffGround = False
    , _justGotHit        = Nothing
    }

-- the following flags should be cleared at the beginning of each frame
clearEnemyFlags :: EnemyFlags -> EnemyFlags
clearEnemyFlags flags = flags
    { _touchingGround    = False
    , _touchingLeftWall  = False
    , _touchingRightWall = False
    , _willFallOffGround = False
    , _justGotHit        = Nothing
    }
