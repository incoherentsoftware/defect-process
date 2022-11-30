module Player.Gun.All.ShardGun.Data.Types
    ( ShardGunStAttackDescription(..)
    , ShardGunStImages(..)
    , ShardGunData(..)
    ) where

import qualified Data.List.NonEmpty as NE

import Attack.Description.Types
import Configs.All.PlayerGun.ShardGun
import Msg.Types
import Util
import Window.Graphics

data ShardGunStAttackDescription = ShardGunStAttackDescription
    { _playerBlinkStrikes    :: NE.NonEmpty AttackDescription
    , _shardExplosionAtkDesc :: AttackDescription
    }

data ShardGunStImages = ShardGunStImages
    { _shardImpale :: Image
    }

data ShardGunData = ShardGunData
    { _gunMsgId             :: MsgId
    , _aimAngle             :: Radians
    , _blinkStrikePosMsgIds :: [(Pos2, MsgId)]
    , _attackDescription    :: ShardGunStAttackDescription
    , _images               :: ShardGunStImages
    , _config               :: ShardGunConfig
    }
