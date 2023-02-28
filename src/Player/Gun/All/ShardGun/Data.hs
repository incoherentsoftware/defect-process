module Player.Gun.All.ShardGun.Data
    ( module Player.Gun.All.ShardGun.Data.Types
    , mkShardGunData
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.List.NonEmpty as NE

import Attack.Description
import Configs
import Configs.All.PlayerGun
import FileCache
import Id
import Player.Gun.All.ShardGun.Data.Types
import Util
import Window.Graphics

packFilePath         = "data/player/player-guns.pack" :: FilePath
blinkStrikeFileNames = "shard-gun-blink-strike-a.atk" NE.:|
    [ "shard-gun-blink-strike-b.atk"
    , "shard-gun-blink-strike-c.atk"
    , "shard-gun-blink-strike-d.atk"
    ] :: NE.NonEmpty FileName

mkShardGunStAttackDescription :: (FileCache m, GraphicsRead m, MonadIO m) => m ShardGunStAttackDescription
mkShardGunStAttackDescription = do
    let loadPackAtkDesc = \f -> loadPackAttackDescription $ PackResourceFilePath packFilePath f

    blinkStrikeAtkDescs   <- traverse loadPackAtkDesc blinkStrikeFileNames
    shardExplosionAtkDesc <- loadPackAtkDesc "shard-pre-explosion.atk"

    return $ ShardGunStAttackDescription
        { _playerBlinkStrikes    = blinkStrikeAtkDescs
        , _shardExplosionAtkDesc = shardExplosionAtkDesc
        }

mkShardGunStImages :: (FileCache m, GraphicsRead m, MonadIO m) => m ShardGunStImages
mkShardGunStImages =
    ShardGunStImages <$>
    loadPackImage (PackResourceFilePath packFilePath "shard-impale.image")

mkShardGunData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m ShardGunData
mkShardGunData = do
    msgId   <- newId
    atkDesc <- mkShardGunStAttackDescription
    imgs    <- mkShardGunStImages
    cfg     <- readConfig _playerGun _shardGun

    return $ ShardGunData
        { _gunMsgId                  = msgId
        , _aimAngle                  = 0.0
        , _blinkStrikePosMsgIds      = []
        , _blinkStrikePlayerStartPos = Nothing
        , _attackDescription         = atkDesc
        , _images                    = imgs
        , _config                    = cfg
        }
