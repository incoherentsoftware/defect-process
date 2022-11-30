module Player.Gun.All.Shotgun.Data
    ( module Player.Gun.All.Shotgun.Data.Types
    , mkShotgunData
    , shotgunDataDecreaseCd
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.List.NonEmpty as NE

import Configs
import Configs.All.PlayerGun
import Configs.All.PlayerGun.Shotgun
import Constants
import FileCache
import Id
import Player.Gun.All.Shotgun.Data.Types
import Player.Gun.MuzzleFlash
import Util
import Window.Graphics

muzzleFlashSpriteFileNames = NE.fromList
    [ "shotgun-muzzle-flash-a.spr"
    , "shotgun-muzzle-flash-b.spr"
    , "shotgun-muzzle-flash-c.spr"
    ] :: NE.NonEmpty FileName

burnMuzzleFlashSpriteFileNames = NE.fromList
    [ "shotgun-burn-muzzle-flash-a.spr"
    , "shotgun-burn-muzzle-flash-b.spr"
    , "shotgun-burn-muzzle-flash-c.spr"
    ] :: NE.NonEmpty FileName

loadNormalMuzzleFlash :: (FileCache m, GraphicsRead m, MonadIO m) => ShotgunConfig -> m MuzzleFlash
loadNormalMuzzleFlash cfg = do
    let loadPackSpr  = \f -> loadPackSprite $ PackResourceFilePath "data/player/player-guns.pack" f
    muzzleFlashSprs <- traverse loadPackSpr muzzleFlashSpriteFileNames
    return $ mkMuzzleFlash LeadArmMuzzleFlash (_muzzleFlashOffset cfg) muzzleFlashSprs

loadBurnMuzzleFlash :: (FileCache m, GraphicsRead m, MonadIO m) => ShotgunConfig -> m MuzzleFlash
loadBurnMuzzleFlash cfg = do
    let loadPackSpr      = \f -> loadPackSprite $ PackResourceFilePath "data/player/player-guns.pack" f
    burnMuzzleFlashSprs <- traverse loadPackSpr burnMuzzleFlashSpriteFileNames
    return $ mkMuzzleFlash LeadArmMuzzleFlash (_muzzleFlashOffset cfg) burnMuzzleFlashSprs

mkShotgunData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m ShotgunData
mkShotgunData = do
    burnShotOwnerId   <- newId
    cfg               <- readConfig _playerGun _shotgun
    normalMuzzleFlash <- loadNormalMuzzleFlash cfg
    burnMuzzleFlash   <- loadBurnMuzzleFlash cfg

    return $ ShotgunData
        { _status            = ReadyStatus
        , _cooldown          = 0.0
        , _burnShotOwnerId   = burnShotOwnerId
        , _normalMuzzleFlash = normalMuzzleFlash
        , _burnMuzzleFlash   = burnMuzzleFlash
        , _config            = cfg
        }

shotgunDataDecreaseCd :: ShotgunData -> ShotgunData
shotgunDataDecreaseCd shotgunData = shotgunData {_cooldown = cooldown'}
    where cooldown' = _cooldown shotgunData - timeStep
