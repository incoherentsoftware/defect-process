module Player.Weapon.All.SpiritBlade.Data
    ( SpiritBladeAttackDescriptions(..)
    , SpiritBladeSprites(..)
    , SpiritFormMsgIds(..)
    , SpiritBladeData(..)
    , mkSpiritBladeData
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Map as M

import Attack
import Configs
import Configs.All.PlayerWeapon
import Configs.All.PlayerWeapon.SpiritBlade
import FileCache
import Id
import Msg
import Util
import Window.Graphics

packFilePath       = "data/player/weapons/spirit-blade.pack"        :: FilePath
packAppearFilePath = "data/player/weapons/spirit-blade-appear.pack" :: FilePath

data SpiritBladeAttackDescriptions = SpiritBladeAttackDescriptions
    { _doubleSlash1                 :: AttackDescription
    , _doubleSlash2                 :: AttackDescription
    , _knockbackSlash               :: AttackDescription
    , _spinSlash                    :: AttackDescription
    , _advancingSlash               :: AttackDescription
    , _advancingSlashFollowup       :: AttackDescription
    , _airDoubleSlash1              :: AttackDescription
    , _airDoubleSlash2              :: AttackDescription
    , _airDownThrust                :: AttackDescription
    , _spiritLargeSlashAppear       :: AttackDescription
    , _spiritLargeSlash             :: AttackDescription
    , _spiritLaunchSlashAppear      :: AttackDescription
    , _spiritLaunchSlash            :: AttackDescription
    , _spiritBackwardsSlashAppear   :: AttackDescription
    , _spiritBackwardsSlash         :: AttackDescription
    , _spiritPillarBlastAppear      :: AttackDescription
    , _spiritPillarBlast            :: AttackDescription
    , _spiritAirBlowAppear          :: AttackDescription
    , _spiritAirBlow                :: AttackDescription
    , _spiritAirCircularSlashAppear :: AttackDescription
    , _spiritAirCircularSlash       :: AttackDescription
    }

mkSpiritBladeAttackDescriptions :: (FileCache m, GraphicsRead m, MonadIO m) => m SpiritBladeAttackDescriptions
mkSpiritBladeAttackDescriptions =
    SpiritBladeAttackDescriptions <$>
    loadAtkDesc "double-slash1-.atk" <*>
    loadAtkDesc "double-slash2-.atk" <*>
    loadAtkDesc "knockback-slash.atk" <*>
    loadAtkDesc "spin-slash.atk" <*>
    loadAtkDesc "advancing-slash.atk" <*>
    loadAtkDesc "advancing-slash-followup.atk" <*>
    loadAtkDesc "air-double-slash1-.atk" <*>
    loadAtkDesc "air-double-slash2-.atk" <*>
    loadAtkDesc "air-down-thrust.atk" <*>
    loadAppearAtkDesc "spirit-large-slash-appear.atk" <*>
    loadAtkDesc "spirit-large-slash.atk" <*>
    loadAppearAtkDesc "spirit-launch-slash-appear.atk" <*>
    loadAtkDesc "spirit-launch-slash.atk" <*>
    loadAppearAtkDesc "spirit-backwards-slash-appear.atk" <*>
    loadAtkDesc "spirit-backwards-slash.atk" <*>
    loadAppearAtkDesc "spirit-pillar-blast-appear.atk" <*>
    loadAtkDesc "spirit-pillar-blast.atk" <*>
    loadAppearAtkDesc "spirit-air-blow-appear.atk" <*>
    loadAtkDesc "spirit-air-blow.atk" <*>
    loadAppearAtkDesc "spirit-air-circular-slash-appear.atk" <*>
    loadAtkDesc "spirit-air-circular-slash.atk"
    where
        loadAtkDesc       = \f -> loadPackAttackDescription $ PackResourceFilePath packFilePath f
        loadAppearAtkDesc = \f -> loadPackAttackDescription $ PackResourceFilePath packAppearFilePath f

data SpiritBladeSprites = SpiritBladeSprites
    { _activateEffect :: Sprite
    }

mkSpiritBladeSprites :: (FileCache m, GraphicsRead m, MonadIO m) => m SpiritBladeSprites
mkSpiritBladeSprites =
    SpiritBladeSprites <$>
    loadPackSpr "activate-effect.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath packFilePath f

data SpiritFormMsgIds = SpiritFormMsgIds
    { _largeSlashId       :: MsgId
    , _launchSlashId      :: MsgId
    , _backwardsSlashId   :: MsgId
    , _pillarBlastId      :: MsgId
    , _airBlowId          :: MsgId
    , _airCircularSlashId :: MsgId
    }

mkSpiritFormMsgIds :: SpiritFormMsgIds
mkSpiritFormMsgIds = SpiritFormMsgIds
    { _largeSlashId       = NullId
    , _launchSlashId      = NullId
    , _backwardsSlashId   = NullId
    , _pillarBlastId      = NullId
    , _airBlowId          = NullId
    , _airCircularSlashId = NullId
    }

data SpiritBladeData = SpiritBladeData
    { _isInputHeldForSpiritForm :: Bool
    , _overlaySprite            :: Maybe Sprite
    , _lastSummonAttackId       :: Id Attack
    , _spiritFormMsgIds         :: SpiritFormMsgIds
    , _spiritFormMsgIdTimeouts  :: M.Map MsgId Secs
    , _checkedWallOffsetX       :: OffsetX
    , _attackDescriptions       :: SpiritBladeAttackDescriptions
    , _sprites                  :: SpiritBladeSprites
    , _config                   :: SpiritBladeConfig
    }

mkSpiritBladeData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m SpiritBladeData
mkSpiritBladeData = do
    wpnAtkDescs <- mkSpiritBladeAttackDescriptions
    sprs        <- mkSpiritBladeSprites
    cfg         <- readConfig _playerWeapon _spiritBlade

    return $ SpiritBladeData
        { _isInputHeldForSpiritForm = False
        , _overlaySprite            = Nothing
        , _lastSummonAttackId       = NullId
        , _spiritFormMsgIds         = mkSpiritFormMsgIds
        , _spiritFormMsgIdTimeouts  = M.empty
        , _checkedWallOffsetX       = 0.0
        , _attackDescriptions       = wpnAtkDescs
        , _sprites                  = sprs
        , _config                   = cfg
        }
