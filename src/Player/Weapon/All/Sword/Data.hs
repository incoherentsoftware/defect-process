module Player.Weapon.All.Sword.Data
    ( SwordChargeStatus(..)
    , swordChargeStatusChargeSecs
    , SwordAttackDescriptions(..)
    , SwordData(..)
    , mkSwordData
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack
import Configs
import Configs.All.PlayerWeapon
import Configs.All.PlayerWeapon.Sword
import FileCache
import Id
import Util
import Window.Graphics

packFilePath = "data/player/weapons/sword.pack" :: FilePath

data SwordChargeStatus
    = SwordNoChargeStatus
    | SwordPartialChargeStatus Secs
    | SwordFullChargeStatus
    deriving Eq

swordChargeStatusChargeSecs :: SwordChargeStatus -> Secs
swordChargeStatusChargeSecs = \case
    SwordNoChargeStatus                 -> 0.0
    SwordPartialChargeStatus chargeSecs -> chargeSecs
    SwordFullChargeStatus               -> maxSecs

data SwordAttackDescriptions = SwordAttackDescriptions
    { _slash1                  :: AttackDescription
    , _slash2                  :: AttackDescription
    , _slash2Heavy             :: AttackDescription
    , _slash3                  :: AttackDescription
    , _slash3Aoe               :: AttackDescription
    , _fallSlash               :: AttackDescription
    , _fallSlashLand           :: AttackDescription
    , _upSlash                 :: AttackDescription
    , _airSlash1               :: AttackDescription
    , _airSlash2               :: AttackDescription
    , _airDownSlash            :: AttackDescription
    , _airSlash3               :: AttackDescription
    , _flurryStab              :: AttackDescription
    , _flurryThrust            :: AttackDescription
    , _chargeRelease           :: AttackDescription
    , _airChargeRelease        :: AttackDescription
    , _chargeReleaseProjectile :: AttackDescription
    , _summonAttackOrb         :: AttackDescription
    , _attackOrbActivate       :: AttackDescription
    }

mkSwordAttackDescriptions :: forall m. (FileCache m, GraphicsRead m, MonadIO m) => m SwordAttackDescriptions
mkSwordAttackDescriptions =
    SwordAttackDescriptions <$>
    loadAtkDesc "slash1-.atk" <*>
    loadAtkDesc "slash2-.atk" <*>
    loadAtkDesc "slash2-heavy.atk" <*>
    loadAtkDesc "slash3-.atk" <*>
    loadAtkDesc "slash3-aoe.atk" <*>
    loadAtkDesc "fall-slash.atk" <*>
    loadAtkDesc "fall-slash-land.atk" <*>
    loadAtkDesc "up-slash.atk" <*>
    loadAtkDesc "air-slash1-.atk" <*>
    loadAtkDesc "air-slash2-.atk" <*>
    loadAtkDesc "air-down-slash.atk" <*>
    loadAtkDesc "air-slash3-.atk" <*>
    loadAtkDesc "flurry-stab.atk" <*>
    loadAtkDesc "flurry-thrust.atk" <*>
    loadAtkDesc "charge-release.atk" <*>
    loadAtkDesc "air-charge-release.atk" <*>
    loadAtkDesc "charge-release-projectile.atk" <*>
    loadAtkDesc "summon-attack-orb.atk" <*>
    loadAtkDesc "attack-orb-activate.atk"
    where loadAtkDesc = \f -> loadPackAttackDescription $ PackResourceFilePath packFilePath f

data SwordData = SwordData
    { _prevChargeStatus    :: SwordChargeStatus
    , _chargeStatus        :: SwordChargeStatus
    , _chargeOverlaySpr    :: Sprite
    , _chargeSoundHashedId :: HashedId
    , _attackDescriptions  :: SwordAttackDescriptions
    , _config              :: SwordConfig
    }

mkSwordData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m SwordData
mkSwordData = do
    chargeOverlaySpr    <- loadPackSprite $ PackResourceFilePath packFilePath "charge-overlay.spr"
    chargeSoundHashedId <- hashId <$> newId
    wpnAtkDescs         <- mkSwordAttackDescriptions
    cfg                 <- readConfig _playerWeapon _sword

    return $ SwordData
        { _prevChargeStatus    = SwordNoChargeStatus
        , _chargeStatus        = SwordNoChargeStatus
        , _chargeOverlaySpr    = chargeOverlaySpr
        , _chargeSoundHashedId = chargeSoundHashedId
        , _attackDescriptions  = wpnAtkDescs
        , _config              = cfg
        }
