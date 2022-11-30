module Player.Weapon.All.Staff.Data
    ( StaffChargeStatus(..)
    , staffChargeStatusChargeSecs
    , StaffAttackDescriptions(..)
    , StaffData(..)
    , mkStaffData
    , updateAirStrikeOnHit
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Vector as V

import Attack
import Configs
import Configs.All.PlayerWeapon
import Configs.All.PlayerWeapon.Staff
import FileCache
import Id
import Msg
import Player.Weapon
import Util
import Window.Graphics

packFilePath            = "data/player/weapons/staff.pack"                     :: FilePath
surfaceHitSoundFilePath = "event:/SFX Events/Player/Weapons/Staff/surface-hit" :: FilePath

data StaffChargeStatus
    = StaffNoChargeStatus
    | StaffPartialChargeStatus Secs
    | StaffFullChargeStatus
    deriving Eq

staffChargeStatusChargeSecs :: StaffChargeStatus -> Secs
staffChargeStatusChargeSecs = \case
    StaffNoChargeStatus                 -> 0.0
    StaffPartialChargeStatus chargeSecs -> chargeSecs
    StaffFullChargeStatus               -> maxSecs

data StaffAttackDescriptions = StaffAttackDescriptions
    { _strike1                  :: AttackDescription
    , _strike2                  :: AttackDescription
    , _strike2Launch            :: AttackDescription
    , _strike3                  :: AttackDescription
    , _strike3Launch            :: AttackDescription
    , _upSpinStrike1            :: AttackDescription
    , _upSpinStrike2            :: AttackDescription
    , _upSpinStrike3            :: AttackDescription
    , _groundStrike             :: AttackDescription
    , _dashLaunchStrike         :: AttackDescription
    , _parabolicStrike          :: AttackDescription
    , _parabolicStrikeLand      :: AttackDescription
    , _airStrike                :: AttackDescription
    , _airLiftStrike            :: AttackDescription
    , _groundStrikeEffect       :: AttackDescription
    , _windProjectileAppearIdle :: AttackDescription
    }

mkStaffAttackDescriptions :: (FileCache m, GraphicsRead m, MonadIO m) => m StaffAttackDescriptions
mkStaffAttackDescriptions =
    StaffAttackDescriptions <$>
    loadAtkDesc "strike1-.atk" <*>
    loadAtkDesc "strike2-.atk" <*>
    loadAtkDesc "strike2-launch.atk" <*>
    loadAtkDesc "strike3-.atk" <*>
    loadAtkDesc "strike3-launch.atk" <*>
    loadAtkDesc "up-spin-strike1-.atk" <*>
    loadAtkDesc "up-spin-strike2-.atk" <*>
    loadAtkDesc "up-spin-strike3-.atk" <*>
    loadAtkDesc "ground-strike.atk" <*>
    loadAtkDesc "dash-launch-strike.atk" <*>
    loadAtkDesc "parabolic-strike.atk" <*>
    loadAtkDesc "parabolic-strike-land.atk" <*>
    loadAtkDesc "air-strike.atk" <*>
    loadAtkDesc "air-lift-strike.atk" <*>
    loadAtkDesc "ground-strike-effect.atk" <*>
    loadAtkDesc "wind-projectile-appear-idle.atk"
    where loadAtkDesc = \f -> loadPackAttackDescription $ PackResourceFilePath packFilePath f

data StaffData = StaffData
    { _airStrikeCount      :: Int
    , _prevChargeStatus    :: StaffChargeStatus
    , _chargeStatus        :: StaffChargeStatus
    , _chargeOverlaySpr    :: Sprite
    , _chargeSoundHashedId :: HashedId
    , _attackDescriptions  :: StaffAttackDescriptions
    , _config              :: StaffConfig
    }

mkStaffData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m StaffData
mkStaffData = do
    atkDescs            <- mkStaffAttackDescriptions
    chargeOverlaySpr    <- loadPackSprite $ PackResourceFilePath packFilePath "charge-overlay.spr"
    chargeSoundHashedId <- hashId <$> newId
    cfg                 <- readConfig _playerWeapon _staff

    return $ StaffData
        { _airStrikeCount      = 0
        , _prevChargeStatus    = StaffNoChargeStatus
        , _chargeStatus        = StaffNoChargeStatus
        , _chargeOverlaySpr    = chargeOverlaySpr
        , _chargeSoundHashedId = chargeSoundHashedId
        , _attackDescriptions  = atkDescs
        , _config              = cfg
        }

updateAirStrikeOnHit :: StaffData -> Attack -> Attack
updateAirStrikeOnHit staffData airStrikeAtk = airStrikeAtk
    { _description = (_description airStrikeAtk)
        { _onHitType        = AddedOnHitType (\_ _ -> onHit)
        , _onSurfaceHitType = WithSurfaceHitType (\_ -> onHit)
        }
    }
    where
        airStrikeCount = _airStrikeCount staffData
        cfg            = _config staffData
        updateVel      = case _bounceVelsY cfg V.!? airStrikeCount of
            Just bounceVelY -> \(Vel2 velX _) -> Vel2 velX bounceVelY
            Nothing         -> id

        clearOnHitBounce = \a -> a
            { _description = (_description a)
                { _onHitType        = NormalOnHitType
                , _onSurfaceHitType = NoSurfaceHitType
                }
            }

        updateAirStrikeCount = \w -> w
            { _data = (_data w) {_airStrikeCount = airStrikeCount + 1}
            }

        onHit :: Attack -> [Msg ThinkCollisionMsgsPhase]
        onHit atk =
            [ mkMsg $ PlayerMsgUpdateVelocity updateVel
            , mkMsg $ PlayerMsgUpdateWeapon updateAirStrikeCount
            , mkMsg $ PlayerMsgUpdateAttack clearOnHitBounce
            , mkMsg $ WorldMsgScreenshake (_bounceScreenshakeMagnitude cfg)
            , mkMsg $ AudioMsgPlaySound surfaceHitSoundFilePath (_pos atk)
            ]
