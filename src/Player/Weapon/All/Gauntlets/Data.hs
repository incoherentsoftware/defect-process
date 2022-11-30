module Player.Weapon.All.Gauntlets.Data
    ( GauntletsAttackDescriptions(..)
    , GauntletsDataSprites(..)
    , GauntletsData(..)
    , mkGauntletsData
    , isReleasableAtkConditionsMet
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Functor           ((<&>))
import qualified Data.Map as M

import Attack
import Configs
import Configs.All.PlayerWeapon
import Configs.All.PlayerWeapon.Gauntlets
import FileCache
import Id
import Msg
import Player.Weapon
import Window.Graphics

packFilePath = "data/player/weapons/gauntlets.pack" :: FilePath

data GauntletsAttackDescriptions = GauntletsAttackDescriptions
    { _punch1                :: AttackDescription
    , _punch2                :: AttackDescription
    , _doubleKick1           :: AttackDescription
    , _doubleKick2           :: AttackDescription
    , _punch3                :: AttackDescription
    , _kick                  :: AttackDescription
    , _risingUppercut        :: AttackDescription
    , _evasiveKick           :: AttackDescription
    , _evasiveKickProjectile :: AttackDescription
    , _dashPunchForwards     :: AttackDescription
    , _dashPunch             :: AttackDescription
    , _airPunch1             :: AttackDescription
    , _airForwardsKick1      :: AttackDescription
    , _airForwardsKick2      :: AttackDescription
    , _airPunch2             :: AttackDescription
    , _airPunch3             :: AttackDescription
    , _diveKick              :: AttackDescription
    , _diveKickBounce        :: AttackDescription
    , _diveKickLand          :: AttackDescription
    , _palmExplodeRelease    :: AttackDescription
    , _groundPunchRelease    :: AttackDescription
    , _flyingKickRelease     :: AttackDescription
    , _fallingAxeKickRelease :: AttackDescription
    , _fallingAxeKickLand    :: AttackDescription
    , _diveKickRelease       :: AttackDescription
    , _fall                  :: AttackDescription
    }

mkGauntletsAttackDescriptions :: (FileCache m, GraphicsRead m, MonadIO m) => m GauntletsAttackDescriptions
mkGauntletsAttackDescriptions =
    let
        loadAtkDesc :: forall m. (FileCache m, GraphicsRead m, MonadIO m) => FilePath -> m AttackDescription
        loadAtkDesc f = loadPackAttackDescription $ PackResourceFilePath packFilePath f

        mkAtkOnHit :: (Weapon GauntletsData -> Weapon GauntletsData) -> AttackOnHit
        mkAtkOnHit onHit = \_ _ _ -> [mkMsg $ PlayerMsgUpdateWeapon onHit]

        releasableAtkOnHit :: Weapon GauntletsData -> Weapon GauntletsData
        releasableAtkOnHit w = w
            { _data = (_data w) {_attackHitEnemy = True}
            }

        loadReleasableAtkDesc :: forall m. (FileCache m, GraphicsRead m, MonadIO m) => FilePath -> m AttackDescription
        loadReleasableAtkDesc f = loadAtkDesc f <&> \a -> a
            { _onHitType = AddedOnHitType $ mkAtkOnHit releasableAtkOnHit
            }

        loadDashPunchForwards = loadAtkDesc "dash-punch-forwards.atk" <&> \a -> a
            { _onHitType            = ReplacedOnHitType . mkAtkOnHit $ \w -> w
                { _data = (_data w) {_enemyInAttackRange = True}
                }
            , _nextAttackDescOnDone = _nextAttackDescOnDone a <&> \nextA ->
                nextA {_onHitType = AddedOnHitType $ mkAtkOnHit releasableAtkOnHit}
            }
    in do
        diveKickBounce   <- loadReleasableAtkDesc "dive-kick-bounce.atk"
        airForwardsKick2 <- loadReleasableAtkDesc "air-forwards-kick2-.atk"

        let
            loadDiveKick = loadAtkDesc "dive-kick.atk" <&> \a -> a
                { _onHitType = AddedOnHitType $ \_ _ _ ->
                    [ mkMsg $ PlayerMsgSetAttackDesc diveKickBounce
                    , mkMsg $ PlayerMsgUpdateWeapon releasableAtkOnHit
                    ]
                }

            loadAirForwardsKick1 = loadAtkDesc "air-forwards-kick1-.atk" <&> \a -> a
                { _onHitType = AddedOnHitType $ \_ _ _ ->
                    [ mkMsg (PlayerMsgSetAttackDesc airForwardsKick2)
                    , mkMsg $ PlayerMsgUpdateWeapon releasableAtkOnHit
                    ]
                }

        id $
            GauntletsAttackDescriptions <$>
            loadReleasableAtkDesc "punch1-.atk" <*>
            loadReleasableAtkDesc "punch2-.atk" <*>
            loadReleasableAtkDesc "double-kick1-.atk" <*>
            loadReleasableAtkDesc "double-kick2-.atk" <*>
            loadReleasableAtkDesc "punch3-.atk" <*>
            loadReleasableAtkDesc "kick.atk" <*>
            loadReleasableAtkDesc "rising-uppercut.atk" <*>
            loadReleasableAtkDesc "evasive-kick.atk" <*>
            loadReleasableAtkDesc "evasive-kick-projectile.atk" <*>
            loadDashPunchForwards <*>
            loadReleasableAtkDesc "dash-punch.atk" <*>
            loadReleasableAtkDesc "air-punch1-.atk" <*>
            loadAirForwardsKick1 <*>
            pure airForwardsKick2 <*>
            loadReleasableAtkDesc "air-punch2-.atk" <*>
            loadReleasableAtkDesc "air-punch3-.atk" <*>
            loadDiveKick <*>
            pure diveKickBounce <*>
            loadReleasableAtkDesc "dive-kick-land.atk" <*>
            loadAtkDesc "palm-explode-release.atk" <*>
            loadAtkDesc "ground-punch-release.atk" <*>
            loadAtkDesc "flying-kick-release.atk" <*>
            loadAtkDesc "falling-axe-kick-release.atk" <*>
            loadAtkDesc "falling-axe-kick-land.atk" <*>
            loadAtkDesc "dive-kick-release.atk" <*>
            loadAtkDesc "fall.atk"

data GauntletsDataSprites = GauntletsDataSprites
    { _punch1Indicator           :: Sprite
    , _punch2Indicator           :: Sprite
    , _doubleKick2Indicator      :: Sprite
    , _punch3Indicator           :: Sprite
    , _kickIndicator             :: Sprite
    , _risingUppercutIndicator   :: Sprite
    , _dashPunchIndicator        :: Sprite
    , _evasiveKickIndicator      :: Sprite
    , _airPunch1Indicator        :: Sprite
    , _airPunch2Indicator        :: Sprite
    , _airForwardsKick2Indicator :: Sprite
    , _airPunch3Indicator        :: Sprite
    , _diveKickBounceIndicator   :: Sprite
    , _diveKickLandIndicator     :: Sprite
    }

mkGauntletsDataSprites :: (FileCache m, GraphicsRead m, MonadIO m) => m GauntletsDataSprites
mkGauntletsDataSprites =
    GauntletsDataSprites <$>
    loadPackSpr "punch1-indicator.spr" <*>
    loadPackSpr "punch2-indicator.spr" <*>
    loadPackSpr "double-kick2-indicator.spr" <*>
    loadPackSpr "punch3-indicator.spr" <*>
    loadPackSpr "kick-indicator.spr" <*>
    loadPackSpr "rising-uppercut-indicator.spr" <*>
    loadPackSpr "dash-punch-indicator.spr" <*>
    loadPackSpr "evasive-kick-indicator.spr" <*>
    loadPackSpr "air-punch1-indicator.spr" <*>
    loadPackSpr "air-punch2-indicator.spr" <*>
    loadPackSpr "air-forwards-kick2-indicator.spr" <*>
    loadPackSpr "air-punch3-indicator.spr" <*>
    loadPackSpr "dive-kick-bounce-indicator.spr" <*>
    loadPackSpr "dive-kick-land-indicator.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath packFilePath f

releasableAtkIndicatorSprs :: GauntletsAttackDescriptions -> GauntletsDataSprites -> M.Map AttackDescription Sprite
releasableAtkIndicatorSprs atkDescs sprs = M.fromList
    [ atkDescSpr _punch1 _punch1Indicator
    , atkDescSpr _punch2 _punch2Indicator
    , atkDescSpr _doubleKick2 _doubleKick2Indicator
    , atkDescSpr _punch3 _punch3Indicator
    , atkDescSpr _kick _kickIndicator
    , atkDescSpr _risingUppercut _risingUppercutIndicator
    , atkDescSpr _dashPunch _dashPunchIndicator
    , atkDescSpr _evasiveKick _evasiveKickIndicator
    , atkDescSpr _airPunch1 _airPunch1Indicator
    , atkDescSpr _airPunch2 _airPunch2Indicator
    , atkDescSpr _airForwardsKick2 _airForwardsKick2Indicator
    , atkDescSpr _airPunch3 _airPunch3Indicator
    , atkDescSpr _diveKickBounce _diveKickBounceIndicator
    , atkDescSpr _diveKickLand _diveKickLandIndicator
    ]
    where atkDescSpr = \atkDescF overlaySprF -> (atkDescF atkDescs, overlaySprF sprs)

data GauntletsData = GauntletsData
    { _enemyInAttackRange         :: Bool
    , _releaseHeld                :: Bool
    , _attackHitEnemy             :: Bool
    , _attackSeenReleasableFrame  :: Bool
    , _overlaySprite              :: Maybe Sprite
    , _chargeOverlaySprite        :: Sprite
    , _sprites                    :: GauntletsDataSprites
    , _attackDescriptions         :: GauntletsAttackDescriptions
    , _releasableAtkIndicatorSprs :: M.Map AttackDescription Sprite
    , _chargeSoundHashedId        :: HashedId
    , _config                     :: GauntletsConfig
    }

mkGauntletsData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m GauntletsData
mkGauntletsData = do
    chargeOverlaySpr    <- loadPackSprite $ PackResourceFilePath packFilePath "charge-overlay.spr"
    sprs                <- mkGauntletsDataSprites
    atkDescs            <- mkGauntletsAttackDescriptions
    chargeSoundHashedId <- hashId <$> newId
    cfg                 <- readConfig _playerWeapon _gauntlets

    return $ GauntletsData
        { _enemyInAttackRange         = False
        , _releaseHeld                = False
        , _attackHitEnemy             = False
        , _attackSeenReleasableFrame  = False
        , _overlaySprite              = Nothing
        , _chargeOverlaySprite        = chargeOverlaySpr
        , _sprites                    = sprs
        , _attackDescriptions         = atkDescs
        , _releasableAtkIndicatorSprs = releasableAtkIndicatorSprs atkDescs sprs
        , _chargeSoundHashedId        = chargeSoundHashedId
        , _config                     = cfg
        }

isReleasableAtkConditionsMet :: GauntletsData -> Bool
isReleasableAtkConditionsMet gauntletsData
    | _allowReleasableAttacksWithoutHit (_config gauntletsData) = _releaseHeld gauntletsData
    | otherwise                                                 =
        _releaseHeld gauntletsData && _attackHitEnemy gauntletsData && _attackSeenReleasableFrame gauntletsData
