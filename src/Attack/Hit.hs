module Attack.Hit
    ( AttackHit(..)
    , mkAttackHit
    , mkAttackHitEx
    , mkAttackHitEmpty
    , attackHitLaunchTargetY
    ) where

import Data.Maybe (fromMaybe)

import Attack
import Attack.Hit.Types
import Id
import Msg
import Util

overshootDifferenceMultiplier = 0.4 :: Float

mkAttackHit :: Attack -> AttackHit
mkAttackHit atk = mkAttackHitEx (_pos atk) atk

mkAttackHitEx :: Pos2 -> Attack -> AttackHit
mkAttackHitEx intersectPos atk = AttackHit
    { _hashedId          = hashId $ _id atk
    , _intersectPos      = intersectPos
    , _hitbox            = attackHitbox atk
    , _vel               = fromMaybe zeroVel2 (attackHitVel atk)
    , _dir               = Just $ _dir (atk :: Attack)
    , _launchTarget      = launchTarget
    , _damage            = attackDamage atk
    , _stagger           = attackStagger atk
    , _alwaysLaunches    = False
    , _isWeakVel         = attackIsWeakHitVels atk
    , _isRanged          = _isRanged (atkDesc :: AttackDescription)
    , _hitstunMultiplier = attackHitstunMultiplier atk
    , _hitEffectType     = attackHitEffectType atk
    , _specksType        = _specksType (atkDesc :: AttackDescription)
    , _specksPos         = _specksPos (atkDesc :: AttackDescription)
    , _specksDirection   = _specksDirection (atkDesc :: AttackDescription)
    }
    where
        atkDesc        = _description atk
        allowOvershoot = maybe True not (_noLaunchTargetOvershoot atkDesc)

        launchTarget =
            AttackHitLaunchTarget <$>
            _launchTargetY (atk :: Attack) <*>
            _launchTargetOffsetY atkDesc <*>
            Just allowOvershoot

mkAttackHitEmpty :: MsgId -> Pos2 -> AttackHit
mkAttackHitEmpty msgId intersectPos = AttackHit
    { _hashedId          = hashId msgId
    , _intersectPos      = intersectPos
    , _hitbox            = Nothing
    , _vel               = zeroVel2
    , _dir               = Nothing
    , _launchTarget      = Nothing
    , _damage            = Damage 0
    , _stagger           = Stagger 0
    , _alwaysLaunches    = False
    , _isWeakVel         = False
    , _isRanged          = False
    , _hitstunMultiplier = 1.0
    , _hitEffectType     = NormalHitEffect
    , _specksType        = Nothing
    , _specksPos         = Nothing
    , _specksDirection   = Nothing
    }

attackHitLaunchTargetY :: Pos2 -> AttackHit -> Maybe PosY
attackHitLaunchTargetY (Pos2 _ entityY) atkHit = case _launchTarget atkHit of
    Nothing           -> Nothing
    Just launchTarget ->
        let
            launchY           = _posY launchTarget
            entityDifferenceY = entityY - launchY
            offsetY           = _offsetY launchTarget
            allowOvershoot    = _allowOvershoot launchTarget
        in Just $ if
            | allowOvershoot && entityDifferenceY < offsetY * overshootDifferenceMultiplier ->
                launchY - offsetY * overshootDifferenceMultiplier * 2.0
            | otherwise                                                                     -> launchY
