module Enemy.All.Boss.Util
    ( isPhasedHitbox
    , isAtUnusedHpThreshold
    , isGuardState
    , isSuperArmorState
    , isDoHpThresholdAttackFrame
    , isHpThresholdSummonFlying
    , isHpThresholdSummonSpears
    , isHpThresholdSummonWalls
    , isHpThresholdPhaseInAttackFrame
    ) where

import Data.Maybe (isNothing)

import Attack
import Configs.All.Enemy
import Configs.All.Enemy.Boss
import Enemy as E
import Enemy.All.Boss.Behavior
import Enemy.All.Boss.Data
import Window.Graphics

phasedFrameTagName              = FrameTagName "phased"              :: FrameTagName
guardFrameTagName               = FrameTagName "guard"               :: FrameTagName
superArmorFrameTagName          = FrameTagName "superArmor"          :: FrameTagName
doHpThresholdAttackFrameTagName = FrameTagName "doHpThresholdAttack" :: FrameTagName
summonFlyingFrameTagName        = FrameTagName "summonFlying"        :: FrameTagName
summonSpearsFrameTagName        = FrameTagName "summonSpears"        :: FrameTagName
summonWallsFrameTagName         = FrameTagName "summonWalls"         :: FrameTagName
hpThresholdPhaseInFrameTagName  = FrameTagName "hpThresholdPhaseIn"  :: FrameTagName

isAttackFrameTag' :: FrameTagName -> Enemy BossEnemyData -> Bool
isAttackFrameTag' frameTagName enemy = case E._attack enemy of
    Nothing  -> False
    Just atk -> frameTagName `isAttackFrameTag` atk

isAttackFrameChanged :: Enemy BossEnemyData -> Bool
isAttackFrameChanged enemy = maybe False attackFrameChanged (E._attack enemy)

isPhasedHitbox :: Enemy BossEnemyData -> Bool
isPhasedHitbox enemy = isAttackFrameTag' phasedFrameTagName enemy

isAtUnusedHpThreshold :: (Damage, Maybe SummonAttackDesc) -> Enemy BossEnemyData -> Bool
isAtUnusedHpThreshold (dmgThreshold, usedAtkDesc) enemy = isDmgThreshold && isNothing usedAtkDesc
    where isDmgThreshold = isHealthZero $ decreaseHealth dmgThreshold (E._health enemy)

isGuardState :: Enemy BossEnemyData -> Bool
isGuardState enemy = isAnyGuardBehavior behavior || isGuardFrame
    where
        behavior     = _behavior $ _data enemy
        isGuardFrame = isAttackFrameTag' guardFrameTagName enemy

isSuperArmorState :: Enemy BossEnemyData -> Bool
isSuperArmorState enemy = isSuperArmorAllowed && isAttackFrameTag' superArmorFrameTagName enemy
    where isSuperArmorAllowed = not . _noSuperArmor $ _boss (_config (_data enemy) :: EnemyConfig)

isDoHpThresholdAttackFrame :: Enemy BossEnemyData -> Bool
isDoHpThresholdAttackFrame enemy = isAttackFrameTag' doHpThresholdAttackFrameTagName enemy

isHpThresholdSummonFlying :: Enemy BossEnemyData -> Bool
isHpThresholdSummonFlying enemy = isFrameTag && isAttackFrameChanged enemy
    where isFrameTag = isAttackFrameTag' summonFlyingFrameTagName enemy

isHpThresholdSummonSpears :: Enemy BossEnemyData -> Bool
isHpThresholdSummonSpears enemy = isFrameTag && isAttackFrameChanged enemy
    where isFrameTag = isAttackFrameTag' summonSpearsFrameTagName enemy

isHpThresholdSummonWalls :: Enemy BossEnemyData -> Bool
isHpThresholdSummonWalls enemy = isFrameTag && isAttackFrameChanged enemy
    where isFrameTag = isAttackFrameTag' summonWallsFrameTagName enemy

isHpThresholdPhaseInAttackFrame :: Enemy BossEnemyData -> Bool
isHpThresholdPhaseInAttackFrame enemy = isAttackFrameTag' hpThresholdPhaseInFrameTagName enemy
