module Attack
    ( module Attack.Types
    , module Attack.Description
    , module Attack.Util
    , mkAttack
    , attackSprite
    , attackFrameIndex
    , attackIsLastFrameIndex
    , attackFrameChanged
    , attackVel
    , attackVels
    , attackHitVel
    , attackHitlag
    , attackCancelable
    , attackWalkCancelable
    , attackIsWeakHitVels
    , attackHitstunMultiplier
    , attackOnHitType
    , attackHitEffectType
    , attackDamage
    , attackStagger
    , attackScreenshakeType
    , attackHitbox
    , attackHitlagMessages
    , attackHitSoundFilePath
    , attackSoundMessages
    , attackCollisionEntityHitMessages
    , attackEnemyHitMessages
    , attackSurfaceHitMessages
    , attackMeterGain
    , attackIs
    , attackIn
    , finishAttack
    , updateAttack
    , thinkAttack
    , isAttackFrameTag
    , attackFrameSecs
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe, isNothing, listToMaybe)
import qualified Data.List as L
import qualified Data.Set as S

import Attack.Description
import Attack.Sound
import Attack.Types
import Attack.Util
import Collision
import Enemy.Types
import Id
import Msg
import Particle.All.Simple
import Player.Meter
import Util
import Window.Graphics
import World.Screenshake.Types
import World.ZIndex
import {-# SOURCE #-} Attack.Hit

attackCancelableFrameTagName = FrameTagName "attackCancelable" :: FrameTagName

mkAttack :: MonadIO m => Pos2 -> Direction -> AttackDescription -> m Attack
mkAttack pos@(Pos2 _ y) dir atkDesc =
    let
        maxLoops           = _maxLoops =<< _loopData (_sprite (atkDesc :: AttackDescription))
        atkRefreshIdsCount = case _refreshHitboxesType atkDesc of
            RefreshHitboxesPerFrameCount count
                | isNothing maxLoops -> (length (_hitboxes atkDesc) `div` count) + 1
            _                        -> maybe 0 (+ 1) maxLoops
        launchTargetY      = (y -) <$> _launchTargetOffsetY atkDesc  -- relative from initial attack pos on create
    in do
        atkId         <- newId
        atkRefreshIds <- sequenceA $ replicate atkRefreshIdsCount newId

        nextAttackOnDone <- case _nextAttackDescOnDone atkDesc of
            Nothing          -> return Nothing
            Just nextAtkDesc -> Just <$> mkAttack pos dir nextAtkDesc

        return $ Attack
            { _id               = atkId
            , _refreshIds       = atkRefreshIds
            , _pos              = pos
            , _dir              = dir
            , _angle            = 0.0
            , _launchTargetY    = launchTargetY
            , _done             = False
            , _description      = atkDesc
            , _deferUpdate      = True
            , _nextAttackOnDone = nextAttackOnDone
            , _soundHashedId    = hashId atkId
            }

attackSprite :: Attack -> Sprite
attackSprite = (_sprite :: AttackDescription -> Sprite) . _description

attackFrameIndex :: Attack -> FrameIndex
attackFrameIndex = _frameIndex . attackSprite

attackIsLastFrameIndex :: Attack -> Bool
attackIsLastFrameIndex = spriteIsLastFrameIndex . attackSprite

attackFrameChanged :: Attack -> Bool
attackFrameChanged = _frameChanged . attackSprite

attackVel :: Attack -> AttackVel
attackVel attack = maybe NoAttackVel (flipAttackVel dir) (listToMaybe $ attackVels attack)
    where dir = _dir (attack :: Attack)

attackVels :: Attack -> [AttackVel]
attackVels = _vels . _description

attackHitVel :: Attack -> Maybe Vel2
attackHitVel attack = (\(Vel2 velX velY) -> Vel2 (velX * dirNeg) velY) <$> vel
    where
        dirNeg  = directionNeg $ _dir (attack :: Attack)
        hitVels = _hitVels $ _description attack
        vel     = fromMaybe Nothing (listToMaybe hitVels)

attackHitlag :: Attack -> Secs
attackHitlag = _hitlag . _description

attackDamage :: Attack -> Damage
attackDamage = _damage . _description

attackStagger :: Attack -> Stagger
attackStagger = _stagger . _description

attackScreenshakeType :: Attack -> ScreenshakeType
attackScreenshakeType = _screenshakeType . _description

attackHitbox :: Attack -> Maybe Hitbox
attackHitbox attack = setHitboxTopLeftEx pos flipped <$> hitbox
    where
        pos      = _pos (attack :: Attack)
        flipped  = _dir (attack :: Attack) == LeftDir
        hitboxes = _hitboxes $ _description attack
        hitbox   = fromMaybe Nothing (listToMaybe hitboxes)

attackCancelable :: Attack -> Bool
attackCancelable attack = frameIndex >= cancelFrameIndex || isAtkCancelableFrameTag
    where
        cancelFrameIndex        = _cancelFrameIndex $ _description attack
        frameIndex              = attackFrameIndex attack
        isAtkCancelableFrameTag = attackCancelableFrameTagName `isAttackFrameTag` attack

attackWalkCancelable :: Attack -> Bool
attackWalkCancelable attack = nextAtkFrameIndex >= walkCancelFrameIndex
    where
        walkCancelFrameIndex = _walkCancelFrameIndex $ _description attack
        nextAtkFrameIndex    =
            attackFrameIndex $ updateAttack zeroPos2 RightDir attack  -- pos/dir don't matter for this calculation

attackIsWeakHitVels :: Attack -> Bool
attackIsWeakHitVels attack = fromMaybe False (_isWeakHitVels $ _description attack)

attackHitstunMultiplier :: Attack -> Float
attackHitstunMultiplier attack = fromMaybe 1.0 (_hitstunMultiplier $ _description attack)

attackOnHitType :: Attack -> AttackOnHitType
attackOnHitType = _onHitType . _description

attackHitEffectType :: Attack -> AttackHitEffectType
attackHitEffectType = _hitEffectType . _description

attackHitEffectMessages :: Hitbox -> Attack -> [Msg ThinkCollisionMsgsPhase]
attackHitEffectMessages collisionEntityHbx attack = case _hitEffectPath (_description attack) of
    Nothing            -> []
    Just hitEffectPath -> [mkMsg $ ParticleMsgAddM mkEffect]
        where
            atkPos          = _pos (attack :: Attack)
            atkHitbox       = fromMaybe (dummyHitbox atkPos) (attackHitbox attack)
            atkIntersectPos = hitboxAvgIntersectPos atkHitbox collisionEntityHbx
            dir             = _dir (attack :: Attack)
            mkEffect        = loadSimpleParticle atkIntersectPos dir playerAttackEffectZIndex hitEffectPath

attackHitlagMessages :: AllowMsgWrite p WorldMsgPayload => Attack -> [Msg p]
attackHitlagMessages attack
    | hitlagSecs > 0.0 = [mkMsg $ WorldMsgHitlag hitlagSecs]
    | otherwise        = []
    where hitlagSecs = attackHitlag attack

attackScreenshakeMessages :: Attack -> [Msg ThinkCollisionMsgsPhase]
attackScreenshakeMessages attack = case attackScreenshakeType attack of
    ScreenshakeOnHit magnitude -> [mkMsg $ WorldMsgScreenshake magnitude]
    _                          -> []

attackSound :: Attack -> AttackSound
attackSound = _sound . _description

attackHitSoundFilePath :: Attack -> Maybe FilePath
attackHitSoundFilePath = _hitSoundFilePath . attackSound

attackSoundMessages :: (AllowMsgWrite p AudioMsgPayload) => Attack -> [Msg p]
attackSoundMessages atk = case _type (attackSound atk :: AttackSound) of
    AttackPlaySound soundFilePath frameIndex
        | atkFrameIndex == frameIndex && atkFrameChanged ->
            [mkMsg $ AudioMsgPlaySoundUnique soundFilePath soundHashedId atkPos]
        | otherwise                                      -> []

    AttackPlaySounds soundFilePath frameIndices
        | atkFrameIndex `S.member` frameIndices && atkFrameChanged -> [mkMsg $ AudioMsgPlaySound soundFilePath atkPos]
        | otherwise                                                -> []

    AttackPlaySoundContinuous soundFilePath soundContinuousData ->
        let
            startFrameIndex = _startFrameIndex (soundContinuousData :: AttackSoundContinuousData)
            isSoundFrame    = case _endFrameIndex (soundContinuousData :: AttackSoundContinuousData) of
                Nothing            -> atkFrameIndex >= startFrameIndex
                Just endFrameIndex -> atkFrameIndex >= startFrameIndex && atkFrameIndex < endFrameIndex
        in if
            | isSoundFrame -> [mkMsg $ AudioMsgPlaySoundContinuous soundFilePath soundHashedId atkPos]
            | otherwise    -> []

    AttackNoSound -> []

    where
        atkFrameIndex   = attackFrameIndex atk
        atkFrameChanged = attackFrameChanged atk
        atkPos          = _pos (atk :: Attack)
        soundHashedId   = _soundHashedId atk

attackCollisionEntityHitMessages :: CollisionEntity e => e -> Attack -> [Msg ThinkCollisionMsgsPhase]
attackCollisionEntityHitMessages collisionEntity attack = case attackOnHitType attack of
    NormalOnHitType         -> normalOnHitMsgs
    ReplacedOnHitType onHit -> onHit entityHbx entityId attack
    AddedOnHitType onHit    -> normalOnHitMsgs ++ onHit entityHbx entityId attack
    where
        entityId        = collisionEntityMsgId collisionEntity
        atkHashedId     = hashId $ _id (attack :: Attack)
        isNewAtk        = collisionEntityNotPrevHitBy atkHashedId collisionEntity
        atkDamage       = attackDamage attack
        entityHbx       = collisionEntityHitbox collisionEntity
        atkPos          = _pos (attack :: Attack)
        atkHitbox       = fromMaybe (dummyHitbox atkPos) (attackHitbox attack)
        atkIntersectPos = hitboxAvgIntersectPos atkHitbox entityHbx
        atkHit          = mkAttackHitEx atkIntersectPos attack
        hurtMsg         = mkMsgTo (HurtMsgAttackHit atkHit) entityId

        worldMsgs
            | isNewAtk  = attackHitlagMessages attack ++ attackScreenshakeMessages attack
            | otherwise = []

        audioMsgs
            | isNewAtk && atkDamage > Damage 0 = case _hitSoundFilePath (attackSound attack) of
                Just hitSoundFilePath -> [mkMsg $ AudioMsgPlaySound hitSoundFilePath atkIntersectPos]
                Nothing               -> []
            | otherwise                        = []

        normalOnHitMsgs = hurtMsg:worldMsgs ++ audioMsgs

attackEnemyHitMessages :: Enemy d -> Attack -> [Msg ThinkCollisionMsgsPhase]
attackEnemyHitMessages enemy attack = enemySpecificHitMsgs ++ attackCollisionEntityHitMessages enemy attack
    where
        atkHashedId = hashId $ _id (attack :: Attack)
        isNewAtk    = collisionEntityNotPrevHitBy atkHashedId enemy
        enemyHbx    = collisionEntityHitbox enemy

        particleMsgs
            | isNewAtk  = attackHitEffectMessages enemyHbx attack
            | otherwise = []

        meterMsgs = case attackMeterGain attack of
            Just meter
                | isNewAtk -> [mkMsg $ PlayerMsgGainMeter meter]
            _              -> []

        enemySpecificHitMsgs
            | _dummyType enemy == EnemyDummyType = []
            | otherwise                          =
                let msgs = particleMsgs ++ meterMsgs
                in case attackOnHitType attack of
                    NormalOnHitType     -> msgs
                    ReplacedOnHitType _ -> []
                    AddedOnHitType _    -> msgs

attackSurfaceHitMessages :: Hitbox -> Attack -> [Msg ThinkCollisionMsgsPhase]
attackSurfaceHitMessages surfaceHbx attack = case _onSurfaceHitType (_description attack) of
    NoSurfaceHitType                -> []
    WithSurfaceHitType onSurfaceHit -> onSurfaceHit surfaceHbx attack

attackMeterGain :: Attack -> Maybe MeterValue
attackMeterGain = _meterGain . _description

attackIs :: Attack -> AttackDescription -> Bool
attackIs attack attackDesc =
    _filePath (_description attack :: AttackDescription) == _filePath (attackDesc :: AttackDescription)

attackIn :: Attack -> [AttackDescription] -> Bool
attackIn attack attackDesc = or $ map (attackIs attack) attackDesc

finishAttack :: Attack -> Attack
finishAttack attack = attack {_done = True}

updateAttack :: Pos2 -> Direction -> Attack -> Attack
updateAttack pos dir attack
    -- prevent immediately updating attack on same frame it was created
    | _deferUpdate attack = attack {_deferUpdate = False}

    | otherwise =
        let
            atkDesc = _description attack
            spr     = _sprite (atkDesc :: AttackDescription)
            spr'    = updateSprite spr
            atkDone = spriteFinished spr' || _done attack
        in if
            | atkDone, Just nextAttack <- _nextAttackOnDone attack -> nextAttack
                { _pos = pos
                , _dir = dir
                }

            | otherwise ->
                let
                    hitboxes                           = _hitboxes atkDesc
                    vels                               = _vels atkDesc
                    hitVels                            = _hitVels atkDesc
                    frameIndex                         = _frameIndex spr
                    frameIndex'                        = _frameIndex spr'
                    atkIdRefreshIds@(_, atkRefreshIds) = (_id (attack :: Attack), _refreshIds attack)

                    atkDesc'
                        | frameIndex' > frameIndex = atkDesc
                            { _sprite   = spr'
                            , _hitboxes = safeTail hitboxes
                            , _vels     = safeTail vels
                            , _hitVels  = safeTail hitVels
                            }
                        | otherwise                = atkDesc {_sprite = spr'} :: AttackDescription

                    refreshedAtkIdRefreshIds = fromMaybe atkIdRefreshIds (L.uncons atkRefreshIds)
                    (atkId, atkRefreshIds')  = case _refreshHitboxesType atkDesc' of
                        RefreshHitboxesOnLoop
                            | spriteLoopFrameCount spr' > spriteLoopFrameCount spr                               ->
                                refreshedAtkIdRefreshIds
                        RefreshHitboxesPerFrameCount frameCount
                            | frameIndex' > frameIndex && _int (frameIndex' :: FrameIndex) `mod` frameCount == 0 ->
                                refreshedAtkIdRefreshIds
                        _                                                                                        ->
                            atkIdRefreshIds
                in attack
                    { _id          = atkId
                    , _refreshIds  = atkRefreshIds'
                    , _pos         = pos
                    , _dir         = dir
                    , _done        = atkDone
                    , _description = atkDesc'
                    }

thinkAttack :: (AllowMsgWrite p AudioMsgPayload, AllowMsgWrite p WorldMsgPayload) => Attack -> [Msg p]
thinkAttack atk = screenshakeMsgs ++ attackSoundMessages atk
    where
        atkFrameIndex   = attackFrameIndex atk
        atkFrameChanged = attackFrameChanged atk
        screenshakeMsgs = case attackScreenshakeType atk of
            ScreenshakeOnFrame index magnitude
                | atkFrameIndex == index && atkFrameChanged -> [mkMsg $ WorldMsgScreenshake magnitude]
            _                                               -> []

isAttackFrameTag :: FrameTagName -> Attack -> Bool
isAttackFrameTag tagName atk = tagName `isSpriteFrameTag` spr
    where spr = attackSprite atk

attackFrameSecs :: Attack -> Secs
attackFrameSecs = fromMaybe 0.0 . listToMaybe . _frameSecs . attackSprite
