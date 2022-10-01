module Player.Weapon.All.Sword.AttackOrb
    ( mkAttackOrb
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable          (sequenceA_)
import Data.Functor           ((<&>))
import Data.Maybe             (fromMaybe)

import Attack as A
import Attack.Hit
import Attack.Projectile
import Collision
import Configs
import Configs.All.PlayerWeapon.Sword
import Constants
import Enemy as E
import FileCache
import Id
import Msg
import Particle.All.Simple
import Player.Weapon.All.Sword.Data
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

swordPackPath          = "data/player/weapons/sword.pack"                              :: FilePath
attackOrbAppearPath    = PackResourceFilePath swordPackPath "attack-orb-appear.spr"    :: PackResourceFilePath
attackOrbIdlePath      = PackResourceFilePath swordPackPath "attack-orb-idle.spr"      :: PackResourceFilePath
attackOrbDisappearPath = PackResourceFilePath swordPackPath "attack-orb-disappear.spr" :: PackResourceFilePath
attackOrbIdleSoundPath = "event:/SFX Events/Player/Weapons/Sword/attack-orb-idle-c"    :: FilePath

staggerThreshold = Stagger 1 :: Stagger

registeredCollisions =
    [ ProjRegisteredEnemyRealCollision
    , ProjRegisteredRoomItemCollision
    ] :: [ProjectileRegisteredCollision]

data AttackOrbData = AttackOrbData
    { _numActivations     :: Int
    , _ttl                :: Secs
    , _appearSprite       :: Sprite
    , _idleSprite         :: Sprite
    , _activateAttackDesc :: AttackDescription
    , _config             :: SwordConfig
    }

mkAttackOrbData :: (FileCache m, GraphicsRead m, MonadIO m) => SwordData -> m AttackOrbData
mkAttackOrbData swordData = do
    appearSpr <- loadPackSprite attackOrbAppearPath
    idleSpr   <- loadPackSprite attackOrbIdlePath
    let cfg    = _config (swordData :: SwordData)

    return $ AttackOrbData
        { _numActivations     = 0
        , _ttl                = _attackOrbAliveSecs cfg
        , _appearSprite       = appearSpr
        , _idleSprite         = idleSpr
        , _activateAttackDesc = _attackOrbActivate $ _attackDescriptions swordData
        , _config             = cfg
        }

mkAttackOrb
    :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m)
    => Pos2
    -> Direction
    -> SwordData
    -> m (Some Enemy)
mkAttackOrb pos dir swordData = do
    attackOrbData <- mkAttackOrbData swordData

    mkDummyEnemy attackOrbData pos dir <&> \e -> Some $ e
        { _sprite             = Just $ _appearSprite attackOrbData
        , _hitbox             = attackOrbHitbox
        , _lockOnReticleData  = _attackOrbLockOnReticleData $ _config (attackOrbData :: AttackOrbData)
        , _thinkAI            = thinkAttackOrb
        , _draw               = Just drawAttackOrb
        , _updateHurtResponse = attackOrbHurtResponse
        }

attackOrbHitbox :: EnemyHitbox AttackOrbData
attackOrbHitbox attackOrb
    | isAppearSpr = dummyHitbox pos
    | otherwise   =
        let
            cfg    = _config (attackOrbData :: AttackOrbData)
            width  = _attackOrbWidth cfg
            height = _attackOrbHeight cfg
            pos'   = pos `vecSub` Pos2 (width / 2.0) (height / 2.0)
        in rectHitbox pos' width height
    where
        attackOrbData = E._data attackOrb
        appearSpr     = _appearSprite attackOrbData
        isAppearSpr   = maybe False (== appearSpr) (E._sprite attackOrb)
        pos           = E._pos attackOrb

thinkAttackOrb :: Monad m => EnemyThinkAI AttackOrbData m
thinkAttackOrb attackOrb = return $ updateMsg:audioMsg:disappearMsgs
    where
        pos            = E._pos attackOrb
        dir            = E._dir attackOrb
        attackOrbMsgId = E._msgId attackOrb
        attackOrbData  = E._data attackOrb

        ttl              = _ttl (attackOrbData :: AttackOrbData)
        ttl'             = max 0.0 (ttl - timeStep)
        cfg              = _config (attackOrbData :: AttackOrbData)
        isMaxActivations = _numActivations attackOrbData >= _attackOrbMaxActivations cfg

        disappearMsgs
            | ttl' <= 0.0 || isMaxActivations =
                [ mkMsg $ ParticleMsgAddM (loadSimpleParticle pos dir worldEffectZIndex attackOrbDisappearPath)
                , mkMsgTo EnemyMsgSetDead attackOrbMsgId
                ]
            | otherwise                       = []

        appearSpr = _appearSprite attackOrbData
        idleSpr   = _idleSprite attackOrbData
        spr       = fromMaybe (_idleSprite attackOrbData) (E._sprite attackOrb)

        posOffset
            | spr == appearSpr =
                let velX = _attackOrbAppearVelX cfg * directionNeg dir
                in Pos2 (velX * timeStep) 0.0
            | otherwise        = zeroPos2

        spr'
            | spr == appearSpr && spriteFinished spr = idleSpr
            | otherwise                              = updateSprite spr

        update    = \e -> e
            { E._data   = (E._data e :: AttackOrbData) {_ttl = ttl'}
            , E._pos    = E._pos e `vecAdd` posOffset
            , E._sprite = Just spr'
            }
        updateMsg = mkMsgTo (EnemyMsgUpdate update) attackOrbMsgId

        hashedId = hashId attackOrbMsgId
        audioMsg = mkMsg $ AudioMsgPlaySoundContinuous attackOrbIdleSoundPath hashedId pos

drawAttackOrb :: (GraphicsReadWrite m, MonadIO m) => EnemyDraw AttackOrbData m
drawAttackOrb attackOrb = sequenceA_ $ do
    spr <- E._sprite attackOrb
    Just $ drawSprite (E._pos attackOrb) (E._dir attackOrb) worldBehindProjectileZIndex spr

attackOrbHurtResponse :: MsgsWrite UpdateEnemyMsgsPhase m => EnemyUpdateHurtResponse AttackOrbData m
attackOrbHurtResponse atkHit attackOrb
    | _stagger (atkHit :: AttackHit) >= staggerThreshold =
        let
            pos             = E._pos attackOrb
            dir             = E._dir attackOrb
            attackOrbData   = E._data attackOrb
            activateAtkDesc = _activateAttackDesc attackOrbData
            mkAttackProj    = mkAttackProjectile pos dir activateAtkDesc registeredCollisions
            attackOrbData'  = attackOrbData {_numActivations = _numActivations attackOrbData + 1}
        in do
            writeMsgs [mkMsg $ NewUpdateProjectileMsgAddM mkAttackProj]
            return $ attackOrb {E._data = attackOrbData'}

    | otherwise = return attackOrb
