module Enemy.Util
    ( module Enemy.DeathEffectData
    , module Enemy.HurtEffectData.Types
    , module Enemy.LockOnData
    , module Enemy.LockOnReticleData
    , module Enemy.SpawnEffectData
    , enemySpawnDummyBodyPath
    , enemyDeathEffectPath
    , enemyDeathSoundPath
    , enemySuperArmorSoundPath
    , enemyHitbox
    , enemySpriteFinished
    , enemyFlippedDirIfWallOrGround
    , enemyTauntedStatus
    , enemyTauntedPrevStatus
    , mkEnemyUpdateMsg
    , mkEnemyUpdateMsgM
    , updateEnemySprite
    , setEnemySprite
    , setEnemyHurtSprite
    , setOrUpdateEnemySprite
    , clearEnemySprite
    , drawEnemyDeath
    , enemyKnownPlayerPos
    , enemyKnownPlayerCenterPos
    , enemyInHangtimeVel
    , enemyHitstunFromAttackHit
    , enemySpawnEffectMessages
    , enemySetDeadMessages
    , decreaseEnemyHealth
    , enemyGroundImpactMessages
    , enemyWallImpactMessages
    , isEnemyFacingPlayer
    , isEnemyInStasis
    , isEnemyInHitstun
    , mkEnemyAttack
    , mkEnemyAttackProjectile
    , mkEnemyAttackProjectileEx
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import Data.Typeable          (Typeable)
import qualified Data.Set as S

import AppEnv
import Attack
import Attack.Hit.Types
import Attack.Projectile
import Collision.Hitbox
import Configs
import Configs.All.Enemy
import Configs.All.Settings.Debug
import Constants
import Enemy.DeathEffectData
import Enemy.Flags
import Enemy.HurtEffectData.Types
import Enemy.LockOnData
import Enemy.LockOnReticleData
import Enemy.SpawnEffectData
import Enemy.StasisData
import Enemy.TauntedData
import Enemy.Types as E
import FileCache
import InfoMsg.Util
import Msg
import Particle.All.Simple
import Projectile.Types
import Util
import Window.Graphics
import World.ZIndex

particlesEnemyPath          = \f -> PackResourceFilePath "data/particles/particles-enemy.pack" f
enemySpawnDummyBodyPath     = particlesEnemyPath "enemy-spawn-dummy-body.spr" :: PackResourceFilePath
enemySpawnEffectPath        = particlesEnemyPath "enemy-spawn.spr"            :: PackResourceFilePath
enemyAirSpawnEffectPath     = particlesEnemyPath "enemy-air-spawn.spr"        :: PackResourceFilePath
enemyDeathEffectPath        = particlesEnemyPath "enemy-death.spr"            :: PackResourceFilePath
enemyGroundImpactEffectPath = particlesEnemyPath "enemy-ground-impact.spr"    :: PackResourceFilePath
enemyWallImpactEffectPath   = particlesEnemyPath "enemy-wall-impact.spr"      :: PackResourceFilePath
enemySpawnSoundPath         = "event:/SFX Events/Enemy/spawn"                 :: FilePath
enemyDeathSoundPath         = "event:/SFX Events/Enemy/death"                 :: FilePath
enemyGroundImpactSoundPath  = "event:/SFX Events/Enemy/ground-impact"         :: FilePath
enemySuperArmorSoundPath    = "event:/SFX Events/Enemy/super-armor"           :: FilePath

enemyHitbox :: Enemy d -> Hitbox
enemyHitbox enemy = (E._hitbox enemy) enemy

enemySpriteFinished :: Enemy d -> Bool
enemySpriteFinished enemy = maybe False spriteFinished sprite
    where sprite = E._sprite enemy

enemyFlippedDirIfWallOrGround :: Enemy d -> Direction
enemyFlippedDirIfWallOrGround enemy
    | facingWall || willFallOffGround = flipDirection dir
    | otherwise                       = dir
    where
        flags                          = _flags enemy
        dir                            = E._dir enemy
        facingWall
            | _touchingLeftWall flags  = dir == LeftDir
            | _touchingRightWall flags = dir == RightDir
            | otherwise                = False
        willFallOffGround              = _willFallOffGround flags

enemyTauntedStatus :: Enemy d -> EnemyTauntedStatus
enemyTauntedStatus enemy = maybe EnemyTauntedInactive _status (_tauntedData enemy)

enemyTauntedPrevStatus :: Enemy d -> EnemyTauntedStatus
enemyTauntedPrevStatus enemy = maybe EnemyTauntedInactive _prevStatus (_tauntedData enemy)

mkEnemyUpdateMsg :: Typeable d => Enemy d -> (Enemy d -> Enemy d) -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateMsg enemy update = [mkMsgTo (EnemyMsgUpdate update) enemyId]
    where enemyId = E._msgId enemy

mkEnemyUpdateMsgM
    :: Typeable d
    => Enemy d
    -> (Enemy d -> AppEnv UpdateEnemyMsgsPhase (Enemy d))
    -> [Msg ThinkEnemyMsgsPhase]
mkEnemyUpdateMsgM enemy update = [mkMsgTo (EnemyMsgUpdateM update) enemyId]
    where enemyId = E._msgId enemy

updateEnemySprite :: Enemy d -> Enemy d
updateEnemySprite enemy = enemy {_sprite = spr}
    where spr = updateSprite <$> E._sprite enemy

setEnemySprite :: Enemy d -> Sprite -> Enemy d
setEnemySprite enemy spr = enemy {_sprite = Just spr}

-- enemy hurt sprites should flip back/forth between frame 0/1 when hit repeatedly very quickly
setEnemyHurtSprite :: Enemy d -> Sprite -> Enemy d
setEnemyHurtSprite enemy hurtSpr = setEnemySprite enemy $ case E._sprite enemy of
    Just spr
        | spr == hurtSpr && _frameIndex spr == 0 -> advanceSprite 1 spr
    _                                            -> hurtSpr

setOrUpdateEnemySprite :: Enemy d -> Sprite -> Enemy d
setOrUpdateEnemySprite enemy spr
    | maybe True (/= spr) (E._sprite enemy) = enemy {_sprite = Just spr}
    | otherwise                             = updateEnemySprite enemy

clearEnemySprite :: Enemy d -> Enemy d
clearEnemySprite enemy = enemy {_sprite = Nothing}

-- assumes death sprite is already set for Enemy _sprite
drawEnemyDeath :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => EnemyDraw d m
drawEnemyDeath enemy = case E._sprite enemy of
    Nothing  -> return ()
    Just spr -> do
        deathEffectData <- readDeathEffectData $ _type enemy
        let
            offset    = fromMaybe zeroPos2 (_offset (deathEffectData :: EnemyDeathEffectData))
            pos       = hitboxCenter (enemyHitbox enemy) `vecAdd` offset
            drawScale = _drawScale (deathEffectData :: EnemyDeathEffectData)
        drawSpriteEx pos (E._dir enemy) enemyBodyZIndex 0.0 FullOpacity drawScale spr

enemyKnownPlayerPos :: Enemy d -> Maybe Pos2
enemyKnownPlayerPos enemy = playerInfoPos <$> _knownPlayerInfo enemy

enemyKnownPlayerCenterPos :: Enemy d -> Maybe Pos2
enemyKnownPlayerCenterPos enemy = playerInfoCenterPos <$> _knownPlayerInfo enemy

enemyInHangtimeVel :: Enemy d -> EnemyConfig -> Bool
enemyInHangtimeVel enemy enemyCfg = inAir && velWithinRange
    where
        gravityVelY       = _gravity enemyCfg * timeStep
        maxHangtimeSpeedX = _maxHangtimeSpeedX enemyCfg
        maxHangtimeVelY   = _maxHangtimeVelY enemyCfg
        minHangtimeVelY   = _minHangtimeVelY enemyCfg

        inAir          = not $ _touchingGround (_flags enemy :: EnemyFlags)
        Vel2 velX velY = E._vel enemy
        velY'          = velY + gravityVelY  -- use projected vertical velocity
        velWithinRange = abs velX <= maxHangtimeSpeedX && velY' >= minHangtimeVelY && velY' <= maxHangtimeVelY

enemyHitstunFromAttackHit :: AttackHit -> EnemyConfig -> Secs
enemyHitstunFromAttackHit atkHit enemyCfg
    | damageVal <= base = minHurtSecs * hitstunMultiplier
    | otherwise         = logBase base damageVal * minHurtSecs * hitstunMultiplier
    where
        damage            = _damage (atkHit :: AttackHit)
        damageVal         = fromIntegral $ _int (damage :: Damage)
        base              = _hitstunLogBase enemyCfg
        minHurtSecs       = _minHurtSecs enemyCfg
        hitstunMultiplier = _hitstunMultiplier (atkHit :: AttackHit)

enemySpawnEffectMessages :: Enemy d -> [Msg ThinkEnemyMsgsPhase]
enemySpawnEffectMessages enemy =
    [ mkMsg $ ParticleMsgAddM mkSpawnEffect
    , mkMsg $ AudioMsgPlaySound enemySpawnSoundPath (E._pos enemy)
    ]
    where
        mkSpawnEffect = do
            spawnEffectData <- readSpawnEffectData $ _type enemy
            let
                offset          = fromMaybe zeroPos2 (_offset (spawnEffectData :: EnemySpawnEffectData))
                inAir           = fromMaybe False (_inAir spawnEffectData)
                pos
                    | inAir     = hitboxCenter (enemyHitbox enemy) `vecAdd` offset
                    | otherwise = E._pos enemy `vecAdd` offset
                dir             = E._dir enemy
                drawScale       = _drawScale (spawnEffectData :: EnemySpawnEffectData)
                effectPath
                    | inAir     = enemyAirSpawnEffectPath
                    | otherwise = enemySpawnEffectPath
            loadSimpleParticleEx pos dir enemyOverBodyZIndex drawScale effectPath

enemySetDeadMessages :: Enemy d -> [Msg ThinkEnemyMsgsPhase]
enemySetDeadMessages enemy = [mkMsgTo EnemyMsgSetDead (E._msgId enemy)]

decreaseEnemyHealth :: Damage -> Enemy d -> Health
decreaseEnemyHealth damage enemy
    | _enemiesInvincible (_debugConfig enemy) = health {_value = max (_value health) minHealthValue}
    | otherwise                               = health
    where health = decreaseHealth damage (_health enemy)

enemyGroundImpactMessages :: DrawScale -> Enemy d -> [Msg UpdateEnemyMsgsPhase]
enemyGroundImpactMessages effectDrawScale enemy =
    [ mkMsg $ ParticleMsgAddM mkEffect
    , mkMsg $ AudioMsgPlaySound enemyGroundImpactSoundPath pos
    ]
    where
        pos      = E._pos enemy
        dir      = E._dir enemy
        mkEffect = loadSimpleParticleEx pos dir worldEffectZIndex effectDrawScale enemyGroundImpactEffectPath

enemyWallImpactMessages :: DrawScale -> Enemy d -> [Msg ThinkEnemyMsgsPhase]
enemyWallImpactMessages effectDrawScale enemy =
    [ mkMsg $ ParticleMsgAddM mkEffect
    , mkMsg $ RoomMsgArenaWallsSplat pos
    ]
    where
        pos      = hitboxCenter $ enemyHitbox enemy
        dir      = E._dir enemy
        mkEffect = loadSimpleParticleEx pos dir worldEffectZIndex effectDrawScale enemyWallImpactEffectPath

isEnemyFacingPlayer :: Enemy d -> Bool
isEnemyFacingPlayer enemy = case vecX . playerInfoPos <$> _knownPlayerInfo enemy of
    Nothing      -> False
    Just playerX ->
        let
            x   = vecX $ E._pos enemy
            dir = E._dir enemy
        in (dir == LeftDir && playerX <= x) || (dir == RightDir && playerX >= x)

isEnemyInStasis :: Enemy d -> Bool
isEnemyInStasis = isEnemyStasisDataInStasis . _stasisData

isEnemyInHitstun :: Enemy d -> Bool
isEnemyInHitstun enemy = (_inHitstun enemy) enemy

mkEnemyAttack :: (ConfigsRead m, MonadIO m) => Pos2 -> Direction -> AttackDescription -> EnemyTauntedStatus -> m Attack
mkEnemyAttack pos dir atkDesc tauntedStatus = do
    atkDesc' <- case tauntedStatus of
        EnemyTauntedActive -> do
            tauntedDamageMultiplier <- readConfig _enemy _tauntedDamageMultiplier
            let damage               = _damage (atkDesc :: AttackDescription)
            return $ (atkDesc :: AttackDescription)
                { _damage = multiplyDamage tauntedDamageMultiplier damage
                }
        _                  -> return atkDesc
    mkAttack pos dir atkDesc'

mkEnemyAttackProjectile
    :: (ConfigsRead m, MonadIO m)
    => Pos2
    -> Direction
    -> AttackDescription
    -> EnemyTauntedStatus
    -> m (Some Projectile)
mkEnemyAttackProjectile pos dir atkDesc tauntedStatus =
    mkEnemyAttackProjectileEx pos dir atkDesc tauntedStatus registeredCollisions
        where registeredCollisions = S.singleton ProjRegisteredPlayerCollision

mkEnemyAttackProjectileEx
    :: (ConfigsRead m, MonadIO m)
    => Pos2
    -> Direction
    -> AttackDescription
    -> EnemyTauntedStatus
    -> S.Set ProjectileRegisteredCollision
    -> m (Some Projectile)
mkEnemyAttackProjectileEx pos dir atkDesc tauntedStatus registeredCollisions =
    mkAttackProjectile pos dir atkDesc mkEnemyAttack' registeredCollisions
        where mkEnemyAttack' = \p d a -> mkEnemyAttack p d a tauntedStatus
