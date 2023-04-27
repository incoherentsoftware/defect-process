module Enemy.All.Turret
    ( allTurretEnemyPreloadPackFilePaths
    , mkTurretEnemy
    ) where

import Control.Monad          (unless, when)
import Control.Monad.IO.Class (MonadIO)

import Attack
import Attack.Hit
import Collision
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Turret
import Configs.All.EnemyLockOn
import Configs.All.Settings
import Configs.All.Settings.Debug
import Enemy as E
import Enemy.All.Turret.AI
import Enemy.All.Turret.Behavior
import Enemy.All.Turret.Data
import Enemy.All.Turret.Sprites
import FileCache
import Msg
import Particle.All.AttackSpecks
import Particle.All.EnemyHurt
import Util
import Window.Graphics

allTurretEnemyPreloadPackFilePaths = ["data/enemies/turret-enemy.pack"] :: [FilePath]

hurtSoundPath = "event:/SFX Events/Enemy/Turret/hurt" :: FilePath

mkTurretEnemy :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> Direction -> m (Some Enemy)
mkTurretEnemy pos dir = do
    enemyData         <- mkTurretEnemyData
    enemy             <- mkEnemy enemyData pos dir
    turretCfg         <- readEnemyConfig _turret
    lockOnReticleData <- readEnemyLockOnConfig _turret
    tauntedData       <- mkEnemyTauntedData $ _tauntUnderlayDrawScale turretCfg

    return . Some $ enemy
        { _type                 = Just TurretEnemy
        , _health               = _health (turretCfg :: TurretEnemyConfig)
        , _hitbox               = turretEnemyHitbox
        , _pullable             = const False
        , _lockOnReticleData    = lockOnReticleData
        , _tauntedData          = Just tauntedData
        , _thinkAI              = thinkAI
        , _updateHurtResponse   = updateHurtResponse
        , _updateGroundResponse = updateGroundResponse
        , _updateSprite         = updateSpr
        }

turretEnemyHitbox :: EnemyHitbox TurretEnemyData
turretEnemyHitbox enemy
    | _behavior enemyData `elem` [SpawnBehavior, DeathBehavior] = dummyHitbox $ Pos2 x (y - height / 2.0)
    | otherwise                                                 = rectHitbox pos width height
    where
        enemyData = _data enemy
        Pos2 x y  = E._pos enemy
        cfg       = _turret (_config enemyData :: EnemyConfig)
        width     = _width (cfg :: TurretEnemyConfig)
        height    = _height (cfg :: TurretEnemyConfig)
        pos       = Pos2 (x - width / 2.0) (y - height)

updateSpr :: EnemyUpdateSprite TurretEnemyData
updateSpr enemy = case _behavior enemyData of
    IdleBehavior     -> setOrUpdateEnemySpr $ _idle sprs
    HurtBehavior _
        | justGotHit -> setEnemyHurtSprite enemy $ _hurt sprs
        | otherwise  -> updateEnemySprite enemy
    DeathBehavior    -> (setOrUpdateEnemySpr (_death sprs)) {_draw = Just drawEnemyDeath}
    AttackBehavior   -> clearEnemySprite enemy
    SpawnBehavior    -> setOrUpdateEnemySpr $ _spawn sprs
    where
        setOrUpdateEnemySpr = \spr -> setOrUpdateEnemySprite enemy spr

        justGotHit = enemyJustGotHit enemy
        enemyData  = _data enemy
        sprs       = _sprites enemyData

updateHurtResponse :: (ConfigsRead m, MsgsWrite UpdateEnemyMsgsPhase m) => EnemyUpdateHurtResponse TurretEnemyData m
updateHurtResponse atkHit enemy
    | isDeathBehavior behavior = return enemy

    | isStagger =
        let
            hitstunSecs = enemyHitstunFromAttackHit atkHit (_config enemyData)
            hurtSecs    = case behavior of
                HurtBehavior hurtTtl
                    | hurtTtl > hitstunSecs -> hurtTtl
                _                           -> hitstunSecs

            behavior'    = HurtBehavior hurtSecs
            enemyData'   = enemyData {_behavior = behavior'}
            dir          = maybe (E._dir enemy) flipDirection (_dir (atkHit :: AttackHit))
            attackProjId = _attackProjMsgId enemyData'
        in do
            when (atkDmg > 0) $ do
                unless justGotHit $
                    writeMsgs
                        [ mkMsg $ ParticleMsgAddM (mkEnemyHurtParticle enemy atkHit hurtEffectData)
                        , mkMsg $ ParticleMsgAddM (mkAttackSpecksParticle atkHit)
                        ]
                unlessM ((justGotHit ||) <$> readSettingsConfig _debug _disableEnemyHurtSfx) $
                    writeMsgs [mkMsg $ AudioMsgPlaySound hurtSoundPath atkPos]

            -- kill attack projectile beam when enemy gets hurt (interrupted)
            writeMsgs [mkMsgTo (ProjectileMsgSetTtl 0.0) attackProjId]

            return $ enemy
                { _data   = enemyData'
                , _dir    = dir
                , _attack = Nothing
                , _health = hp
                , _flags  = flags
                }

    | otherwise = do
        when (atkDmg > 0 && not justGotHit) $
            writeMsgs
                [ mkMsg $ ParticleMsgAddM (mkEnemyHurtParticleEx enemy atkHit hurtEffectData WeakHitEffect)
                , mkMsg $ ParticleMsgAddM (mkAttackSpecksParticleEx atkHit WeakHitEffect)
                ]

        return $ enemy
            { _health = hp
            , _flags  = flags
            }

    where
        enemyData      = _data enemy
        cfg            = _turret (_config enemyData :: EnemyConfig)
        hurtEffectData = _hurtEffectData cfg
        behavior       = _behavior enemyData

        stagger                                = _stagger (atkHit :: AttackHit)
        isStagger
            | stagger >= _staggerThreshold cfg = True
            | _isWeakVel atkHit                = False
            | otherwise                        = isHurtBehavior behavior

        atkPos     = _intersectPos atkHit
        justGotHit = enemyJustGotHit enemy
        flags      = (_flags enemy) {_justGotHit = Just atkPos}
        atkDmg     = _damage (atkHit :: AttackHit)
        hp         = decreaseEnemyHealth atkDmg enemy

updateGroundResponse :: Monad m => EnemyUpdateGroundResponse TurretEnemyData m
updateGroundResponse groundY enemy = return $ if
    | velY >= 0.0 ->
        let x = vecX $ E._pos enemy
        in enemy
            { _pos   = Pos2 x groundY
            , _vel   = Vel2 velX 0.1
            , _flags = flags
            }
    | otherwise   -> enemy {_flags = flags}
    where
        Vel2 velX velY = E._vel enemy
        flags          = (_flags enemy) {_touchingGround = True}
