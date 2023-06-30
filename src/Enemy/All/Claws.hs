module Enemy.All.Claws
    ( allClawsEnemyPreloadPackFilePaths
    , mkClawsEnemy
    ) where

import Control.Monad          (unless, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State    (execState, get, modify, put)
import qualified Data.Text as T

import Attack
import Attack.Hit
import Collision
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Claws
import Configs.All.EnemyLockOn
import Configs.All.Settings
import Configs.All.Settings.Debug
import Enemy as E
import Enemy.All.Claws.AI
import Enemy.All.Claws.Behavior
import Enemy.All.Claws.Data
import Enemy.All.Claws.Sprites
import FileCache
import Msg
import Particle.All.AttackSpecks
import Particle.All.EnemyHurt
import Util
import Window.Graphics

allClawsEnemyPreloadPackFilePaths = ["data/enemies/claws-enemy.pack"] :: [FilePath]

hurtSoundPath           = "event:/SFX Events/Enemy/Claws/hurt" :: FilePath
onlySlashDebugFlag      = T.toLower "onlySlash"                :: T.Text
onlyProjectileDebugFlag = T.toLower "onlyShoot"                :: T.Text

mkClawsEnemy :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> Direction -> m (Some Enemy)
mkClawsEnemy pos dir = do
    enemyData         <- mkClawsEnemyData
    enemy             <- mkEnemy enemyData pos dir
    clawsCfg          <- readEnemyConfig _claws
    lockOnReticleData <- readEnemyLockOnConfig _claws
    tauntedData       <- mkEnemyTauntedData $ _tauntedUnderlayDrawScale clawsCfg

    return . Some $ enemy
        { _type                   = Just ClawsEnemy
        , _health                 = _health (clawsCfg :: ClawsEnemyConfig)
        , _hitbox                 = clawsEnemyHitbox
        , _inHitstun              = clawsEnemyInHitstun
        , _lockOnReticleData      = lockOnReticleData
        , _tauntedData            = Just tauntedData
        , _thinkAI                = thinkAI
        , _updateHurtResponse     = updateHurtResponse
        , _updateGroundResponse   = updateGroundResponse
        , _updateHangtimeResponse = updateHangtimeResponse
        , _updateSprite           = updateSpr
        , _setDebugBehavior       = setDebugBehavior
        }

clawsEnemyHitbox :: EnemyHitbox ClawsEnemyData
clawsEnemyHitbox enemy = case _behavior enemyData of
    SpawnBehavior -> dummyHbx
    DeathBehavior -> dummyHbx
    HurtBehavior _ _
        | inAir   -> rectHitbox airHurtPos airHurtWidth airHurtHeight
    _             -> rectHitbox pos width height
    where
        enemyData = _data enemy
        inAir     = not $ enemyTouchingGround enemy
        Pos2 x y  = E._pos enemy
        cfg       = _claws (_config enemyData :: EnemyConfig)

        width  = _width (cfg :: ClawsEnemyConfig)
        height = _height (cfg :: ClawsEnemyConfig)
        pos    = Pos2 (x - width / 2.0) (y - height)

        airHurtWidth   = _airHurtWidth (cfg :: ClawsEnemyConfig)
        airHurtHeight  = _airHurtHeight (cfg :: ClawsEnemyConfig)
        airHurtOffsetY = _airHurtOffsetY (cfg :: ClawsEnemyConfig)
        airHurtPos     = Pos2 (x - airHurtWidth / 2.0) (y - airHurtHeight + airHurtOffsetY)

        dummyHbx = dummyHitbox $ Pos2 x (y - height / 2.0)

clawsEnemyInHitstun :: EnemyInHitstun ClawsEnemyData
clawsEnemyInHitstun enemy = case _behavior (_data enemy) of
    HurtBehavior _ _    -> True
    LaunchedBehavior _  -> True
    FallenBehavior _    -> True
    GetUpBehavior       -> True
    WallSplatBehavior _ -> True
    _                   -> False

updateSpr :: EnemyUpdateSprite ClawsEnemyData
updateSpr enemy = case _behavior enemyData of
    IdleBehavior _                     -> setOrUpdateEnemySpr $ _idle sprs
    LaunchedBehavior _
        | velY <= 0.0 || inHangtimeVel -> setOrUpdateEnemySpr $ _launched sprs
        | otherwise                    -> setOrUpdateEnemySpr $ _fall sprs
    HurtBehavior _ hurtType
        | justGotHit                   -> setEnemyHurtSprite enemy $ case hurtType of
            WallHurt      -> _wallHurt sprs
            FallenHurt    -> _fallenHurt sprs
            KnockDownHurt -> _knockDownFallen sprs
            AirHurt       -> _airHurt sprs
            LaunchUpHurt  -> _launchUp sprs
            StandHurt     -> _hurt sprs
        | otherwise                    -> updateEnemySprite enemy
    WallSplatBehavior _                -> setOrUpdateEnemySpr $ _wallSplat sprs
    AdvanceBehavior _                  -> setOrUpdateEnemySpr $ _walk sprs
    DashBehavior                       -> clearEnemySprite enemy
    RetreatBehavior _                  -> setOrUpdateEnemySpr $ _walkBack sprs
    DeathBehavior                      -> (setOrUpdateEnemySpr (_death sprs)) {_draw = Just drawEnemyDeath}
    AttackBehavior                     -> clearEnemySprite enemy
    SpawnBehavior                      -> setOrUpdateEnemySpr $ _spawn sprs
    FallenBehavior _                   -> setOrUpdateEnemySpr $ _fallen sprs
    GetUpBehavior                      -> setOrUpdateEnemySpr $ _getUp sprs
    where
        setOrUpdateEnemySpr = \spr -> setOrUpdateEnemySprite enemy spr

        justGotHit    = enemyJustGotHit enemy
        velY          = vecY $ E._vel enemy
        inHangtimeVel = enemyInHangtimeVel enemy (_config enemyData)
        enemyData     = _data enemy
        sprs          = _sprites enemyData

updateHurtResponse :: (ConfigsRead m, MsgsWrite UpdateEnemyMsgsPhase m) => EnemyUpdateHurtResponse ClawsEnemyData m
updateHurtResponse atkHit enemy
    | behavior == DeathBehavior = return enemy

    | isStagger || isAirVulnerable || atkAlwaysLaunches =
        let
            -- prevent sliding on the ground from downwards aerial attacks
            atkVel'
                | onGround && atkVelY > 0.0 = Vel2 0.0 atkVelY
                | otherwise                 = atkVel

            isFallen    = isFallenBehavior behavior || isFallenHurtBehavior behavior
            isKnockDown = onGround && atkVelY > 0.0 && not isFallen

            hurtType
                | atkVelY < 0.0 || atkAlwaysLaunches = LaunchUpHurt
                | otherwise                          = case behavior of
                    _
                        | isKnockDown           -> KnockDownHurt
                    LaunchedBehavior _          -> AirHurt
                    HurtBehavior _ LaunchUpHurt -> AirHurt
                    HurtBehavior _ AirHurt      -> AirHurt
                    FallenBehavior _            -> FallenHurt
                    HurtBehavior _ FallenHurt   -> FallenHurt
                    WallSplatBehavior _         -> WallHurt
                    HurtBehavior _ WallHurt     -> WallHurt
                    _                           -> StandHurt

            enemyCfg    = _config enemyData
            hitstunSecs = enemyHitstunFromAttackHit atkHit enemyCfg
            hurtSecs    = flip execState hitstunSecs $ do
                -- don't override longer stun w/ shorter one
                get >>= \secs -> case behavior of
                    HurtBehavior hurtTtl _
                        | hurtTtl > secs -> put hurtTtl
                    _                    -> return ()

                -- enforce min time in fallen state
                when (hurtType `elem` [FallenHurt, KnockDownHurt]) $
                    modify $ max (_minFallenSecs enemyCfg)

            behavior'                      = HurtBehavior hurtSecs hurtType
            enemyData'                     = enemyData {_behavior = behavior'}
            dir                            = maybe (E._dir enemy) flipDirection (_dir (atkHit :: AttackHit))
            touchingGround
                | hurtType == LaunchUpHurt = False
                | otherwise                = onGround
        in do
            when (atkDmg > 0) $ do
                unless justGotHit $
                    writeMsgs
                        [ mkMsg $ ParticleMsgAddM (mkEnemyHurtParticle enemy atkHit hurtEffectData)
                        , mkMsg $ ParticleMsgAddM (mkAttackSpecksParticle atkHit)
                        ]
                unlessM ((justGotHit ||) <$> readSettingsConfig _debug _disableEnemyHurtSfx) $
                    writeMsgs [mkMsg $ AudioMsgPlaySound hurtSoundPath atkPos]

            return $ enemy
                { _data          = enemyData'
                , _vel           = atkVel'
                , _dir           = dir
                , _attack        = Nothing
                , _health        = hp
                , _launchTargetY = attackHitLaunchTargetY (E._pos enemy) atkHit
                , _flags         = flags
                    { _touchingGround = touchingGround
                    , _justGotHit     = Just atkPos
                    }
                }

    | otherwise = do
        when (atkDmg > 0 && not justGotHit) $
            writeMsgs
                [ mkMsg $ ParticleMsgAddM (mkEnemyHurtParticleEx enemy atkHit hurtEffectData WeakHitEffect)
                , mkMsg $ ParticleMsgAddM (mkAttackSpecksParticleEx atkHit WeakHitEffect)
                ]

        return $ enemy
            { _health = hp
            , _flags  = flags {_justGotHit = Just atkPos}
            }

    where
        enemyData      = _data enemy
        cfg            = _claws (_config enemyData :: EnemyConfig)
        hurtEffectData = _hurtEffectData cfg
        behavior       = _behavior enemyData
        onGround       = enemyTouchingGround enemy
        inAir          = not onGround

        isWeakAtkHitVel = _isWeakVel atkHit
        velY            = vecY $ E._vel enemy
        stagger         = _stagger (atkHit :: AttackHit)
        isStagger       = stagger >= _staggerThreshold cfg || case behavior of
            HurtBehavior _ _
                | isWeakAtkHitVel -> velY >= 0 && inAir
                | otherwise       -> True
            LaunchedBehavior _
                | isWeakAtkHitVel -> velY >= 0
                | otherwise       -> True
            FallenBehavior _      -> True
            WallSplatBehavior _   -> True
            _                     -> False

        atkPos                  = _intersectPos atkHit
        justGotHit              = enemyJustGotHit enemy
        flags                   = _flags enemy
        atkAlwaysLaunches       = _alwaysLaunches atkHit
        atkVel@(Vel2 _ atkVelY) = _vel (atkHit :: AttackHit)
        atkDmg                  = _damage (atkHit :: AttackHit)
        hp                      = decreaseEnemyHealth atkDmg enemy
        isAirVulnerable         = inAir && not isWeakAtkHitVel

updateGroundResponse :: MsgsWrite UpdateEnemyMsgsPhase m => EnemyUpdateGroundResponse ClawsEnemyData m
updateGroundResponse groundY enemy
    | velY >= 0.0 =
        let
            x              = vecX $ E._pos enemy
            enemyData      = _data enemy
            minFallenSecs  = _minFallenSecs $ _config enemyData
            fallenBehavior = FallenBehavior minFallenSecs

            behavior       = _behavior enemyData
            isPrevLaunched = isLaunchedBehavior behavior
            behavior'      = case _behavior enemyData of
                HurtBehavior _ LaunchUpHurt -> fallenBehavior
                HurtBehavior _ AirHurt      -> fallenBehavior
                LaunchedBehavior _          -> fallenBehavior
                _                           -> behavior
        in do
            when isPrevLaunched $
                let effectDrawScale = _groundImpactEffectDrawScale $ _claws (_config enemyData :: EnemyConfig)
                in writeMsgs $ enemyGroundImpactMessages effectDrawScale enemy

            return $ enemy
                { _data  = enemyData {_behavior = behavior'}
                , _pos   = Pos2 x groundY
                , _vel   = Vel2 velX 0.1
                , _flags = flags
                }

    | otherwise = return $ enemy {_flags = flags}

    where
        Vel2 velX velY = E._vel enemy
        flags          = (_flags enemy) {_touchingGround = True}

updateHangtimeResponse :: EnemyUpdateHangtimeResponse ClawsEnemyData
updateHangtimeResponse hangtimeSecs enemy
    | behavior == DeathBehavior = enemy
    | inAir                     = enemy
        { _data = enemyData {_behavior = LaunchedBehavior hangtimeSecs}
        }
    | otherwise                 = enemy
    where
        enemyData = _data enemy
        behavior  = _behavior enemyData
        inAir     = not $ enemyTouchingGround enemy

setDebugBehavior :: EnemySetDebugBehavior ClawsEnemyData
setDebugBehavior flags enemy = (debugTxt, enemy {_data = enemyData'})
    where
        enemyData                                  = _data enemy
        cfg                                        = _config enemyData
        clawsCfg                                   = _claws (cfg :: EnemyConfig)
        (clawsCfg', debugTxt)
            | onlySlashDebugFlag `elem` flags      =
                (clawsCfg {_willUseProjectileChance = 0.0}, " | only slash attack")
            | onlyProjectileDebugFlag `elem` flags =
                (clawsCfg {_willUseProjectileChance = 1.0}, " | only projectile attack")
            | otherwise                            = (clawsCfg, "")

        enemyData' = enemyData
            { _willUseAttackProjectile = if
                | _willUseProjectileChance clawsCfg' <= 0.0 -> False
                | _willUseProjectileChance clawsCfg' >= 1.0 -> True
                | otherwise                                 -> _willUseAttackProjectile enemyData
            , _config                  = cfg {_claws = clawsCfg'}
            }
