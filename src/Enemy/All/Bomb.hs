module Enemy.All.Bomb
    ( allBombEnemyPreloadPackFilePaths
    , mkBombEnemy
    ) where

import Control.Monad          (unless, when)
import Control.Monad.IO.Class (MonadIO)

import Attack
import Attack.Hit
import Collision
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Bomb
import Configs.All.EnemyLockOn
import Configs.All.Settings
import Configs.All.Settings.Debug
import Enemy as E
import Enemy.All.Bomb.AI
import Enemy.All.Bomb.Behavior
import Enemy.All.Bomb.Data
import Enemy.All.Bomb.Sprites
import FileCache
import Msg
import Particle.All.AttackSpecks
import Particle.All.EnemyHurt
import Util
import Window.Graphics

allBombEnemyPreloadPackFilePaths = ["data/enemies/bomb-enemy.pack"] :: [FilePath]

hurtSoundPath      = "event:/SFX Events/Enemy/Bomb/hurt-short" :: FilePath
hurtDeathSoundPath = "event:/SFX Events/Enemy/Bomb/hurt-death" :: FilePath

mkBombEnemy :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> Direction -> m (Some Enemy)
mkBombEnemy pos dir = do
    enemyData         <- mkBombEnemyData
    enemy             <- mkEnemy enemyData pos dir
    bombCfg           <- readEnemyConfig _bomb
    lockOnReticleData <- readEnemyLockOnConfig _bomb
    tauntedData       <- mkEnemyTauntedData $ _tauntUnderlayDrawScale bombCfg

    return . Some $ enemy
        { _type                   = Just BombEnemy
        , _health                 = _health (bombCfg :: BombEnemyConfig)
        , _hitbox                 = bombEnemyHitbox
        , _inHitstun              = bombEnemyInHitstun
        , _lockOnReticleData      = lockOnReticleData
        , _tauntedData            = Just tauntedData
        , _thinkAI                = thinkAI
        , _updateHurtResponse     = updateHurtResponse
        , _updateGroundResponse   = updateGroundResponse
        , _updateHangtimeResponse = updateHangtimeResponse
        , _updateSprite           = updateSpr
        }

bombEnemyHitbox :: EnemyHitbox BombEnemyData
bombEnemyHitbox enemy
    | _behavior enemyData == SpawnBehavior = dummyHitbox pos
    | otherwise                            = rectHitbox pos width height
    where
        enemyData = _data enemy
        Pos2 x y  = E._pos enemy
        cfg       = _bomb (_config enemyData :: EnemyConfig)
        width     = _width (cfg :: BombEnemyConfig)
        height    = _height (cfg :: BombEnemyConfig)
        pos       = Pos2 (x - width / 2.0) (y - height)

bombEnemyInHitstun :: EnemyInHitstun BombEnemyData
bombEnemyInHitstun enemy = case _behavior (_data enemy) of
    HurtBehavior _ _    -> True
    LaunchedBehavior _  -> True
    WallSplatBehavior _ -> True
    _                   -> False

updateSpr :: EnemyUpdateSprite BombEnemyData
updateSpr enemy = case behavior of
    _
        | isExplodeTimerActive enemyData -> case behavior of
            _
                | isExplodeSpr -> updateEnemySprite enemy

            HurtBehavior _ hurtType -> setEnemySpr $ case hurtType of
                WallHurt      -> _explodeWall sprs
                FallenHurt    -> _explodeFallen sprs
                KnockDownHurt -> _explodeFallen sprs
                AirHurt       -> _explodeLaunched sprs
                LaunchUpHurt  -> _explodeLaunched sprs
                StandHurt     -> _explode sprs

            _ -> setEnemySpr $ _explode sprs

    IdleBehavior _                     -> setOrUpdateEnemySpr $ _idle sprs
    SearchBehavior _ _                 -> setOrUpdateEnemySpr $ _idle sprs
    SprintBehavior _                   -> setOrUpdateEnemySpr $ _run sprs
    LaunchedBehavior _
        | velY <= 0.0 || inHangtimeVel -> setOrUpdateEnemySpr $ _launched sprs
        | otherwise                    -> setOrUpdateEnemySpr $ _fall sprs
    WallSplatBehavior _                -> setOrUpdateEnemySpr $ _explodeWall sprs
    SpawnBehavior                      -> setOrUpdateEnemySpr $ _spawn sprs

    HurtBehavior _ hurtType
        | justGotHit -> setEnemyHurtSprite enemy $ case hurtType of
            WallHurt      -> _explodeWall sprs
            FallenHurt    -> _explodeFallen sprs
            KnockDownHurt -> _explodeFallen sprs
            AirHurt       -> _airHurt sprs
            LaunchUpHurt  -> _launchUp sprs
            StandHurt     -> _hurt sprs
        | otherwise  -> updateEnemySprite enemy

    where
        setOrUpdateEnemySpr = \spr -> setOrUpdateEnemySprite enemy spr
        setEnemySpr         = \spr -> setEnemySprite enemy spr

        justGotHit    = enemyJustGotHit enemy
        velY          = vecY $ E._vel enemy
        inHangtimeVel = enemyInHangtimeVel enemy (_config enemyData)
        enemyData     = _data enemy
        behavior      = _behavior enemyData
        sprs          = _sprites enemyData
        isExplodeSpr  = maybe False (\spr -> isExplodeSprite spr sprs) (E._sprite enemy)

updateHurtResponse :: (ConfigsRead m, MsgsWrite UpdateEnemyMsgsPhase m) => EnemyUpdateHurtResponse BombEnemyData m
updateHurtResponse atkHit enemy
    | isStagger || isAirVulnerable || atkAlwaysLaunches =
        let
            -- prevent sliding on the ground from downwards aerial attacks
            atkVel'
                | onGround && atkVelY > 0.0 = Vel2 0.0 atkVelY
                | otherwise                 = atkVel

            isKnockDown                              = onGround && atkVelY > 0.0
            hurtType
                | atkVelY < 0.0 || atkAlwaysLaunches = LaunchUpHurt
                | otherwise                          = case behavior of
                    _
                        | isKnockDown           -> KnockDownHurt
                    LaunchedBehavior _          -> AirHurt
                    HurtBehavior _ LaunchUpHurt -> AirHurt
                    HurtBehavior _ AirHurt      -> AirHurt
                    _                           -> StandHurt

            hitstunSecs = enemyHitstunFromAttackHit atkHit (_config enemyData)
            hurtSecs    = case behavior of
                HurtBehavior hurtTtl _
                    | hurtTtl > hitstunSecs -> hurtTtl
                _                           -> hitstunSecs

            dir                            = maybe (E._dir enemy) flipDirection (_dir (atkHit :: AttackHit))
            behavior'                      = HurtBehavior hurtSecs hurtType
            enemyData'                     = enemyData {_behavior = behavior'}
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
                unlessM ((justGotHit ||) <$> readSettingsConfig _debug _disableEnemyHurtSfx) $ if
                    | Just explodeTimerTtl <- _explodeTimerTtl enemyData, explodeTimerTtl <= 0.0 ->
                        writeMsgs [mkMsg $ AudioMsgPlaySound hurtDeathSoundPath atkPos]
                    | otherwise                                                                  ->
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
        cfg            = _bomb (_config enemyData :: EnemyConfig)
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
            _                     -> False

        atkPos                  = _intersectPos atkHit
        justGotHit              = enemyJustGotHit enemy
        flags                   = _flags enemy
        atkAlwaysLaunches       = _alwaysLaunches atkHit
        atkVel@(Vel2 _ atkVelY) = _vel (atkHit :: AttackHit)
        atkDmg                  = _damage (atkHit :: AttackHit)
        hp                      = decreaseEnemyHealth atkDmg enemy
        isAirVulnerable         = inAir && not isWeakAtkHitVel

updateGroundResponse :: MsgsWrite UpdateEnemyMsgsPhase m => EnemyUpdateGroundResponse BombEnemyData m
updateGroundResponse groundY enemy
    | velY >= 0.0 =
        let
            x              = vecX $ E._pos enemy
            enemyData      = _data enemy
            behavior       = _behavior enemyData

            isPrevLaunched     = isLaunchedBehavior behavior
            hurtFallenBehavior = HurtBehavior 0.0 FallenHurt
            behavior'          = case behavior of
                HurtBehavior _ LaunchUpHurt -> hurtFallenBehavior
                HurtBehavior _ AirHurt      -> hurtFallenBehavior
                LaunchedBehavior _          -> hurtFallenBehavior
                _                           -> behavior

            velX'
                | isHurtBehavior behavior' = 0.0
                | otherwise                = velX
        in do
            when isPrevLaunched $
                let effectDrawScale = _groundImpactEffectDrawScale $ _bomb (_config enemyData :: EnemyConfig)
                in writeMsgs $ enemyGroundImpactMessages effectDrawScale enemy

            return $ enemy
                { _data  = enemyData
                    { _behavior = behavior'}
                , _pos   = Pos2 x groundY
                , _vel   = Vel2 velX' 0.1
                , _flags = flags
                }

    | otherwise = return $ enemy {_flags = flags}

    where
        Vel2 velX velY = E._vel enemy
        flags          = (_flags enemy) {_touchingGround = True}

updateHangtimeResponse :: EnemyUpdateHangtimeResponse BombEnemyData
updateHangtimeResponse hangtimeSecs enemy
    | enemyTouchingGround enemy = enemy
    | otherwise                 = enemy
        { _data = (_data enemy) {_behavior = LaunchedBehavior hangtimeSecs}
        }
