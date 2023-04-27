module Enemy.All.Lanky
    ( allLankyEnemyPreloadPackFilePaths
    , mkLankyEnemy
    ) where

import Control.Monad          (unless, when)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T

import Attack
import Attack.Hit
import Collision
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Lanky
import Configs.All.EnemyLockOn
import Configs.All.Settings
import Configs.All.Settings.Debug
import Enemy as E
import Enemy.All.Lanky.AI
import Enemy.All.Lanky.Behavior
import Enemy.All.Lanky.Data
import Enemy.All.Lanky.Sprites
import FileCache
import Msg
import Particle.All.AttackSpecks
import Particle.All.EnemyHurt
import Particle.All.Simple
import Util
import Window.Graphics
import World.ZIndex

allLankyEnemyPreloadPackFilePaths = ["data/enemies/lanky-enemy.pack"] :: [FilePath]

hurtSoundPath       = "event:/SFX Events/Enemy/Lanky/hurt" :: FilePath
onlyPillarDebugFlag = T.toLower "onlyPillar"               :: T.Text
onlyBeamDebugFlag   = T.toLower "onlyBeam"                 :: T.Text
infAuraDebugFlag    = T.toLower "infAura"                  :: T.Text
noAuraDebugFlag     = T.toLower "noAura"                   :: T.Text

lankyEnemyPack      = \p -> PackResourceFilePath "data/enemies/lanky-enemy.pack" p
auraHitEffectPath   = lankyEnemyPack "aura-hit-effect.spr"   :: PackResourceFilePath
auraBreakEffectPath = lankyEnemyPack "aura-break-effect.spr" :: PackResourceFilePath

mkLankyEnemy :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> Direction -> m (Some Enemy)
mkLankyEnemy pos dir = do
    enemyData         <- mkLankyEnemyData pos
    enemy             <- mkEnemy enemyData pos dir
    lankyCfg          <- readEnemyConfig _lanky
    lockOnReticleData <- readEnemyLockOnConfig _lanky
    tauntedData       <- mkEnemyTauntedData $ _tauntUnderlayDrawScale lankyCfg

    return . Some $ enemy
        { _type                   = Just LankyEnemy
        , _health                 = _health (lankyCfg :: LankyEnemyConfig)
        , _hitbox                 = lankyEnemyHitbox
        , _pullable               = lankyEnemyPullable
        , _lockOnReticleData      = lockOnReticleData
        , _tauntedData            = Just tauntedData
        , _thinkAI                = thinkAI
        , _updateHurtResponse     = updateHurtResponse
        , _updateGroundResponse   = updateGroundResponse
        , _updateHangtimeResponse = updateHangtimeResponse
        , _updateSprite           = updateSpr
        , _setDebugBehavior       = setDebugBehavior
        , _bodyZIndex             = enemyBodyTallZIndex
        }

lankyEnemyHitbox :: EnemyHitbox LankyEnemyData
lankyEnemyHitbox enemy
    | _behavior enemyData `elem` [SpawnBehavior, DeathBehavior] = dummyHitbox $ Pos2 x (y - height / 2.0)
    | otherwise                                                 = rectHitbox pos width height
    where
        enemyData = _data enemy
        Pos2 x y  = E._pos enemy
        cfg       = _lanky (_config enemyData :: EnemyConfig)
        width     = _width (cfg :: LankyEnemyConfig)
        height    = _height (cfg :: LankyEnemyConfig)
        pos       = Pos2 (x - width / 2.0) (y - height)

lankyEnemyPullable :: EnemyPullable LankyEnemyData
lankyEnemyPullable enemy = not $ hasLankyEnemyDataAura (_data enemy)

updateSpr :: EnemyUpdateSprite LankyEnemyData
updateSpr enemy = case _behavior enemyData of
    IdleBehavior _
        | hasAura                   -> setOrUpdateEnemySpr $ _idleAura sprs
        | otherwise                 -> setOrUpdateEnemySpr $ _idle sprs
    WalkBehavior _
        | hasAura                   -> setOrUpdateEnemySpr $ _walkAura sprs
        | otherwise                 -> setOrUpdateEnemySpr $ _walk sprs
    RetreatBehavior _
        | hasAura                   -> setOrUpdateEnemySpr $ _backWalkAura sprs
        | otherwise                 -> setOrUpdateEnemySpr $ _backWalk sprs
    LaunchedBehavior _
        | velY <= 0.0 || inHangtime -> setOrUpdateEnemySpr $ _launched sprs
        | otherwise                 -> setOrUpdateEnemySpr $ _fall sprs

    HurtBehavior _ hurtType
        | justGotHit -> setEnemyHurtSprite enemy $ case hurtType of
            NormalHurt   -> _hurt sprs
            LaunchUpHurt -> _launchUp sprs
            AirHurt      -> _airHurt sprs
            KneelingHurt -> _kneelingHurt sprs
            WallHurt     -> _wallHurt sprs
        | otherwise  -> updateEnemySprite enemy

    AttackBehavior      -> clearEnemySprite enemy
    KneelingBehavior _  -> setOrUpdateEnemySpr $ _kneelingImpact sprs
    GetUpBehavior       -> setOrUpdateEnemySpr $ _getUp sprs
    WallSplatBehavior _ -> setOrUpdateEnemySpr $ _wallSplat sprs
    AuraBreakBehavior   -> setOrUpdateEnemySpr $ _auraBreak sprs
    SpawnBehavior       -> setOrUpdateEnemySpr $ _spawn sprs
    DeathBehavior       -> (setOrUpdateEnemySpr (_death sprs)) {_draw = Just drawEnemyDeath}

    where
        setOrUpdateEnemySpr = \spr -> setOrUpdateEnemySprite enemy spr

        enemyData  = _data enemy
        hasAura    = hasLankyEnemyDataAura enemyData
        justGotHit = enemyJustGotHit enemy
        sprs       = _sprites enemyData
        velY       = vecY $ E._vel enemy
        inHangtime = enemyInHangtimeVel enemy (_config enemyData)

updateHurtResponse :: (ConfigsRead m, MsgsWrite UpdateEnemyMsgsPhase m) => EnemyUpdateHurtResponse LankyEnemyData m
updateHurtResponse atkHit enemy
    | behavior == DeathBehavior = return enemy

    | not (isHealthZero auraHealth) =
        let
            mkAuraHitEffectParticle =
                loadSimpleParticle atkPos atkFlippedDir enemyAttackProjectileZIndex auraHitEffectPath

            auraHealth'      = decreaseHealth atkDmg auraHealth
            auraBroken       = isHealthZero auraHealth'
            enemyData'
                | auraBroken = enemyData
                    { _auraHealth = auraHealth'
                    , _behavior   = AuraBreakBehavior
                    }
                | otherwise  = enemyData {_auraHealth = auraHealth'}
        in do
            writeMsgs
                [ mkMsg $ ParticleMsgAddM mkAuraHitEffectParticle
                , mkMsg $ ParticleMsgAddM (mkAttackSpecksParticle atkHit)
                , mkMsg $ AudioMsgPlaySound enemySuperArmorSoundPath atkPos
                ]

            when auraBroken $
                let
                    pos                           = E._pos enemy
                    mkAuraBreakEffectParticle     =
                        loadSimpleParticle pos atkFlippedDir enemyAttackProjectileZIndex auraBreakEffectPath
                    auraBreakScreenshakeMagnitude =
                        _auraBreakScreenshakeMagnitude $ _lanky (_config enemyData' :: EnemyConfig)
                in writeMsgs
                    [ mkMsg $ ParticleMsgAddM mkAuraBreakEffectParticle
                    , mkMsg $ WorldMsgScreenshake auraBreakScreenshakeMagnitude
                    ]

            return $ if
                | auraBroken -> enemy
                    { _data   = enemyData'
                    , _dir    = atkFlippedDir
                    , _attack = Nothing
                    }
                | otherwise  -> enemy {_data = enemyData'}

    | isStagger || isAirVulnerable || atkAlwaysLaunches =
        let
            atkVel@(Vel2 _ atkVelY)         = _vel (atkHit :: AttackHit)
            atkVel'
                -- prevent sliding on the ground from downwards aerial attacks
                | onGround && atkVelY > 0.0 = Vel2 0.0 atkVelY
                | otherwise                 = atkVel

            hurtType
                | onGround && (atkVelY < 0.0 || atkAlwaysLaunches) = LaunchUpHurt
                | onGround && atkVelY > 0.0                        = KneelingHurt
                | otherwise                                        = case behavior of
                    HurtBehavior _ t    -> case t of
                        NormalHurt   -> NormalHurt
                        LaunchUpHurt -> AirHurt
                        AirHurt      -> AirHurt
                        KneelingHurt -> KneelingHurt
                        WallHurt     -> WallHurt
                    LaunchedBehavior _  -> AirHurt
                    GetUpBehavior       -> KneelingHurt
                    WallSplatBehavior _ -> WallHurt
                    _                   -> NormalHurt

            hitstunSecs = enemyHitstunFromAttackHit atkHit (_config enemyData)
            hurtSecs    = case behavior of
                HurtBehavior hurtTtl _
                    | hurtTtl > hitstunSecs -> hurtTtl
                _                           -> hitstunSecs

            behavior'  = HurtBehavior hurtSecs hurtType
            enemyData' = enemyData {_behavior = behavior'}
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
                , _dir           = atkFlippedDir
                , _attack        = Nothing
                , _health        = hp
                , _launchTargetY = attackHitLaunchTargetY (E._pos enemy) atkHit
                , _flags         = flags'
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
        cfg            = _lanky (_config enemyData :: EnemyConfig)
        hurtEffectData = _hurtEffectData cfg
        behavior       = _behavior enemyData
        onGround       = enemyTouchingGround enemy
        inAir          = not onGround

        isWeakAtkHitVel = _isWeakVel atkHit
        velY            = vecY $ E._vel enemy
        stagger         = _stagger (atkHit :: AttackHit)
        isStagger       = stagger >= _staggerThreshold cfg || case behavior of
            AuraBreakBehavior     -> True
            HurtBehavior _ _
                | isWeakAtkHitVel -> velY >= 0 && inAir
                | otherwise       -> True
            LaunchedBehavior _
                | isWeakAtkHitVel -> velY >= 0
                | otherwise       -> True
            KneelingBehavior _    -> True
            WallSplatBehavior _   -> True
            _                     -> False

        auraHealth        = _auraHealth enemyData
        atkAlwaysLaunches = _alwaysLaunches atkHit
        atkPos            = _intersectPos atkHit
        justGotHit        = enemyJustGotHit enemy
        flags             = _flags enemy
        flags'            = flags {_justGotHit = Just atkPos}
        atkDmg            = _damage (atkHit :: AttackHit)
        atkFlippedDir     = maybe (E._dir enemy) flipDirection (_dir (atkHit :: AttackHit))
        hp                = decreaseEnemyHealth atkDmg enemy
        isAirVulnerable   = inAir && not isWeakAtkHitVel

updateGroundResponse :: MsgsWrite UpdateEnemyMsgsPhase m => EnemyUpdateGroundResponse LankyEnemyData m
updateGroundResponse groundY enemy
    | velY >= 0.0 =
        let
            x                = vecX $ E._pos enemy
            enemyData        = _data enemy
            minKneelingSecs  = _minFallenSecs $ _config enemyData
            kneelingBehavior = KneelingBehavior minKneelingSecs

            behavior       = _behavior enemyData
            isPrevLaunched = isLaunchedBehavior behavior
            behavior'      = case behavior of
                HurtBehavior _ LaunchUpHurt -> kneelingBehavior
                HurtBehavior _ AirHurt      -> kneelingBehavior
                LaunchedBehavior _          -> kneelingBehavior
                _                           -> behavior
        in do
            when isPrevLaunched $
                let effectDrawScale = _groundImpactEffectDrawScale $ _lanky (_config enemyData :: EnemyConfig)
                in writeMsgs $ enemyGroundImpactMessages effectDrawScale enemy

            return $ enemy
                { _data  = enemyData {_behavior = behavior'}
                , _pos   = Pos2 x groundY
                , _vel   = Vel2 0.0 0.1
                , _flags = flags
                }

    | otherwise = return $ enemy {_flags = flags}

    where
        Vel2 _ velY = E._vel enemy
        flags       = (_flags enemy) {_touchingGround = True}

updateHangtimeResponse :: EnemyUpdateHangtimeResponse LankyEnemyData
updateHangtimeResponse hangtimeSecs enemy
    | inAir && notDead = enemy
        { _data = enemyData {_behavior = LaunchedBehavior hangtimeSecs}
        }
    | otherwise        = enemy
    where
        inAir     = not $ enemyTouchingGround enemy
        enemyData = _data enemy
        notDead   = _behavior enemyData /= DeathBehavior

setDebugBehavior :: EnemySetDebugBehavior LankyEnemyData
setDebugBehavior flags enemy = (debugTxt, enemy {_data = enemyData'})
    where
        enemyData = _data enemy
        cfg       = _config enemyData
        lankyCfg = _lanky (cfg :: EnemyConfig)

        (enemyData', debugTxt)
            | onlyPillarDebugFlag `elem` flags =
                let
                    lankyCfg' = lankyCfg
                        { _beamAtkRangeX   = -1.0 * abs (_beamAtkRangeX lankyCfg)
                        , _summonAtkRangeX = abs $ _summonAtkRangeX lankyCfg
                        }
                    cfg'      = cfg {_lanky = lankyCfg'} :: EnemyConfig
                in (enemyData {_config = cfg'}, " | only pillar attack")

            | onlyBeamDebugFlag `elem` flags =
                let
                    lankyCfg' = lankyCfg
                        { _beamAtkRangeX   = abs $ _beamAtkRangeX lankyCfg
                        , _summonAtkRangeX = -1.0 * abs (_summonAtkRangeX lankyCfg)
                        }
                    cfg'      = cfg {_lanky = lankyCfg'} :: EnemyConfig
                in (enemyData {_config = cfg'}, " | only beam attack")

            | infAuraDebugFlag `elem` flags =
                ( enemyData {_auraHealth = mkHealth maxBound}
                , " | infinite aura"
                )

            | noAuraDebugFlag `elem` flags =
                ( enemyData
                    { _auraHealth = (_auraHealth enemyData) {_value = 0}
                    }
                , " | no aura"
                )

            | otherwise = (enemyData, "")
