module Enemy.All.Boss
    ( allBossEnemyPreloadPackFilePaths
    , mkBossEnemy
    ) where

import Control.Monad          (unless, when)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import Attack
import Attack.Hit
import Collision
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Boss
import Configs.All.EnemyLockOn
import Configs.All.Settings
import Configs.All.Settings.Debug
import Enemy as E
import Enemy.All.Boss.AI
import Enemy.All.Boss.AttackDescriptions
import Enemy.All.Boss.Behavior
import Enemy.All.Boss.Data
import Enemy.All.Boss.Images
import Enemy.All.Boss.Sprites
import Enemy.All.Boss.Util
import FileCache
import Msg
import Particle.All.AttackSpecks
import Particle.All.EnemyHurt
import Particle.All.Simple
import Util
import Window.Graphics
import World.ZIndex

allBossEnemyPreloadPackFilePaths =
    [ "data/enemies/boss-enemy.pack"
    , "data/enemies/boss-enemy-attack1.pack"
    , "data/enemies/boss-enemy-attack2.pack"
    , "data/enemies/boss-enemy-attack3.pack"
    , "data/enemies/boss-enemy-death.pack"
    , "data/enemies/boss-enemy-spawn.pack"
    ] :: [FilePath]

guardEffectPath = PackResourceFilePath "data/enemies/boss-enemy.pack" "guard-effect.spr" :: PackResourceFilePath
guardSoundPath  = "event:/SFX Events/Enemy/Boss/guard-deflect"                           :: FilePath
hurtSoundPath   = "event:/SFX Events/Enemy/Boss/hurt-long"                               :: FilePath

onlyBlobDebugFlag    = T.toLower "onlyBlob"    :: T.Text
onlyDogDebugFlag     = T.toLower "onlyDog"     :: T.Text
onlyFlailDebugFlag   = T.toLower "onlyFlail"   :: T.Text
onlyGiantDebugFlag   = T.toLower "onlyGiant"   :: T.Text
onlyHammerDebugFlag  = T.toLower "onlyHammer"  :: T.Text
onlyTurretDebugFlag  = T.toLower "onlyTurret"  :: T.Text
onlyHopDebugFlag     = T.toLower "onlyHop"     :: T.Text
onlyFlyingDebugFlag  = T.toLower "onlyFlying"  :: T.Text
onlyLankyDebugFlag   = T.toLower "onlyLanky"   :: T.Text
alwaysGuardDebugFlag = T.toLower "alwaysGuard" :: T.Text

mkBossEnemy :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> Direction -> m (Some Enemy)
mkBossEnemy pos dir = do
    enemyData         <- mkBossEnemyData pos
    enemy             <- mkEnemy enemyData pos dir
    bossCfg           <- readEnemyConfig _boss
    lockOnReticleData <- readEnemyLockOnConfig _boss

    return . Some $ enemy
        { _type                   = Just BossEnemy
        , _health                 = _health (bossCfg :: BossEnemyConfig)
        , _hitbox                 = bossEnemyHitbox
        , _pullable               = bossEnemyPullable
        , _lockOnReticleData      = lockOnReticleData
        , _thinkAI                = thinkAI
        , _updateHurtResponse     = updateHurtResponse
        , _updateGroundResponse   = updateGroundResponse
        , _updateHangtimeResponse = updateHangtimeResponse
        , _updateSprite           = updateSpr
        , _drawOverlay            = drawOverlay
        , _setDebugBehavior       = setDebugBehavior
        , _bodyZIndex             = bossBodyZIndex
        }

bossEnemyHitbox :: EnemyHitbox BossEnemyData
bossEnemyHitbox enemy = case behavior of
    SpawnBehavior              -> dummyHbx
    _
        | isPhasedHitbox enemy -> dummyHbx
        | otherwise            -> hbx
    where
        enemyData = _data enemy
        behavior  = _behavior enemyData
        Pos2 x y  = E._pos enemy
        cfg       = _boss (_config enemyData :: EnemyConfig)
        width     = _width (cfg :: BossEnemyConfig)
        height    = _height (cfg :: BossEnemyConfig)
        pos       = Pos2 (x - width / 2.0) (y - height)
        hbx       = rectHitbox pos width height
        dummyHbx  = dummyHitbox $ Pos2 x (y - height / 2.0)

bossEnemyPullable :: EnemyPullable BossEnemyData
bossEnemyPullable enemy = not $ isSuperArmorState enemy || isGuardState enemy

updateSpr :: EnemyUpdateSprite BossEnemyData
updateSpr enemy = case _behavior enemyData of
    IdleBehavior _                  -> setOrUpdateEnemySpr $ _idle sprs
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

    GuardBehavior   -> setOrUpdateEnemySpr $ _guard sprs
    AirGuardBehavior
        | inAir     -> setOrUpdateEnemySpr $ _airGuard sprs
        | otherwise -> setOrUpdateEnemySpr $ _airGuardLand sprs

    DeathBehavior   -> setOrUpdateEnemySpr $ _death sprs
    AirDeathBehavior
        | inAir     -> setOrUpdateEnemySpr $ _airDeath sprs
        | otherwise -> setOrUpdateEnemySpr $ _airDeathLand sprs

    AttackBehavior      -> clearEnemySprite enemy
    KneelingBehavior _  -> setOrUpdateEnemySpr $ _kneelingImpact sprs
    GetUpBehavior       -> setOrUpdateEnemySpr $ _getUp sprs
    WallSplatBehavior _ -> setOrUpdateEnemySpr $ _wallSplat sprs
    SpawnBehavior       -> setOrUpdateEnemySpr $ _spawn sprs

    where
        setOrUpdateEnemySpr = \spr -> setOrUpdateEnemySprite enemy spr

        enemyData  = _data enemy
        justGotHit = enemyJustGotHit enemy
        sprs       = _sprites enemyData
        velY       = vecY $ E._vel enemy
        inHangtime = enemyInHangtimeVel enemy (_config enemyData)
        inAir      = not $ _touchingGround (_flags enemy)

updateHurtResponse :: (ConfigsRead m, MsgsWrite UpdateEnemyMsgsPhase m) => EnemyUpdateHurtResponse BossEnemyData m
updateHurtResponse atkHit enemy
    | isAnyDeathBehavior behavior = return enemy

    | isGuardState enemy =
        let mkGuardEffectPart = loadSimpleParticle atkPos atkFlippedDir enemyHurtParticleZIndex guardEffectPath
        in do
            writeMsgs
                [ mkMsg $ ParticleMsgAddM mkGuardEffectPart
                , mkMsg $ ParticleMsgAddM (mkAttackSpecksParticle atkHit)
                , mkMsg $ AudioMsgPlaySound guardSoundPath pos
                ]
            return enemy

    | _incapacitatedSecs enemyData >= _maxIncapacitatedSecs cfg =
        let guardBehavior = if onGround then GuardBehavior else AirGuardBehavior
        in return $ enemy
            { _data = enemyData {_behavior = guardBehavior}
            }

    | not isSuperArmor && (isStagger || isAirVulnerable || atkAlwaysLaunches) =
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
                , _launchTargetY = attackHitLaunchTargetY pos atkHit
                , _flags         = flags'
                }

    | otherwise = do
        when (atkDmg > 0 && not justGotHit) $
            let
                audioMsgs
                    | isSuperArmor = [mkMsg $ AudioMsgPlaySound enemySuperArmorSoundPath atkPos]
                    | otherwise    = []
            in writeMsgs $
                [ mkMsg $ ParticleMsgAddM (mkEnemyHurtParticleEx enemy atkHit hurtEffectData WeakHitEffect)
                , mkMsg $ ParticleMsgAddM (mkAttackSpecksParticleEx atkHit WeakHitEffect)
                ] ++ audioMsgs

        return $ enemy
            { _health = hp
            , _flags  = flags {_justGotHit = Just atkPos}
            }

    where
        enemyData      = _data enemy
        cfg            = _boss (_config enemyData :: EnemyConfig)
        hurtEffectData = _hurtEffectData cfg
        behavior       = _behavior enemyData
        onGround       = enemyTouchingGround enemy
        inAir          = not onGround
        pos            = E._pos enemy
        isSuperArmor   = isSuperArmorState enemy

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
            KneelingBehavior _    -> True
            WallSplatBehavior _   -> True
            _                     -> False

        atkAlwaysLaunches = _alwaysLaunches atkHit
        atkPos            = _intersectPos atkHit
        justGotHit        = enemyJustGotHit enemy
        flags             = _flags enemy
        flags'            = flags {_justGotHit = Just atkPos}
        atkDmg            = _damage (atkHit :: AttackHit)
        atkFlippedDir     = maybe (E._dir enemy) flipDirection (_dir (atkHit :: AttackHit))
        hp                = decreaseEnemyHealth atkDmg enemy
        isAirVulnerable   = inAir && not isWeakAtkHitVel

updateGroundResponse :: MsgsWrite UpdateEnemyMsgsPhase m => EnemyUpdateGroundResponse BossEnemyData m
updateGroundResponse groundY enemy
    | velY >= 0.0 =
        let
            x                = vecX $ E._pos enemy
            enemyData        = _data enemy
            minKneelingSecs  = _minFallenSecs $ _config enemyData
            kneelingBehavior = KneelingBehavior minKneelingSecs

            behavior       = _behavior enemyData
            isPrevLaunched = isLaunchedBehavior behavior
            behavior'      = case _behavior enemyData of
                HurtBehavior _ LaunchUpHurt -> kneelingBehavior
                HurtBehavior _ AirHurt      -> kneelingBehavior
                LaunchedBehavior _          -> kneelingBehavior
                _                           -> behavior
        in do
            when isPrevLaunched $
                let effectDrawScale = _groundImpactEffectDrawScale $ _boss (_config enemyData :: EnemyConfig)
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

updateHangtimeResponse :: EnemyUpdateHangtimeResponse BossEnemyData
updateHangtimeResponse hangtimeSecs enemy
    | inAir && not (isGuardState enemy) && not isAnyDeath = enemy
        { _data = enemyData {_behavior = LaunchedBehavior hangtimeSecs}
        }
    | otherwise                                           = enemy

    where
        inAir      = not $ enemyTouchingGround enemy
        enemyData  = _data enemy
        isAnyDeath = isAnyDeathBehavior $ _behavior enemyData

drawOverlay :: (GraphicsReadWrite m, MonadIO m) => EnemyDrawOverlay BossEnemyData m
drawOverlay enemy =
    let
        enemyData    = _data enemy
        healthbarPos = _healthbarBackdropPos $ _boss (_config enemyData :: EnemyConfig)
        imgs         = _images (enemyData :: BossEnemyData)

        health         = E._health enemy
        healthScale    = realToFrac (_value health) / realToFrac (_maxValue health)
        innerImg       = _healthbarInner imgs
        innerImgWidth  = imageWidth innerImg * healthScale
        innerImgHeight = imageHeight innerImg
    in do
        setCameraSpace CameraScreenSpace
        drawImage healthbarPos RightDir enemyUnderBodyZIndex (_healthbarBackdrop imgs)
        drawImageCropped healthbarPos RightDir enemyUnderBodyZIndex innerImgWidth innerImgHeight innerImg
        setCameraSpace CameraWorldSpace

setDebugBehavior :: EnemySetDebugBehavior BossEnemyData
setDebugBehavior flags enemy = (debugTxt, enemy {_data = enemyData'})
    where
        enemyData = _data enemy

        setAttackChoices :: (EnemyAttackDescriptions -> AttackDescription) -> BossEnemyData
        setAttackChoices atkDescF = enemyData
            { _attackShortChoices     = atkDesc NE.:| []
            , _attackMediumAirChoices = atkDesc NE.:| []
            , _attackLongChoices      = atkDesc NE.:| []
            }
            where atkDesc = atkDescF $ _attackDescs enemyData

        (enemyData', debugTxt)
            | onlyBlobDebugFlag `elem` flags   = (setAttackChoices _blob, " | only blob attack")
            | onlyDogDebugFlag `elem` flags    = (setAttackChoices _dog, " | only dog attack")
            | onlyFlailDebugFlag `elem` flags  = (setAttackChoices _flail1, " | only flail attack")
            | onlyGiantDebugFlag `elem` flags  = (setAttackChoices _giant, " | only giant attack")
            | onlyHammerDebugFlag `elem` flags = (setAttackChoices _hammer, " | only hammer attack")
            | onlyTurretDebugFlag `elem` flags = (setAttackChoices _turret1, " | only turret attack")
            | onlyHopDebugFlag `elem` flags    = (setAttackChoices _hop, " | only hop attack")
            | onlyFlyingDebugFlag `elem` flags = (setAttackChoices _flying, " | only flying attack")
            | onlyLankyDebugFlag `elem` flags  = (setAttackChoices _lanky, " | only lanky attack")

            | alwaysGuardDebugFlag `elem` flags =
                let
                    enemyCfg = _config enemyData
                    bossCfg  = _boss (enemyCfg :: EnemyConfig)
                    bossCfg' = bossCfg {_maxIncapacitatedSecs = 0.0}
                in
                    ( enemyData {_config = enemyCfg {_boss = bossCfg'}}
                    , " | always guard"
                    )

            | otherwise = (enemyData, "")
