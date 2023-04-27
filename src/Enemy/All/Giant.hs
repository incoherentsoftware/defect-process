module Enemy.All.Giant
    ( allGiantEnemyPreloadPackFilePaths
    , mkGiantEnemy
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T

import Attack
import Attack.Hit
import Collision
import Configs
import Configs.All.Enemy
import Configs.All.Enemy.Giant
import Configs.All.EnemyLockOn
import Enemy as E
import Enemy.All.Giant.AI
import Enemy.All.Giant.Behavior
import Enemy.All.Giant.Data
import Enemy.All.Giant.Sprites
import FileCache
import Msg
import Particle.All.AttackSpecks
import Particle.All.EnemyHurt
import Util
import Window.Graphics
import World.ZIndex

allGiantEnemyPreloadPackFilePaths = ["data/enemies/giant-enemy.pack"] :: [FilePath]

onlyPunchDebugFlag = T.toLower "onlyPunch" :: T.Text
onlySmashDebugFlag = T.toLower "onlySmash" :: T.Text

mkGiantEnemy :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> Direction -> m (Some Enemy)
mkGiantEnemy pos dir = do
    enemyData         <- mkGiantEnemyData
    enemy             <- mkEnemy enemyData pos dir
    giantCfg          <- readEnemyConfig _giant
    lockOnReticleData <- readEnemyLockOnConfig _giant
    tauntedData       <- mkEnemyTauntedData $ _tauntUnderlayDrawScale giantCfg

    return . Some $ enemy
        { _type                 = Just GiantEnemy
        , _health               = _health (giantCfg :: GiantEnemyConfig)
        , _hitbox               = giantEnemyHitbox
        , _pullable             = const False
        , _lockOnReticleData    = lockOnReticleData
        , _tauntedData            = Just tauntedData
        , _thinkAI              = thinkAI
        , _updateHurtResponse   = updateHurtResponse
        , _updateGroundResponse = updateGroundResponse
        , _updateSprite         = updateSpr
        , _setDebugBehavior     = setDebugBehavior
        , _bodyZIndex           = enemyBodyBigZIndex
        }

giantEnemyHitbox :: EnemyHitbox GiantEnemyData
giantEnemyHitbox enemy
    | _behavior enemyData `elem` [SpawnBehavior, DeathBehavior] = dummyHitbox $ Pos2 x (y - height / 2.0)
    | otherwise                                                 =
        let pos = Pos2 (x - width / 2.0) (y - height)
        in rectHitbox pos width height
    where
        enemyData = _data enemy
        Pos2 x y  = E._pos enemy
        cfg       = _giant (_config enemyData :: EnemyConfig)
        width     = _width (cfg :: GiantEnemyConfig)
        height    = _height (cfg :: GiantEnemyConfig)

updateSpr :: EnemyUpdateSprite GiantEnemyData
updateSpr enemy
    | isHealthZero (E._health enemy) = setDeathSpr
    | otherwise                      = case _behavior enemyData of
        IdleBehavior _    -> setOrUpdateEnemySpr $ _idle sprs
        SpawnBehavior     -> setOrUpdateEnemySpr $ _spawn sprs
        DeathBehavior     -> setDeathSpr
        AdvanceBehavior _ -> setOrUpdateEnemySpr $ _walk sprs
        RetreatBehavior _ -> setOrUpdateEnemySpr $ _backWalk sprs
        AttackBehavior    -> clearEnemySprite enemy
    where
        setOrUpdateEnemySpr = \spr -> setOrUpdateEnemySprite enemy spr
        setDeathSpr         = (setOrUpdateEnemySpr (_death sprs)) {_draw = Just drawEnemyDeath}

        enemyData = _data enemy
        sprs      = _sprites enemyData

updateHurtResponse :: (ConfigsRead m, MsgsWrite UpdateEnemyMsgsPhase m) => EnemyUpdateHurtResponse GiantEnemyData m
updateHurtResponse atkHit enemy =
    let
        atkPos     = _intersectPos atkHit
        atkDmg     = _damage (atkHit :: AttackHit)
        justGotHit = enemyJustGotHit enemy
        hp         = decreaseEnemyHealth atkDmg enemy
    in do
        when (atkDmg > 0 && not justGotHit) $
            let
                cfg            = _giant (_config (_data enemy) :: EnemyConfig)
                hurtEffectData = _hurtEffectData cfg
            in writeMsgs
                [ mkMsg $ ParticleMsgAddM (mkEnemyHurtParticle enemy atkHit hurtEffectData)
                , mkMsg $ ParticleMsgAddM (mkAttackSpecksParticle atkHit)
                , mkMsg $ AudioMsgPlaySound enemySuperArmorSoundPath atkPos
                ]

        return $ enemy
            { _health = hp
            , _flags  = (_flags enemy) {_justGotHit = Just atkPos}
            }

updateGroundResponse :: Monad m => EnemyUpdateGroundResponse GiantEnemyData m
updateGroundResponse groundY enemy = return $ if
    | velY >= 0.0 ->
        let x = vecX $ E._pos enemy
        in enemy
            { _pos   = Pos2 x groundY
            , _vel   = Vel2 velX 0.1
            , _flags = flags
            }

    | otherwise -> enemy {_flags = flags}

    where
        Vel2 velX velY = E._vel enemy
        flags          = (_flags enemy) {_touchingGround = True}

setDebugBehavior :: EnemySetDebugBehavior GiantEnemyData
setDebugBehavior flags enemy = (debugTxt, enemy {_data = enemyData'})
    where
        enemyData = _data enemy
        cfg       = _config enemyData
        giantCfg = _giant (cfg :: EnemyConfig)

        (giantCfg', debugTxt)
            | onlyPunchDebugFlag `elem` flags =
                ( giantCfg
                    { _punchRangeX = abs $ _punchRangeX giantCfg
                    , _smashRangeX = -1.0 * abs (_smashRangeX giantCfg)
                    }
                , " | only punch attack"
                )

            | onlySmashDebugFlag `elem` flags =
                ( giantCfg
                    { _punchRangeX = -1.0 * abs (_punchRangeX giantCfg)
                    , _smashRangeX = abs $ _smashRangeX giantCfg
                    }
                , " | only smash attack"
                )

            | otherwise = (giantCfg, "")

        enemyData' = enemyData
            { _config = cfg {_giant = giantCfg'}
            }
