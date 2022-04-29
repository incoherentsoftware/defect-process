module Enemy
    ( module E
    , module Enemy.Flags
    , module Enemy.Timers
    , module Enemy.Util
    , mkEnemy
    , mkEnemyWithId
    , mkDummyEnemy
    , enemyAttackHitbox
    , enemyAttackId
    , enemyAttackHitVel
    , enemyAttackDamage
    , enemyAttackStagger
    , enemySightPos
    , enemyAttackSprite
    , enemyTouchingGround
    , enemyTouchingWall
    , enemyJustGotHit
    , enemyDead
    , drawEnemy
    , thinkEnemy
    ) where

import Control.Applicative    ((<|>))
import Control.Monad          (unless, when)
import Control.Monad.IO.Class (MonadIO)
import Data.Dynamic           (fromDynamic)
import Data.Foldable          (sequenceA_)
import Data.Maybe             (fromMaybe, isJust, listToMaybe)
import Data.Typeable          (Typeable)
import System.FilePath        (takeBaseName)
import qualified Data.Map as M
import qualified Data.Set as S

import AppEnv
import Attack
import Collision
import Configs
import Configs.All.Enemy
import Configs.All.Settings
import Configs.All.Settings.Debug
import Enemy.DebugText
import Enemy.Flags
import Enemy.Timers
import Enemy.Types as E
import Enemy.Util
import Id
import Msg
import Util
import Window.Graphics
import World.ZIndex

spawnVel    = Vel2 0.0 0.1    :: Vel2
dummyHealth = mkHealth 999999 :: Health

debugHitboxColor       = Color 205 100 0 155  :: Color
debugAttackHitboxColor = Color 183 65 222 155 :: Color

dummyLockOnReticleData = EnemyLockOnReticleData
    { _scale     = 1.0
    , _offset    = zeroPos2
    , _offsetMap = Nothing
    } :: EnemyLockOnReticleData

dummyDeathEffectData = EnemyDeathEffectData
    { _drawScale = NonScaled
    , _offset    = Nothing
    } :: EnemyDeathEffectData

dummySpawnEffectData = EnemySpawnEffectData
    { _drawScale = NonScaled
    , _offset    = Nothing
    , _inAir     = Nothing
    } :: EnemySpawnEffectData

mkEnemy :: (ConfigsRead m, GraphicsRead m, MonadIO m, Typeable d) => d -> Pos2 -> Direction -> m (Enemy d)
mkEnemy enData pos dir = do
    enMsgId <- newId
    mkEnemyWithId enMsgId enData pos dir

mkEnemyWithId
    :: (ConfigsRead m, GraphicsRead m, MonadIO m, Typeable d)
    => MsgId
    -> d
    -> Pos2
    -> Direction
    -> m (Enemy d)
mkEnemyWithId enMsgId enData pos dir = mkEnemyInternal enMsgId enData pos dir EnemyRealType

mkEnemyInternal
    :: (ConfigsRead m, GraphicsRead m, MonadIO m, Typeable d)
    => MsgId
    -> d
    -> Pos2
    -> Direction
    -> EnemyDummyType
    -> m (Enemy d)
mkEnemyInternal enMsgId enData pos dir enDummyType = do
    (health, debugText) <- case enDummyType of
        EnemyRealType  -> do
            health <- readConfig _enemy _defaultHealth
            (health,) . Just <$> mkEnemyDebugText health
        EnemyDummyType -> return (dummyHealth, Nothing)

    debugCfg <- readConfig _settings _debug

    return $ Enemy
        { _data                   = enData
        , _dummyType              = enDummyType
        , _msgId                  = enMsgId
        , _pos                    = pos
        , _vel                    = spawnVel
        , _dir                    = dir
        , _health                 = health
        , _launchTargetY          = Nothing
        , _hitByHashedIds         = S.empty
        , _sprite                 = Nothing
        , _attack                 = Nothing
        , _hitbox                 = const $ dummyHitbox pos
        , _pullable               = const True
        , _lockOnReticleData      = dummyLockOnReticleData
        , _deathEffectData        = dummyDeathEffectData
        , _spawnEffectData        = dummySpawnEffectData
        , _knownPlayerInfo        = Nothing
        , _flags                  = mkEnemyFlags
        , _timers                 = mkEnemyTimers
        , _thinkAI                = const $ return []
        , _updateHurtResponse     = const return
        , _updateGroundResponse   = const return
        , _updateHangtimeResponse = const id
        , _updateSprite           = id
        , _updateDynamic          = updateDynamic
        , _draw                   = Nothing
        , _drawOverlay            = const $ return ()
        , _setDebugBehavior       = const ("",)
        , _bodyZIndex             = enemyBodyZIndex
        , _debugText              = debugText
        , _debugConfig            = debugCfg
        }

mkDummyEnemy :: (ConfigsRead m, GraphicsRead m, MonadIO m, Typeable d) => d -> Pos2 -> Direction -> m (Enemy d)
mkDummyEnemy enData pos dir = do
    enMsgId <- newId
    mkEnemyInternal enMsgId enData pos dir EnemyDummyType

updateDynamic :: Typeable d => EnemyUpdateDynamic d (AppEnv UpdateEnemyMsgsPhase)
updateDynamic dyn enemy
    | Just update <- fromDynamic dyn  = return $ update enemy
    | Just updateM <- fromDynamic dyn = updateM enemy
    | otherwise                       = return enemy

enemyAttackHitbox :: Enemy d -> Maybe Hitbox
enemyAttackHitbox enemy = _attack enemy >>= attackHitbox

enemyAttackId :: Enemy d -> Maybe (Id Attack)
enemyAttackId enemy = (_id :: Attack -> Id Attack) <$> _attack enemy

enemyAttackHitVel :: Enemy d -> Maybe Vel2
enemyAttackHitVel enemy = attackHitVel =<< _attack enemy

enemyAttackDamage :: Enemy d -> Maybe Damage
enemyAttackDamage enemy = attackDamage <$> _attack enemy

enemyAttackStagger :: Enemy d -> Maybe Stagger
enemyAttackStagger enemy = attackStagger <$> _attack enemy

enemySightPos :: Enemy d -> Pos2
enemySightPos enemy = Pos2 (x + w / 2.0) y
    where
        hitbox   = enemyHitbox enemy
        Pos2 x y = hitboxTopLeft hitbox
        w        = hitboxWidth hitbox

enemyAttackSprite :: Enemy d -> Maybe Sprite
enemyAttackSprite enemy = attackSprite <$> _attack enemy

enemyTouchingGround :: Enemy d -> Bool
enemyTouchingGround = _touchingGround . _flags

enemyTouchingWall :: Enemy d -> Bool
enemyTouchingWall enemy = _touchingLeftWall flags || _touchingRightWall flags
    where flags = _flags enemy

enemyJustGotHit :: Enemy d -> Bool
enemyJustGotHit = isJust . _justGotHit . _flags

enemyDead :: Enemy d -> Bool
enemyDead = _dead . _flags

enemyLerpPos :: (GraphicsRead m, MonadIO m) => Enemy d -> m Pos2
enemyLerpPos enemy = graphicsLerpPos pos (Vel2 velX' velY')
    where
        pos            = E._pos enemy
        Vel2 velX velY = _vel enemy
        inStasis       = isEnemyInStasis enemy

        velX'
            | enemyTouchingWall enemy || inStasis   = 0.0
            | otherwise                             = velX
        velY'
            | enemyTouchingGround enemy || inStasis = 0.0
            | otherwise                             = velY

drawEnemyDebugHitboxes :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => Enemy d -> m ()
drawEnemyDebugHitboxes enemy =
    let
        hbx    = enemyHitbox enemy
        atkHbx = enemyAttackHitbox enemy
    in whenM (readSettingsConfig _debug _drawEntityHitboxes) $ do
        drawHitbox debugHitboxColor debugHitboxZIndex hbx
        sequenceA_ $ drawHitbox debugAttackHitboxColor debugHitboxZIndex <$> atkHbx

drawEnemy :: Enemy d -> AppEnv DrawMsgsPhase ()
drawEnemy enemy = case _draw enemy of
    Just drawFn -> drawFn enemy >> drawEnemyOverlay
    Nothing     ->
        let
            isStasis = _stasisTtl (_timers enemy) > 0.0
            dir      = E._dir enemy
            hp       = E._health enemy
            spr      = case _attack enemy of
                Just attack
                    | not (isHealthZero hp) -> Just $ attackSprite attack
                _                           -> E._sprite enemy
        in do
            pos <- enemyLerpPos enemy

            when isStasis $
                setGraphicsBlendMode BlendModeAdditive
            sequenceA_ $ drawSprite pos dir (_bodyZIndex enemy) <$> spr
            when isStasis $
                setGraphicsBlendMode BlendModeAlpha

            drawEnemyOverlay

            drawEnemyDebugHitboxes enemy
            case _debugText enemy of
                Nothing       -> return ()
                Just debugTxt -> drawEnemyDebugText pos debugTxt

    where drawEnemyOverlay = (_drawOverlay enemy) enemy

lockOnReticleDataMsgs :: Enemy d -> [Msg ThinkEnemyMsgsPhase]
lockOnReticleDataMsgs enemy
    | isHealthZero (_health enemy) = []
    | otherwise                    =
        let
            lockOnReticleData = _lockOnReticleData enemy
            offsetMap         = _offsetMap lockOnReticleData
            offset            = _offset (lockOnReticleData :: EnemyLockOnReticleData)
            reticleOffset     = fromMaybe offset $ do
                spr            <- (attackSprite <$> E._attack enemy) <|> E._sprite enemy
                let sprFileName = takeBaseName $ _filePath (spr :: Sprite)
                offsets        <- M.lookup sprFileName =<< offsetMap
                let frameIndex  = _frameIndex spr
                listToMaybe $ drop (_int (frameIndex :: FrameIndex)) offsets

            lockOnData = EnemyLockOnData
                { _enemyId       = E._msgId enemy
                , _enemyHitbox   = (_hitbox enemy) enemy
                , _enemyHealth   = _health enemy
                , _enemyVel      = E._vel enemy
                , _reticleScale  = _scale (lockOnReticleData :: EnemyLockOnReticleData)
                , _reticleOffset = vecFlip reticleOffset (E._dir enemy)
                }
        in [mkMsg $ InfoMsgEnemyLockOnReticle lockOnData]

thinkEnemy :: Enemy d -> AppEnv ThinkEnemyMsgsPhase ()
thinkEnemy enemy = unless (isEnemyInStasis enemy) $ do
    writeMsgs =<< (_thinkAI enemy) enemy
    writeMsgs $ lockOnReticleDataMsgs enemy
    writeMsgs $ maybe [] thinkAttack (_attack enemy)
