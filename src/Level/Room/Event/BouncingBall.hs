module Level.Room.Event.BouncingBall
    ( mkRoomEventBouncingBall
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable          (sequenceA_)
import Data.Functor           ((<&>))
import Data.Maybe             (fromMaybe)
import System.Random          (randomRIO)
import qualified Data.List.NonEmpty as NE

import Attack
import Attack.Hit
import Collision
import Configs
import Configs.All.Level
import Constants
import Enemy as E
import FileCache
import Id
import Msg
import Particle.All.Simple
import Util
import Window.Graphics
import World.GoldDrop
import World.ZIndex

packPath            = \f -> PackResourceFilePath "data/levels/level-items.pack" f
appearSpritePath    = packPath "bouncing-ball-appear.spr"     :: PackResourceFilePath
idleSpritePath      = packPath "bouncing-ball-idle.spr"       :: PackResourceFilePath
disappearSpritePath = packPath "bouncing-ball-disappear.spr"  :: PackResourceFilePath
hitEffectPath       = packPath "bouncing-ball-hit-effect.spr" :: PackResourceFilePath

bounceSoundPath = "event:/SFX Events/Level/bouncing-ball-bounce" :: FilePath
hitSoundPath    = "event:/SFX Events/Level/bouncing-ball-hit"    :: FilePath
idleSoundPath   = "event:/SFX Events/Level/bouncing-ball-idle-c" :: FilePath

startingOffset  = Pos2 0.0 (-200.0)           :: Pos2
damageThreshold = Damage 1                    :: Damage
width           = 78.0                        :: Float
height          = 78.0                        :: Float
minAngle        = toRadians $ Degrees (-75.0) :: Radians
maxAngle        = toRadians $ Degrees 75.0    :: Radians
noDropOpacity   = Opacity 0.4                 :: Opacity
roomTopOffsetY  = 178.0                       :: PosY

data BouncingBallData = BouncingBallData
    { _ttl          :: Secs
    , _noDropTtl    :: Secs
    , _appearSprite :: Sprite
    , _idleSprite   :: Sprite
    , _config       :: LevelConfig
    }

mkBouncingBallData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m BouncingBallData
mkBouncingBallData = do
    appearSpr <- loadPackSprite appearSpritePath
    idleSpr   <- loadPackSprite idleSpritePath
    cfg       <- _level <$> readConfigs

    return $ BouncingBallData
        { _ttl          = _eventBouncingBallAliveSecs cfg
        , _noDropTtl    = 0.0
        , _appearSprite = appearSpr
        , _idleSprite   = idleSpr
        , _config       = cfg
        }

mkRoomEventBouncingBall :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> m (Some Enemy)
mkRoomEventBouncingBall pos = do
    bouncingBallData <- mkBouncingBallData
    let pos'          = pos `vecAdd` startingOffset

    mkDummyEnemy bouncingBallData pos' RightDir <&> \e -> Some $ e
        { _sprite               = Just $ _appearSprite bouncingBallData
        , _hitbox               = bouncingBallHitbox
        , _pullable             = const False
        , _lockOnReticleData    = _eventBouncingBallLockOnReticleData $ _config (bouncingBallData :: BouncingBallData)
        , _thinkAI              = thinkBouncingBall
        , _draw                 = Just drawBouncingBall
        , _updateHurtResponse   = bouncingBallHurtResponse
        , _updateGroundResponse = bouncingBallGroundResponse
        }

bouncingBallHitbox :: EnemyHitbox BouncingBallData
bouncingBallHitbox bouncingBall
    | isAppearSpr = dummyHitbox pos
    | otherwise   =
        let pos' = pos `vecSub` Pos2 (width / 2.0) (height / 2.0)
        in rectHitbox pos' width height
    where
        bouncingBallData = E._data bouncingBall
        appearSpr        = _appearSprite bouncingBallData
        isAppearSpr      = maybe False (== appearSpr) (E._sprite bouncingBall)
        pos              = E._pos bouncingBall

readIsAtRoomTopBounds :: MsgsRead ThinkEnemyMsgsPhase m => Enemy BouncingBallData -> m Bool
readIsAtRoomTopBounds bouncingBall = processMsgs <$> readMsgs
    where
        processMsgs :: [InfoMsgPayload] -> Bool
        processMsgs []     = False
        processMsgs (p:ps) = case p of
            InfoMsgRoomTopBounds topBounds -> hitboxTop (bouncingBallHitbox bouncingBall) < topBounds + roomTopOffsetY
            _                              -> processMsgs ps

thinkBouncingBall :: MsgsRead ThinkEnemyMsgsPhase m => EnemyUpdateAI BouncingBallData m
thinkBouncingBall bouncingBall = do
    let
        pos               = E._pos bouncingBall
        dir               = E._dir bouncingBall
        bouncingBallMsgId = E._msgId bouncingBall

        ttl  = _ttl (E._data bouncingBall :: BouncingBallData)
        ttl' = max 0.0 (ttl - timeStep)

        disappearMsgs
            | ttl' <= 0.0 =
                [ mkMsg $ ParticleMsgAddM (loadSimpleParticle pos dir worldEffectZIndex disappearSpritePath)
                , mkMsgTo EnemyMsgSetDead bouncingBallMsgId
                , mkMsg RoomMsgRemovePortalBarrier
                ]
            | otherwise   = []

    isAtRoomTopBounds <- readIsAtRoomTopBounds bouncingBall

    let
        update = \e ->
            let
                eData                   = E._data e
                vel@(Vel2 velX velY)    = E._vel e
                flags                   = E._flags e
                vel'@(Vel2 velX' velY') = if
                    | isAtRoomTopBounds && velY < 0.0        -> Vel2 velX (-velY)
                    | _touchingGround flags && velY > 0.0    -> Vel2 velX (-velY)
                    | _touchingLeftWall flags && velX < 0.0  -> Vel2 (-velX) velY
                    | _touchingRightWall flags && velX > 0.0 -> Vel2 (-velX) velY
                    | otherwise                              -> vel

                appearSpr = _appearSprite eData
                idleSpr   = _idleSprite eData
                spr       = fromMaybe (_idleSprite eData) (E._sprite bouncingBall)
                spr'      = if
                    | spr == appearSpr && spriteFinished spr -> idleSpr
                    | otherwise                              -> updateSprite spr
            in do
                writeMsgs [mkMsg $ AudioMsgPlaySoundContinuous idleSoundPath (hashId bouncingBallMsgId) pos]
                when (not (velX' `approxEq` velX) || not (velY' `approxEq` velY)) $
                    writeMsgs [mkMsg $ AudioMsgPlaySound bounceSoundPath pos]

                return $ e
                    { E._data   = eData
                        { _ttl       = _ttl eData - timeStep
                        , _noDropTtl = _noDropTtl eData - timeStep
                        }
                    , E._vel    = vel'
                    , E._sprite = Just spr'
                    }

    return $ mkMsgTo (EnemyMsgUpdateM update) bouncingBallMsgId:disappearMsgs

drawBouncingBall :: (GraphicsReadWrite m, MonadIO m) => EnemyDraw BouncingBallData m
drawBouncingBall bouncingBall =
    let
        pos = E._pos bouncingBall
        dir = E._dir bouncingBall

        opacity
            | _noDropTtl (E._data bouncingBall) > 0.0 = noDropOpacity
            | otherwise                               = FullOpacity
    in sequenceA_ $ do
        spr <- E._sprite bouncingBall
        Just $ drawSpriteEx pos dir worldBehindProjectileZIndex 0.0 opacity NonScaled spr

bouncingBallRandomVel :: MonadIO m => LevelConfig -> m Vel2
bouncingBallRandomVel cfg = do
    angle  <- liftIO $ randomRIO (minAngle, maxAngle)
    angle' <- (angle +) <$> randomChoice (0.0 NE.:| [pi])
    speed  <- liftIO $ randomRIO (_eventBouncingBallMinSpeed cfg, _eventBouncingBallMaxSpeed cfg)
    let vec = Vec2 (cos angle') (sin angle')
    return $ toVel2 (vecNormalize vec) `vecMul` speed

bouncingBallHurtResponse :: (MonadIO m, MsgsWrite UpdateEnemyMsgsPhase m) => EnemyUpdateHurtResponse BouncingBallData m
bouncingBallHurtResponse atkHit bouncingBall
    | _damage (atkHit :: AttackHit) >= damageThreshold && _noDropTtl bouncingBallData <= 0.0 =
        let
            pos           = E._pos bouncingBall
            cfg           = _config (bouncingBallData :: BouncingBallData)
            dropGoldValue = if
                | _isRanged (atkHit :: AttackHit) -> _eventBouncingBallDropRangedGoldValue cfg
                | otherwise                       -> _eventBouncingBallDropMeleeGoldValue cfg
        in do
            writeMsgs
                [ mkMsg $ NewUpdateProjectileMsgAddM (mkGoldChunkGoldDrop pos dropGoldValue)
                , mkMsg $ ParticleMsgAddM (loadSimpleParticle pos RightDir worldEffectZIndex hitEffectPath)
                , mkMsg $ AudioMsgPlaySound hitSoundPath pos
                ]
            vel <- bouncingBallRandomVel cfg

            return $ bouncingBall
                { E._data = bouncingBallData {_noDropTtl = _eventBouncingBallDropCooldownSecs cfg}
                , E._vel  = vel
                }

    | otherwise = return bouncingBall

    where bouncingBallData = E._data bouncingBall

bouncingBallGroundResponse :: Monad m => EnemyUpdateGroundResponse BouncingBallData m
bouncingBallGroundResponse _ bouncingBall = return $ if
    | velY >= 0.0 -> bouncingBall {_flags = (_flags bouncingBall) {_touchingGround = True}}
    | otherwise   -> bouncingBall
    where velY = vecY $ E._vel bouncingBall
