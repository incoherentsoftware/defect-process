module Level.Room.Event.LightningStrike
    ( mkRoomEventLightningStrike
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.List as L
import qualified Data.Set as S

import Attack
import Attack.Hit
import Attack.Projectile
import Collision
import Configs
import Configs.All.Level
import Constants
import FileCache
import Id
import InfoMsg.Util
import Msg
import Projectile as P
import Util
import Window.Graphics
import World.GoldDrop
import World.Util

initialWaveCooldownSecs = 0.75 :: Secs
perWaveCooldownSecs     = 0.75 :: Secs

lightningStrikeAtkDescPath =
    PackResourceFilePath "data/levels/level-items.pack" "lightning-pre-strike.atk" :: PackResourceFilePath

data LightningStrikeData = LightningStrikeData
    { _hitPlayerHashedIds :: S.Set HashedId
    , _remainingWaves     :: Int
    , _waveCooldownTtl    :: Secs
    , _waveMsgIds         :: S.Set MsgId
    , _attackDesc         :: AttackDescription
    , _config             :: LevelConfig
    }

mkLightningStrikeData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> m LightningStrikeData
mkLightningStrikeData _ = do
    cfg     <- _level <$> readConfigs
    atkDesc <- loadPackAttackDescription lightningStrikeAtkDescPath

    return $ LightningStrikeData
        { _hitPlayerHashedIds = S.empty
        , _remainingWaves     = _eventLightningInitialRemainingWaves cfg
        , _waveCooldownTtl    = initialWaveCooldownSecs
        , _waveMsgIds         = S.empty
        , _attackDesc         = atkDesc
        , _config             = cfg
        }

mkRoomEventLightningStrike :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> m (Some Projectile)
mkRoomEventLightningStrike pos = do
    lightningStrikeData <- mkLightningStrikeData pos
    msgId               <- newId
    let dummyHbx         = dummyHitbox pos

    return . Some $ (mkProjectile lightningStrikeData msgId dummyHbx maxSecs)
        { _think  = thinkLightningStrike
        , _update = updateLightningStrike
        }

readPlayerGroundPos :: MsgsRead ThinkProjectileMsgsPhase m => Projectile LightningStrikeData -> m Pos2
readPlayerGroundPos lightningStrike = processMsgs <$> readMsgs
    where
        processMsgs :: [InfoMsgPayload] -> Pos2
        processMsgs []     = hitboxBotCenter $ projectileHitbox lightningStrike
        processMsgs (p:ps) = case p of
            InfoMsgPlayer playerInfo -> _groundBeneathPos playerInfo
            _                        -> processMsgs ps

thinkLightningStrike
    :: (ConfigsRead m, MonadIO m, MsgsRead ThinkProjectileMsgsPhase m)
    => ProjectileThink LightningStrikeData m
thinkLightningStrike lightningStrike
    | null waveMsgIds && waveCooldownTtl <= 0.0 = if
        | remainingWaves <= 0 -> do
            cfg <- _level <$> readConfigs
            let
                dropGoldPos           = hitboxCenter $ projectileHitbox lightningStrike
                playerHitCount        = S.size $ _hitPlayerHashedIds lightningStrikeData
                perWaveGoldValue      = _eventLightningPerWaveGoldValue cfg
                initialRemainingWaves = _eventLightningInitialRemainingWaves cfg
                dropGoldValue         = perWaveGoldValue * GoldValue (initialRemainingWaves - playerHitCount)

            return
                [ mkMsg $ NewUpdateProjectileMsgAddsM (mkArenaGoldDrops dropGoldPos dropGoldValue)
                , mkMsgTo (ProjectileMsgSetTtl 0.0) msgId
                ]

        | otherwise -> do
            pos           <- readPlayerGroundPos lightningStrike
            Some waveProj <- mkEnemyAttackProjectile pos RightDir (_attackDesc lightningStrikeData)

            let
                update = \p -> p
                    { _data = (P._data p)
                        { _remainingWaves  = remainingWaves - 1
                        , _waveCooldownTtl = perWaveCooldownSecs
                        , _waveMsgIds      = S.singleton $ P._msgId waveProj
                        }
                    }

            return
                [ mkMsg $ NewUpdateProjectileMsgAdd (Some waveProj)
                , mkMsgTo (ProjectileMsgUpdate update) msgId
                , mkMsg RoomMsgKeepPortalBarrierAlive
                ]

    | otherwise = return [mkMsg RoomMsgKeepPortalBarrierAlive]

    where
        lightningStrikeData = _data lightningStrike
        remainingWaves      = _remainingWaves lightningStrikeData
        waveMsgIds          = _waveMsgIds lightningStrikeData
        waveCooldownTtl     = _waveCooldownTtl lightningStrikeData
        msgId               = P._msgId lightningStrike

readWaveMsgIds :: MsgsRead UpdateProjectileMsgsPhase m => LightningStrikeData -> m (S.Set MsgId)
readWaveMsgIds lightningStrikeData =
    let
        processMsg :: S.Set MsgId -> InfoMsgPayload -> S.Set MsgId
        processMsg !msgIds p = case p of
            InfoMsgProjectilePos _ _ msgId -> msgId `S.insert` msgIds
            _                              -> msgIds
    in do
        waveMsgIds <- L.foldl' processMsg S.empty <$> readMsgs
        return $ waveMsgIds `S.intersection` _waveMsgIds lightningStrikeData

readPlayerMsgId :: MsgsRead UpdateProjectileMsgsPhase m => m MsgId
readPlayerMsgId = processMsgs <$> readMsgs
    where
        processMsgs :: [InfoMsgPayload] -> MsgId
        processMsgs []     = NullId
        processMsgs (p:ps) = case p of
            InfoMsgPlayer playerInfo -> _msgId (playerInfo :: PlayerInfo)
            _                        -> processMsgs ps

readHitPlayerHashedIds :: MsgsRead UpdateProjectileMsgsPhase m => m (S.Set HashedId)
readHitPlayerHashedIds = processMsgs <$> (readMsgsTo =<< readPlayerMsgId)
    where
        processMsgs :: [HurtMsgPayload] -> S.Set HashedId
        processMsgs []    = S.empty
        processMsgs (p:_) = case p of
            HurtMsgAttackHit atkHit -> S.singleton $ _hashedId atkHit

updateLightningStrike :: (MonadIO m, MsgsRead UpdateProjectileMsgsPhase m) => ProjectileUpdate LightningStrikeData m
updateLightningStrike lightningStrike = do
    waveMsgIds <- readWaveMsgIds $ P._data lightningStrike

    let
        lightningStrikeData = P._data lightningStrike
        hitPlayerHashedIds  = _hitPlayerHashedIds lightningStrikeData
    hitPlayerHashedIds' <- S.union hitPlayerHashedIds <$> readHitPlayerHashedIds

    return $ lightningStrike
        { _data = lightningStrikeData
            { _hitPlayerHashedIds = hitPlayerHashedIds'
            , _waveCooldownTtl    = max 0.0 (_waveCooldownTtl lightningStrikeData - timeStep)
            , _waveMsgIds         = waveMsgIds
            }
        }
