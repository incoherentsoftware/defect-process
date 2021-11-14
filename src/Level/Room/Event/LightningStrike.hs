module Level.Room.Event.LightningStrike
    ( mkRoomEventLightningStrike
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Random.Shuffle  (shuffleM)
import qualified Data.List as L
import qualified Data.Set as S

import AppEnv
import Attack.Hit
import Collision
import Configs
import Configs.All.Level
import Constants
import Id
import InfoMsg.Util
import Level.Room.Event.LightningStrike.Projectile
import Msg
import Projectile as P
import Util
import World.GoldDrop
import World.Util

initialWaveCooldownSecs = 0.75 :: Secs
perWaveCooldownSecs     = 0.75 :: Secs

type MkWave = Pos2 -> AppEnv ThinkProjectileMsgsPhase [Some Projectile]

outsideInMkWave =
    [ \pos -> sequence [mkRelativeLightningProjectile pos 0.05, mkRelativeLightningProjectile pos 0.95]
    , \pos -> sequence [mkRelativeLightningProjectile pos 0.15, mkRelativeLightningProjectile pos 0.85]
    , \pos -> sequence [mkRelativeLightningProjectile pos 0.25, mkRelativeLightningProjectile pos 0.75]
    , \pos -> sequence [mkRelativeLightningProjectile pos 0.35, mkRelativeLightningProjectile pos 0.65]
    , \pos -> sequence [mkRelativeLightningProjectile pos 0.45, mkRelativeLightningProjectile pos 0.55]
    ] :: [MkWave]

inOutsideMkWave =
    [ \pos -> sequence [mkRelativeLightningProjectile pos 0.5]
    , \pos -> sequence [mkRelativeLightningProjectile pos 0.387, mkRelativeLightningProjectile pos 0.612]
    , \pos -> sequence [mkRelativeLightningProjectile pos 0.275, mkRelativeLightningProjectile pos 0.725]
    , \pos -> sequence [mkRelativeLightningProjectile pos 0.162, mkRelativeLightningProjectile pos 0.837]
    , \pos -> sequence [mkRelativeLightningProjectile pos 0.050, mkRelativeLightningProjectile pos 0.950]
    ] :: [MkWave]

homingMkWave =
    [ \pos -> sequence [mkExactLightningProjectile pos]
    , \pos -> sequence [mkExactLightningProjectile pos]
    , \pos -> sequence [mkExactLightningProjectile pos]
    , \pos -> sequence [mkExactLightningProjectile pos]
    , \pos -> sequence [mkExactLightningProjectile pos]
    ] :: [MkWave]

alternatingMkWave =
    let
        pattern    = \pos -> sequence
            [ mkRelativeLightningProjectile pos 0.05
            , mkRelativeLightningProjectile pos 0.25
            , mkRelativeLightningProjectile pos 0.45
            , mkRelativeLightningProjectile pos 0.65
            , mkRelativeLightningProjectile pos 0.85
            ]
        patternAlt = \pos -> sequence
            [ mkRelativeLightningProjectile pos 0.15
            , mkRelativeLightningProjectile pos 0.35
            , mkRelativeLightningProjectile pos 0.55
            , mkRelativeLightningProjectile pos 0.75
            , mkRelativeLightningProjectile pos 0.95
            ]
    in [pattern, patternAlt, pattern, patternAlt, pattern] :: [MkWave]

allMkWaves = [outsideInMkWave, inOutsideMkWave, homingMkWave, alternatingMkWave] :: [[MkWave]]

data LightningStrikeData = LightningStrikeData
    { _hitPlayerHashedIds :: S.Set HashedId
    , _mkWaves            :: [MkWave]
    , _waveCooldownTtl    :: Secs
    , _waveMsgIds         :: S.Set MsgId
    }

mkLightningStrikeData :: (ConfigsRead m, MonadIO m) => m LightningStrikeData
mkLightningStrikeData = do
    eventLightningNumWaves <- _eventLightningNumWaves . _level <$> readConfigs
    mkWaves                <- concat . take eventLightningNumWaves <$> liftIO (shuffleM allMkWaves)

    return $ LightningStrikeData
        { _hitPlayerHashedIds = S.empty
        , _mkWaves            = mkWaves
        , _waveCooldownTtl    = initialWaveCooldownSecs
        , _waveMsgIds         = S.empty
        }

mkRoomEventLightningStrike :: (ConfigsRead m, MonadIO m) => Pos2 -> m (Some Projectile)
mkRoomEventLightningStrike pos = do
    lightningStrikeData <- mkLightningStrikeData
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

thinkLightningStrike :: ProjectileThink LightningStrikeData (AppEnv ThinkProjectileMsgsPhase)
thinkLightningStrike lightningStrike
    | null waveMsgIds && waveCooldownTtl <= 0.0 = case _mkWaves lightningStrikeData of
        [] -> do
            cfg <- _level <$> readConfigs
            let
                dropGoldPos            = hitboxCenter $ projectileHitbox lightningStrike
                playerHitCount         = S.size $ _hitPlayerHashedIds lightningStrikeData
                perHitPenaltyGoldValue = _eventLightningPerHitPenaltyGoldValue cfg
                dropGoldValue          =
                    _eventLightningGoldValue cfg - (GoldValue playerHitCount) * perHitPenaltyGoldValue

            return
                [ mkMsg $ NewUpdateProjectileMsgAddsM (mkArenaGoldDrops dropGoldPos dropGoldValue)
                , mkMsgTo (ProjectileMsgSetTtl 0.0) msgId
                ]

        (mkWave:mkWaves) -> do
            waveProjs <- mkWave =<< readPlayerGroundPos lightningStrike
            let
                update      = \p -> p
                    { _data = (P._data p)
                        { _mkWaves         = mkWaves
                        , _waveCooldownTtl = perWaveCooldownSecs
                        , _waveMsgIds      = S.fromList [P._msgId wp | Some wp <- waveProjs]
                        }
                    }

            return
                [ mkMsg $ NewUpdateProjectileMsgAdds waveProjs
                , mkMsgTo (ProjectileMsgUpdate update) msgId
                , mkMsg RoomMsgKeepPortalBarrierAlive
                ]

    | otherwise = return [mkMsg RoomMsgKeepPortalBarrierAlive]

    where
        lightningStrikeData = _data lightningStrike
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
