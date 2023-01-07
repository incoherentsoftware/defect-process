module Player.Gun.All.SpikeGun.SpikeRing
    ( SpikeRing(..)
    , mkSpikeRing
    , thinkSpikeRing
    , updateSpikeRing
    , drawSpikeRing
    , activateSpikeRing
    , shootSpikeRing
    , isSpikeRingActive
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Functor           ((<&>))
import Data.Traversable       (for)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Configs
import Configs.All.PlayerGun.SpikeGun
import Constants
import Id
import Msg
import Player
import Player.Gun.All.SpikeGun.Data
import Player.Gun.All.SpikeGun.SpikeAlt
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

spinSoundPath = "event:/SFX Events/Player/Guns/spike-spin" :: FilePath

data SpikeRing = SpikeRing
    { _ringAngle                :: Radians
    , _ringSpikeAltAngleOffsets :: M.Map MsgId Radians
    , _soundHashedId            :: HashedId
    , _sprites                  :: SpikeGunSprites
    , _config                   :: SpikeGunConfig
    }

mkSpikeRing :: MonadIO m => SpikeGunSprites -> SpikeGunConfig -> m SpikeRing
mkSpikeRing sprs cfg = do
    soundHashedId <- hashId <$> newId
    return $ SpikeRing
        { _ringAngle                = 0.0
        , _ringSpikeAltAngleOffsets = M.empty
        , _soundHashedId            = soundHashedId
        , _sprites                  = sprs
        , _config                   = cfg
        }

-- spike alts will disappear when projectiles are cleared switching rooms
readIsSpikeAltAllMissing :: MsgsRead ThinkPlayerMsgsPhase m => S.Set MsgId -> m Bool
readIsSpikeAltAllMissing spikeAltIds
    | S.null spikeAltIds = return False
    | otherwise          = S.null . L.foldl' processMsg S.empty <$> readMsgs
        where
            processMsg :: S.Set MsgId -> InfoMsgPayload -> S.Set MsgId
            processMsg aliveIds p = case p of
                InfoMsgProjectilePos _ _ projId
                    | projId `S.member` spikeAltIds -> projId `S.insert` aliveIds
                _                                   -> aliveIds

replaceMissingSpikeAltMsgs :: Player -> SpikeRing -> [Msg ThinkPlayerMsgsPhase]
replaceMissingSpikeAltMsgs player spikeRing = [mkMsg $ NewThinkProjectileMsgAddsM (sequenceA mkSpikes)]
    where
        playerPos = _pos (player :: Player)
        ringPos   = playerPos `vecAdd` playerChargeOverlayOffset player
        ringAngle = _ringAngle spikeRing
        sprs      = _sprites (spikeRing :: SpikeRing)
        mkSpikes =
            [ mkSpikeAltEx ringPos ringAngle angleOffset sprs msgId
            | (msgId, angleOffset) <- M.toList $ _ringSpikeAltAngleOffsets spikeRing
            ]

thinkSpikeRing :: MsgsRead ThinkPlayerMsgsPhase m => Player -> SpikeRing -> m [Msg ThinkPlayerMsgsPhase]
thinkSpikeRing player spikeRing = do
    let
        playerPos       = _pos (player :: Player)
        ringSpikeAltIds = M.keys $ _ringSpikeAltAngleOffsets spikeRing

    spikeAltMsgs <- readIsSpikeAltAllMissing (S.fromList ringSpikeAltIds) <&> \case
        True  -> replaceMissingSpikeAltMsgs player spikeRing
        False ->
            let
                ringPos   = playerPos `vecAdd` playerChargeOverlayOffset player
                ringAngle = _ringAngle spikeRing
            in map (spikeAltUpdatePosMsg ringPos ringAngle) ringSpikeAltIds

    let
        audioMsgs
            | isSpikeRingActive spikeRing =
                let soundHashedId = _soundHashedId spikeRing
                in [mkMsg $ AudioMsgPlaySoundContinuous spinSoundPath soundHashedId playerPos]
            | otherwise                   = []

    return $ spikeAltMsgs ++ audioMsgs

updateSpikeRing :: SpikeRing -> SpikeRing
updateSpikeRing spikeRing = spikeRing {_ringAngle = ringAngle'}
    where
        ringSpeed                = _ringSpeed $ _config (spikeRing :: SpikeRing)
        ringAngle                = _ringAngle spikeRing + ringSpeed * timeStep
        ringAngle'
            | ringAngle > 2 * pi = ringAngle - 2 * pi
            | otherwise          = ringAngle

drawSpikeRing :: (GraphicsReadWrite m, MonadIO m) => Player -> SpikeRing -> m ()
drawSpikeRing player spikeRing = when (isSpikeRingActive spikeRing) $ do
    let cfg    = _config (spikeRing :: SpikeRing)
    playerPos <- (_pos (player :: Player) `vecAdd`) <$> playerLerpOffset player

    let
        ringPos     = playerPos `vecAdd` playerChargeOverlayOffset player
        ringAngle   = _ringAngle spikeRing
        ringOpacity = _ringOpacity cfg
        sprs        = _sprites (spikeRing :: SpikeRing)
    drawSpriteEx ringPos RightDir playerOverBodyZIndex ringAngle ringOpacity NonScaled (_spikesRing sprs)

activateSpikeRing
    :: (ConfigsRead m, MonadIO m)
    => Int
    -> Player
    -> SpikeRing
    -> m ([Msg ThinkPlayerMsgsPhase], SpikeRing)
activateSpikeRing numSpikes player spikeRing =
    let
        sprs = _sprites (spikeRing :: SpikeRing)
        cfg  = _config (spikeRing :: SpikeRing)

        angleOffsets = case numSpikes of
            1 -> _ringSpikeAltAngleOffsets1 cfg
            2 -> _ringSpikeAltAngleOffsets2 cfg
            3 -> _ringSpikeAltAngleOffsets3 cfg
            4 -> _ringSpikeAltAngleOffsets4 cfg
            _ -> _ringSpikeAltAngleOffsets5 cfg
    in do
        spikeAlts <- for angleOffsets $ \angleOffset ->
            let
                playerPos = _pos (player :: Player)
                ringPos   = playerPos `vecAdd` playerChargeOverlayOffset player
                ringSpeed = _ringSpeed $ _config (spikeRing :: SpikeRing)
                ringAngle = _ringAngle spikeRing + ringSpeed * timeStep
            in mkSpikeAlt ringPos ringAngle angleOffset sprs

        return
            ( [mkMsg $ NewThinkProjectileMsgAdds spikeAlts]
            , spikeRing
                { _ringSpikeAltAngleOffsets = M.fromList
                    [ (P._msgId p, angleOffset)
                    | (Some p, angleOffset) <- zip spikeAlts angleOffsets
                    ]
                }
            )

shootSpikeRing :: Pos2 -> SpikeRing -> ([Msg ThinkPlayerMsgsPhase], SpikeRing)
shootSpikeRing playerPos spikeRing =
    ( concatMap (spikeAltShootMsgs playerPos) (M.keys $ _ringSpikeAltAngleOffsets spikeRing)
    , spikeRing {_ringSpikeAltAngleOffsets = M.empty}
    )

isSpikeRingActive :: SpikeRing -> Bool
isSpikeRingActive = not . M.null . _ringSpikeAltAngleOffsets
