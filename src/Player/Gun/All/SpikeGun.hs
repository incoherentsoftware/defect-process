module Player.Gun.All.SpikeGun
    ( mkSpikeGun
    ) where

import Control.Monad.IO.Class (MonadIO)

import Configs
import Configs.All.PlayerGun.SpikeGun
import FileCache
import Id
import Msg
import Player
import Player.Gun
import Player.Gun.All.SpikeGun.Data
import Player.Gun.All.SpikeGun.SpikeBarrage
import Player.Gun.All.SpikeGun.SpikeRing
import Player.Gun.All.SpikeGun.Util
import Util
import Window.Graphics
import Window.InputState
import World.ZIndex

maxSpikes = 5 :: Int

spikeSummonSoundFilePath = "event:/SFX Events/Player/Guns/spike-summon" :: FilePath

mkSpikeGun :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m (Some Gun)
mkSpikeGun = do
    spikeGunData <- mkSpikeGunData
    return . Some $ (mkGun spikeGunData SpikeGun)
        { _think       = thinkSpikeGun
        , _update      = updateSpikeGun
        , _drawOverlay = drawSpikeGunOverlay
        }

isPartialSpikesSummon :: SpikeGunData -> Bool
isPartialSpikesSummon spikeGunData = case _summonSpr spikeGunData of
    Nothing  -> False
    Just spr -> spr `elem` partialSpikesSummonSprs
    where
        sprs                    = _sprites (spikeGunData :: SpikeGunData)
        partialSpikesSummonSprs =
            [ _spikesSummon1 sprs
            , _spikesSummon2 sprs
            , _spikesSummon3 sprs
            , _spikesSummon4 sprs
            ]

progressSpikeGunSummonSprite :: Gun SpikeGunData -> Gun SpikeGunData
progressSpikeGunSummonSprite spikeGun = spikeGun
    { _data = case _summonSpr spikeGunData of
        Nothing                    -> spikeGunData {_summonSpr = Just spikesSummon1}
        Just spr
            | spr == spikesSummon1 -> spikeGunData {_summonSpr = Just spikesSummon2}
            | spr == spikesSummon2 -> spikeGunData {_summonSpr = Just spikesSummon3}
            | spr == spikesSummon3 -> spikeGunData {_summonSpr = Just spikesSummon4}
            | spr == spikesSummon4 -> spikeGunData {_summonSpr = Just $ _spikesSummon5 sprs}
            | otherwise            -> spikeGunData
    }
    where
        spikeGunData  = _data spikeGun
        sprs          = _sprites (spikeGunData :: SpikeGunData)
        spikesSummon1 = _spikesSummon1 sprs
        spikesSummon2 = _spikesSummon2 sprs
        spikesSummon3 = _spikesSummon3 sprs
        spikesSummon4 = _spikesSummon4 sprs

thinkSpikeGun :: (ConfigsRead m, InputRead m, MonadIO m, MsgsRead ThinkPlayerMsgsPhase m) => GunThink SpikeGunData m
thinkSpikeGun gunStatus player spikeGun = do
    inputState <- readInputState
    let
        spikeGunData = _data spikeGun
        shootHold    = ShootAlias `aliasHold` inputState && not (_isSummonBlocked spikeGunData)
        notShootHold = ShootAlias `aliasNotHold` inputState
        active       = isGunStatusActive gunStatus

        spikeRing       = _ring spikeGunData
        isPartialSummon = isPartialSpikesSummon spikeGunData
        cfg             = _config (spikeGunData :: SpikeGunData)
        meterCost       = _summonMeterCost cfg
        numSpikes       = _numSpikes spikeGunData
        canSpendMeter   = canSpendPlayerMeter meterCost player
        playerPos       = _pos (player :: Player)

        audioMsg =
            let hashedId = hashId $ _summonSoundId spikeGunData
            in mkMsg $ AudioMsgPlaySoundContinuous spikeSummonSoundFilePath hashedId playerPos

        summonSpikesMsgs =
            [ mkMsg $ PlayerMsgUpdateGun progressSpikeGunSummonSprite
            , mkMsg $ PlayerMsgSpendMeter meterCost
            ]

    fromInputMsgs <- if
        | isSpikeRingActive spikeRing && shootHold && active ->
            let
                (shootMsgs, spikeRing') = shootSpikeRing playerPos spikeRing
                updateShootRing         = \g -> g
                    { _data = (_data g)
                        { _ring            = spikeRing'
                        , _isSummonBlocked = True
                        }
                    }
            in return $ mkMsg (PlayerMsgUpdateGun updateShootRing):shootMsgs

        | notShootHold && numSpikes <= 0 && not isPartialSummon ->
            let updateSummon = \g -> g {_data = (_data g) {_summonSpr = Nothing}}
            in return [mkMsg $ PlayerMsgUpdateGun updateSummon]

        | notShootHold && active && numSpikes > 0 ->
            let
                updateRelease = \g -> g
                    { _data = (_data g)
                        { _numSpikes = 0
                        , _summonSpr = Nothing
                        }
                    }

                -- refund meter for any incomplete spike
                meterRefundMsgs = case _summonSpr spikeGunData of
                    Just spr
                        | not (spriteIsLastFrameIndex spr) -> [mkMsg $ PlayerMsgGainMeter NullId meterCost]
                    _                                      -> []
            in do
                releaseMsgs <- if
                    | DownAlias `aliasHold` inputState -> do
                        (ringMsgs, spikeRing') <- activateSpikeRing numSpikes player spikeRing
                        let updateActivateRing  = \g -> g {_data = (_data g) {_ring = spikeRing'}}
                        return $ mkMsg (PlayerMsgUpdateGun updateActivateRing):ringMsgs

                    | otherwise -> return
                        [ mkMsg $ NewThinkProjectileMsgAddM (mkSpikeBarrage spikeGunData player)
                        , mkMsg $ ParticleMsgAddM (mkSpikeGunDematerializeParticle spikeGunData player)
                        ]

                return $ mkMsg (PlayerMsgUpdateGun updateRelease):releaseMsgs ++ meterRefundMsgs

        | shootHold && active && numSpikes < maxSpikes -> return $ case _summonSpr spikeGunData of
            Nothing
                | canSpendMeter  -> audioMsg:summonSpikesMsgs
                | numSpikes == 0 -> [mkMsg $ UiMsgInsufficientMeter meterCost False]
                | otherwise      -> []

            Just summonSpr
                | spriteFinished summonSpr && isPartialSummon && canSpendMeter -> audioMsg:summonSpikesMsgs
                | not (spriteFinished summonSpr)                               -> [audioMsg]
                | otherwise                                                    -> []

        | otherwise -> return []

    (fromInputMsgs ++) <$> thinkSpikeRing player spikeRing

numSpikesFromSummonSprite :: Maybe Sprite -> SpikeGunSprites -> Int
numSpikesFromSummonSprite summonSpr sprs = case summonSpr of
    Just spr
        | spr == _spikesSummon1 sprs -> if spriteIsLastFrameIndex spr then 1 else 0
        | spr == _spikesSummon2 sprs -> if spriteIsLastFrameIndex spr then 2 else 1
        | spr == _spikesSummon3 sprs -> if spriteIsLastFrameIndex spr then 3 else 2
        | spr == _spikesSummon4 sprs -> if spriteIsLastFrameIndex spr then 4 else 3
        | spr == _spikesSummon5 sprs -> if spriteIsLastFrameIndex spr then 5 else 4
    _                                -> 0

updateSpikeGun :: (InputRead m, MonadIO m) => GunUpdate SpikeGunData m
updateSpikeGun spikeGun =
    let
        spikeGunData      = _data spikeGun
        prevNumSpikes     = _numSpikes spikeGunData
        prevSummonSoundId = _summonSoundId spikeGunData
        summonSpr         = updateSprite <$> _summonSpr spikeGunData
        sprs              = _sprites (spikeGunData :: SpikeGunData)
        numSpikes         = numSpikesFromSummonSprite summonSpr sprs
    in do
        inputState    <- readInputState
        summonSoundId <- if
            | numSpikes /= prevNumSpikes && numSpikes < maxSpikes -> newId
            | otherwise                                           -> return prevSummonSoundId

        return $ spikeGun
            { _data = spikeGunData
                { _numSpikes       = numSpikes
                , _ring            = updateSpikeRing $ _ring spikeGunData
                , _isSummonBlocked = _isSummonBlocked spikeGunData && ShootAlias `aliasHold` inputState
                , _summonSoundId   = summonSoundId
                , _summonSpr       = summonSpr
                }
            }

drawSpikeGunOverlay :: (GraphicsReadWrite m, MonadIO m) => GunDrawOverlay SpikeGunData m
drawSpikeGunOverlay status player spikeGun =
    let
        spikeGunData = _data spikeGun
        cfg          = _config (spikeGunData :: SpikeGunData)
        summonOffset = vecFlip (_summonOffset cfg) RightDir
    in case status of
        GunDrawOverlayForeground
            | Just summonSpr <- _summonSpr spikeGunData -> do
                playerPos   <- (_pos (player :: Player) `vecAdd`) <$> playerLerpOffset player
                let summonPos = playerPos `vecAdd` summonOffset
                drawSprite summonPos RightDir playerGunOverlayZIndex summonSpr

        _ -> drawSpikeRing player (_ring spikeGunData)
