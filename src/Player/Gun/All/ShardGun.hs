module Player.Gun.All.ShardGun
    ( mkShardGun
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Random   (MonadRandom)
import Data.Maybe             (fromMaybe)
import Data.Ord               (comparing)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M

import Configs
import Configs.All.PlayerGun
import Configs.All.PlayerGun.ShardGun
import FileCache
import Msg
import Player
import Player.BufferedInputState
import Player.Gun
import Player.Gun.All.ShardGun.BlinkStrike
import Player.Gun.All.ShardGun.Data
import Player.Gun.All.ShardGun.Shard
import Player.Gun.All.ShardGun.Shot
import Player.Gun.FireDrawAngle
import Player.Gun.FireDrawData
import Player.Gun.FireDrawState.LegsState
import Player.Gun.MuzzleFlash
import Util
import Window.Graphics
import Window.InputState
import qualified Player.Gun.All.Revolver.Util as Revolver

shardGunSoundPath = "event:/SFX Events/Player/Guns/shard-gun" :: FilePath

shardGunMuzzleFlashSprFileNames = NE.fromList
    [ "shard-gun-muzzle-flash-a.spr"
    , "shard-gun-muzzle-flash-b.spr"
    , "shard-gun-muzzle-flash-c.spr"
    ] :: NE.NonEmpty FileName

loadShardGunSprite :: (FileCache m, GraphicsRead m, MonadIO m) => FilePath -> m Sprite
loadShardGunSprite fileName = loadPackSprite $ PackResourceFilePath "data/player/player-guns.pack" fileName

loadLegsSprites :: (FileCache m, GraphicsRead m, MonadIO m) => m LegsSprites
loadLegsSprites =
    LegsSprites <$>
    loadShardGunSprite "shard-gun-legs.spr" <*>
    loadShardGunSprite "shard-gun-air-legs.spr" <*>
    loadPackSpr "legs-walk.spr" <*>
    loadPackSpr "legs-back-walk.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/player/player-guns.pack" f

loadMuzzleFlash :: (FileCache m, GraphicsRead m, MonadIO m) => ShardGunConfig -> m MuzzleFlash
loadMuzzleFlash cfg = do
    muzzleFlashSprs      <- traverse loadShardGunSprite shardGunMuzzleFlashSprFileNames
    let muzzleFlashOffset = _muzzleFlashOffset (cfg :: ShardGunConfig)
    return $ mkMuzzleFlash LeadArmMuzzleFlash muzzleFlashOffset muzzleFlashSprs

mkFireDrawData :: forall m. (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m GunFireDrawData
mkFireDrawData =
    let
        loadFireDrawAngleMap :: (a -> m b) -> [(GunFireDrawAngle, a)] -> m (M.Map GunFireDrawAngle b)
        loadFireDrawAngleMap f angleValues = M.fromList <$> sequenceA
            [(angle,) <$> f value | (angle, value) <- angleValues]
    in do
        armOrders <- loadFireDrawAngleMap pure
            [ (FireDraw90Degrees, DrawRearArmInFront)
            , (FireDraw45Degrees, DrawRearArmInFront)
            , (FireDraw0Degrees, DrawRearArmInFront)
            , (FireDrawNeg45Degrees, DrawRearArmInFront)
            , (FireDrawNeg90Degrees, DrawRearArmInFront)
            , (BackFireDraw90Degrees, DrawLeadArmInFront)
            , (BackFireDraw45Degrees, DrawLeadArmInFront)
            , (BackFireDraw0Degrees, DrawLeadArmInFront)
            , (BackFireDrawNeg45Degrees, DrawLeadArmInFront)
            , (BackFireDrawNeg90Degrees, DrawLeadArmInFront)
            ]

        headSprs <- loadFireDrawAngleMap loadShardGunSprite
            [ (FireDraw90Degrees, "shard-gun-fire-90-head.spr")
            , (FireDraw45Degrees, "shard-gun-fire-45-head.spr")
            , (FireDraw0Degrees, "shard-gun-fire-0-head.spr")
            , (FireDrawNeg45Degrees, "shard-gun-fire-neg-45-head.spr")
            , (FireDrawNeg90Degrees, "shard-gun-fire-neg-90-head.spr")
            , (BackFireDraw90Degrees, "shard-gun-back-fire-90-head.spr")
            , (BackFireDraw45Degrees, "shard-gun-back-fire-45-head.spr")
            , (BackFireDraw0Degrees, "shard-gun-back-fire-0-head.spr")
            , (BackFireDrawNeg45Degrees, "shard-gun-back-fire-neg-45-head.spr")
            , (BackFireDrawNeg90Degrees, "shard-gun-back-fire-neg-90-head.spr")
            ]

        torsoSprs <- loadFireDrawAngleMap loadShardGunSprite
            [ (FireDraw90Degrees, "shard-gun-fire-90-torso.spr")
            , (FireDraw45Degrees, "shard-gun-fire-45-torso.spr")
            , (FireDraw0Degrees, "shard-gun-fire-0-torso.spr")
            , (FireDrawNeg45Degrees, "shard-gun-fire-neg-45-torso.spr")
            , (FireDrawNeg90Degrees, "shard-gun-fire-neg-90-torso.spr")
            , (BackFireDraw90Degrees, "shard-gun-back-fire-90-torso.spr")
            , (BackFireDraw45Degrees, "shard-gun-back-fire-45-torso.spr")
            , (BackFireDraw0Degrees, "shard-gun-back-fire-0-torso.spr")
            , (BackFireDrawNeg45Degrees, "shard-gun-back-fire-neg-45-torso.spr")
            , (BackFireDrawNeg90Degrees, "shard-gun-back-fire-neg-90-torso.spr")
            ]

        leadArmSprs <- loadFireDrawAngleMap loadShardGunSprite
            [ (FireDraw90Degrees, "shard-gun-fire-90-lead-arm.spr")
            , (FireDraw45Degrees, "shard-gun-fire-45-lead-arm.spr")
            , (FireDraw0Degrees, "shard-gun-fire-0-lead-arm.spr")
            , (FireDrawNeg45Degrees, "shard-gun-fire-neg-45-lead-arm.spr")
            , (FireDrawNeg90Degrees, "shard-gun-fire-neg-90-lead-arm.spr")
            , (BackFireDraw90Degrees, "shard-gun-back-fire-90-lead-arm.spr")
            , (BackFireDraw45Degrees, "shard-gun-back-fire-45-lead-arm.spr")
            , (BackFireDraw0Degrees, "shard-gun-back-fire-0-lead-arm.spr")
            , (BackFireDrawNeg45Degrees, "shard-gun-back-fire-neg-45-lead-arm.spr")
            , (BackFireDrawNeg90Degrees, "shard-gun-back-fire-neg-90-lead-arm.spr")
            ]

        rearArmSprs <- loadFireDrawAngleMap loadShardGunSprite
            [ (FireDraw90Degrees, "shard-gun-fire-90-rear-arm.spr")
            , (FireDraw45Degrees, "shard-gun-fire-45-rear-arm.spr")
            , (FireDraw0Degrees, "shard-gun-fire-0-rear-arm.spr")
            , (FireDrawNeg45Degrees, "shard-gun-fire-neg-45-rear-arm.spr")
            , (FireDrawNeg90Degrees, "shard-gun-fire-neg-90-rear-arm.spr")
            , (BackFireDraw90Degrees, "shard-gun-back-fire-90-rear-arm.spr")
            , (BackFireDraw45Degrees, "shard-gun-back-fire-45-rear-arm.spr")
            , (BackFireDraw0Degrees, "shard-gun-back-fire-0-rear-arm.spr")
            , (BackFireDrawNeg45Degrees, "shard-gun-back-fire-neg-45-rear-arm.spr")
            , (BackFireDrawNeg90Degrees, "shard-gun-back-fire-neg-90-rear-arm.spr")
            ]

        legsSprs     <- loadLegsSprites
        playerGunCfg <- _playerGun <$> readConfigs
        muzzleFlash  <- loadMuzzleFlash $ _shardGun playerGunCfg

        return $ GunFireDrawData
            { _fireDrawAngle          = FireDraw0Degrees
            , _armOrders              = armOrders
            , _headSprites            = headSprs
            , _torsoSprites           = torsoSprs
            , _leadArmSprites         = leadArmSprs
            , _rearArmSprites         = rearArmSprs
            , _legsSprites            = Just legsSprs
            , _muzzleFlash            = Just muzzleFlash
            , _calculatePlayerAimBody = Revolver.calculatePlayerAimBody $ _revolver playerGunCfg
            , _uncancelableSecs       = defaultGunFireDrawStateUncancelableSecs
            }

mkShardGun :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m (Some Gun)
mkShardGun = do
    shardGunData <- mkShardGunData
    fireDrawData <- mkFireDrawData

    return . Some $ (mkGun shardGunData ShardGun)
        { _fireDrawData = Just fireDrawData
        , _think        = thinkShardGun
        , _update       = updateShardGun
        }

getShardsPosMsgIds :: (MonadRandom m, MsgsRead ThinkPlayerMsgsPhase m) => ShardGunData -> m [(Pos2, MsgId)]
getShardsPosMsgIds shardGunData = L.sortBy (comparing snd) . L.foldl' getShardMsgIds [] <$> readMsgs
    where
        getShardMsgIds :: [(Pos2, MsgId)] -> InfoMsgPayload -> [(Pos2, MsgId)]
        getShardMsgIds posMsgIds d = case d of
            InfoMsgProjectilePos projPos projOwnerId projId
                | projOwnerId == shardGunId -> (projPos, projId):posMsgIds
            _                               -> posMsgIds
            where shardGunId = _gunMsgId (shardGunData :: ShardGunData)

getShardsMsgIds :: (MonadRandom m, MsgsRead ThinkPlayerMsgsPhase m) => ShardGunData -> m [MsgId]
getShardsMsgIds shardGunData = map snd <$> getShardsPosMsgIds shardGunData

-- defers thinking for 1 frame, used to allow blink strikes to collide w/ enemies before immediately initiating
-- the next blink strike
deferThinkShardGun :: [(Pos2, MsgId)] -> Gun ShardGunData -> Gun ShardGunData
deferThinkShardGun blinkStrikePosIds shardGun = shardGun
    { _data  = (_data shardGun) {_blinkStrikePosMsgIds = blinkStrikePosIds}
    , _think = \_ _ _ -> return
        [ mkMsg $ PlayerMsgUpdateGun updateThink
        , mkMsg WorldMsgLockCamera
        ]
    }
    where updateThink = \g -> g {_think = thinkShardGun}

thinkShardGun :: (InputRead m, MonadIO m, MonadRandom m, MsgsRead ThinkPlayerMsgsPhase m) => GunThink ShardGunData m
thinkShardGun gunStatus player shardGun =
    let
        shardGunData           = _data shardGun
        active                 = isGunStatusActive gunStatus
        canShoot               = gunStatus == ActiveStatus Shootable
        playerPos              = _pos (player :: Player)
        cfg                    = _config (shardGunData :: ShardGunData)
        shotMeterCost          = _shotMeterCost (cfg :: ShardGunConfig)
        cantSpendMeterShot     = not $ canSpendPlayerMeter shotMeterCost player
        clearBufferedInputsMsg = mkMsg $ PlayerMsgClearInputBuffer allShootBufferedInputs
    in do
        inputState <- readInputState

        let
            shootDownJustPressed = ShootAlias `aliasPressed` inputState && DownAlias `aliasHold` inputState
            shootDownPressed     = shootDownJustPressed || ShootDownInput `inPlayerInputBuffer` player
            shootUpJustPressed   = ShootAlias `aliasPressed` inputState && UpAlias `aliasHold` inputState
            shootUpPressed       = shootUpJustPressed || ShootUpInput `inPlayerInputBuffer` player
            shootJustPressed     = ShootAlias `aliasPressed` inputState
            shootPressed         = shootJustPressed || ShootInput `inPlayerInputBuffer` player

        case _blinkStrikePosMsgIds shardGunData of
            -- blink striking
            ((projPos, projId):projPosIds) ->
                let
                    updateThink    = \g -> deferThinkShardGun projPosIds g
                    updateGunMsg   = mkMsg $ PlayerMsgUpdateGun updateThink
                    playerStartPos = fromMaybe playerPos (_blinkStrikePlayerStartPos shardGunData)
                in (updateGunMsg:) <$> blinkStrikeMsgs shardGunData projPos projId playerStartPos

            []
                -- explode shards
                | shootDownPressed && active -> do
                    shardsMsgs <- map (mkMsgTo $ ProjectileMsgUpdate (setShardExplode playerPos)) <$>
                        getShardsMsgIds shardGunData
                    return $ clearBufferedInputsMsg:shardsMsgs

                -- initiate blink strike on shards
                | canShoot && shootUpPressed -> getShardsPosMsgIds shardGunData >>= \case
                    []                                -> return []
                    ((projPos, projId):projPosMsgIds) ->
                        let
                            updateThink  = \g -> deferThinkShardGun projPosMsgIds g
                            updateGunMsg = mkMsg $ PlayerMsgUpdateGun updateThink
                        in do
                            blinkMsgs <- blinkStrikeMsgs shardGunData projPos projId playerPos
                            return $ clearBufferedInputsMsg:updateGunMsg:blinkMsgs

                -- fire shards
                | canShoot && shootPressed -> return $ if
                    | cantSpendMeterShot -> [mkMsg $ UiMsgInsufficientMeter shotMeterCost shootJustPressed]
                    | otherwise          ->
                        let
                            updateFire = \g -> g
                                { _data = (_data g) {_aimAngle = playerAimAngle player} :: ShardGunData
                                }
                            shotLine   = mkShotLine (_data shardGun) player
                        in
                            [ mkMsg $ PlayerMsgUpdateGun updateFire
                            , mkMsg $ PlayerMsgFiredGun
                            , mkMsg $ NewThinkProjectileMsgAddM shotLine
                            , mkMsg $ AudioMsgPlaySound shardGunSoundPath playerPos
                            , mkMsg $ PlayerMsgSpendMeter shotMeterCost
                            ]

                | otherwise -> return []

updateShardGun :: ConfigsRead m => GunUpdate ShardGunData m
updateShardGun shardGun = do
    playerGunCfg <- _playerGun <$> readConfigs

    let
        fireDrawData =
            (\fdd -> fdd {_calculatePlayerAimBody = Revolver.calculatePlayerAimBody (_revolver playerGunCfg)}) <$>
            _fireDrawData shardGun

    return $ shardGun
        { _data         = (_data shardGun :: ShardGunData) {_config = _shardGun playerGunCfg}
        , _fireDrawData = fireDrawData
        }
