module Player.Gun.All.ShardGun.BlinkStrike
    ( blinkStrikeMsgs
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.List.NonEmpty as NE

import Attack
import FileCache
import Msg
import Particle.All.Simple
import Player.Gun
import Player.Gun.All.ShardGun.Data
import Util
import World.ZIndex

packPath = \f -> PackResourceFilePath "data/player/player-guns.pack" f

blinkStrikePaths = NE.fromList $ map packPath
    [ "shard-blink-strike-effect-a.spr"
    , "shard-blink-strike-effect-b.spr"
    , "shard-blink-strike-effect-c.spr"
    ] :: NE.NonEmpty PackResourceFilePath

blinkStrikeHitEffectPaths = NE.fromList $ map packPath
    [ "shard-blink-strike-hit-effect-a.spr"
    , "shard-blink-strike-hit-effect-b.spr"
    , "shard-blink-strike-hit-effect-c.spr"
    ] :: NE.NonEmpty PackResourceFilePath

blinkStrikeImpactSoundPath = "event:/SFX Events/Player/Guns/shard-gun-blink-strike-impact" :: FilePath

mkBlinkStrikeAttack :: MonadIO m => ShardGunData -> Pos2 -> Pos2 -> m Attack
mkBlinkStrikeAttack shardGunData pos@(Pos2 x _) (Pos2 playerX _) =
    let
        dir
            | x < playerX = LeftDir
            | otherwise   = RightDir
    in do
        atkDesc <- randomChoice $ _playerBlinkStrikes (_attackDescription shardGunData)
        mkAttack pos dir atkDesc

blinkStrikeMsgs :: MonadIO m => ShardGunData -> Pos2 -> MsgId -> Pos2 -> m [Msg ThinkPlayerMsgsPhase]
blinkStrikeMsgs shardGunData projPos projId playerPos =
    let
        updatePlayerStartPos = \g -> g {_data = (_data g) {_blinkStrikePlayerStartPos = Just playerPos}}

        mkEffect    = do
            blinkStrikePath <- randomChoice blinkStrikePaths
            loadSimpleParticle projPos RightDir playerAttackEffectZIndex blinkStrikePath
        mkHitEffect = do
            hitEffectPath <- randomChoice blinkStrikeHitEffectPaths
            loadSimpleParticle projPos RightDir playerAttackEffectZIndex hitEffectPath
    in do
        blinkStrikeAtk <- mkBlinkStrikeAttack shardGunData projPos playerPos
        return
            [ mkMsg $ PlayerMsgUpdateGun updatePlayerStartPos
            , mkMsgEx (PlayerMsgSetPosition projPos) MsgEndOrder
            , mkMsg PlayerMsgSetPhased
            , mkMsg $ PlayerMsgSetAttack blinkStrikeAtk
            , mkMsgTo (ProjectileMsgSetTtl 0.0) projId
            , mkMsg $ ParticleMsgAddM mkEffect
            , mkMsg $ ParticleMsgAddM mkHitEffect
            , mkMsg WorldMsgLockCamera
            , mkMsg $ AudioMsgPlaySound blinkStrikeImpactSoundPath projPos
            ]
