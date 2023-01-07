module Player.Gun.All.ShardGun.BlinkStrike
    ( blinkStrikeMsgs
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.List.NonEmpty as NE

import Attack
import FileCache
import Msg
import Particle.All.Simple
import Player.Gun.All.ShardGun.Data
import Util
import World.ZIndex

packPath                  = \f -> PackResourceFilePath "data/player/player-guns.pack" f
blinkStrikePath           = packPath "shard-blink-strike-effect.spr" :: PackResourceFilePath
blinkStrikeHitEffectPaths = NE.fromList $ map packPath
    [ "shard-blink-strike-hit-effect-a.spr"
    , "shard-blink-strike-hit-effect-b.spr"
    , "shard-blink-strike-hit-effect-c.spr"
    ] :: NE.NonEmpty PackResourceFilePath

blinkStrikeImpactSoundPath = "event:/SFX Events/Player/Guns/shard-gun-blink-strike-impact" :: FilePath

mkBlinkStrikeAttack :: MonadIO m => Pos2 -> ShardGunData -> m Attack
mkBlinkStrikeAttack pos shardGunData = do
    dir     <- randomChoice $ LeftDir NE.:| [RightDir]
    atkDesc <- randomChoice $ _playerBlinkStrikes (_attackDescription shardGunData)
    mkAttack pos dir atkDesc

blinkStrikeMsgs :: MonadIO m => ShardGunData -> Pos2 -> MsgId -> m [Msg ThinkPlayerMsgsPhase]
blinkStrikeMsgs shardGunData projPos projId =
    let
        mkEffect    = loadSimpleParticle projPos RightDir playerAttackEffectZIndex blinkStrikePath
        mkHitEffect = do
            hitEffectPath <- randomChoice blinkStrikeHitEffectPaths
            loadSimpleParticle projPos RightDir playerAttackEffectZIndex hitEffectPath
    in do
        blinkStrikeAtk <- mkBlinkStrikeAttack projPos shardGunData
        return
            [ mkMsgEx (PlayerMsgSetPosition projPos) MsgEndOrder
            , mkMsg PlayerMsgSetPhased
            , mkMsg $ PlayerMsgSetAttack blinkStrikeAtk
            , mkMsgTo (ProjectileMsgSetTtl 0.0) projId
            , mkMsg $ ParticleMsgAddM mkEffect
            , mkMsg $ ParticleMsgAddM mkHitEffect
            , mkMsg WorldMsgLockCamera
            , mkMsg $ AudioMsgPlaySound blinkStrikeImpactSoundPath projPos
            ]
