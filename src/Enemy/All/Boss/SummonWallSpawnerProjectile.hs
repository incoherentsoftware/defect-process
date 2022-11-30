module Enemy.All.Boss.SummonWallSpawnerProjectile
    ( mkSummonWallSpawnerProjectile
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Traversable       (for)
import qualified Data.List.NonEmpty as NE

import Attack
import Collision
import Constants
import Enemy.All.Boss.AttackDescriptions
import Enemy.All.Boss.Data
import Enemy.All.Boss.WallProjectile
import Id
import Msg
import Projectile as P
import Util

spawnerOffset              = Pos2 150.0 300.0  :: Pos2
timeoutWaveCooldownNegSecs = -3.0              :: Secs

allWaveCooldownSecs = NE.fromList
    [ 1.0
    , 1.75
    , 1.5
    , 1.25
    , 1.0
    , 0.75
    , 0.5
    ] :: NE.NonEmpty Secs

pattern1OffsetYs = [20.0, -310.0, -650.0] :: [PosY]
pattern2OffsetYs = [190.0, -500.0]        :: [PosY]

patterns = NE.fromList
    [ \pos -> map (vecAdd pos . Pos2 0.0) pattern1OffsetYs
    , \pos -> map (vecAdd pos . Pos2 0.0) pattern2OffsetYs
    ] :: NE.NonEmpty (Pos2 -> [Pos2])

wallProjAudioOffset = Pos2 (-800.0) 0.0                            :: Pos2
wallSoundPath       = "event:/SFX Events/Enemy/Boss/attack-wall-c" :: FilePath

data SummonWallSpawnerProjData = SummonWallSpawnerProjData
    { _waveCooldownSecs    :: [Secs]
    , _waveCooldownTtl     :: Secs
    , _wallAttackDesc      :: AttackDescription
    , _knownInnerLeftWallX :: PosX
    }

mkSummonWallSpawnerProjData :: BossEnemyData -> SummonWallSpawnerProjData
mkSummonWallSpawnerProjData enemyData = SummonWallSpawnerProjData
    { _waveCooldownSecs    = NE.tail allWaveCooldownSecs
    , _waveCooldownTtl     = NE.head allWaveCooldownSecs
    , _wallAttackDesc      = _wallProjectile $ _attackDescs enemyData
    , _knownInnerLeftWallX = _knownInnerLeftWallX (enemyData :: BossEnemyData)
    }

mkSummonWallSpawnerProjectile :: MonadIO m => Pos2 -> BossEnemyData -> MsgId -> m (Some Projectile)
mkSummonWallSpawnerProjectile pos bossEnemyData bossEnemyMsgId = do
    msgId <- newId
    let
        dummyHbx            = dummyHitbox $ pos `vecAdd` spawnerOffset
        wallSpawnerProjData = mkSummonWallSpawnerProjData bossEnemyData

    return . Some $ (mkProjectile wallSpawnerProjData msgId dummyHbx maxSecs)
        { _think   = thinkSummonWallSpawnerProj
        , _ownerId = bossEnemyMsgId
        }

thinkSummonWallSpawnerProj :: MonadIO m => ProjectileThink SummonWallSpawnerProjData m
thinkSummonWallSpawnerProj wallSpawnerProj =
    let
        wallSpawnerProjData = P._data wallSpawnerProj
        waveCooldownTtl     = _waveCooldownTtl wallSpawnerProjData
        waveCooldownSecs    = _waveCooldownSecs wallSpawnerProjData
        wallSpawnerProjId   = P._msgId wallSpawnerProj

        audioPos = hitboxCenter (projectileHitbox wallSpawnerProj) `vecAdd` wallProjAudioOffset
        audioMsg = mkMsg $ AudioMsgPlaySoundContinuous wallSoundPath (hashId wallSpawnerProjId) audioPos
    in do
        (newWallMsgs, waveCooldownTtl', waveCooldownSecs') <- case waveCooldownSecs of
            (cooldownSec:cooldownSecs)
                | waveCooldownTtl <= 0.0 -> do
                    pattern <- randomChoice patterns
                    let
                        positions           = pattern $ hitboxCenter (projectileHitbox wallSpawnerProj)
                        wallAttackDesc      = _wallAttackDesc wallSpawnerProjData
                        knownInnerLeftWallX = _knownInnerLeftWallX (wallSpawnerProjData :: SummonWallSpawnerProjData)
                        mkWallProjs         = for positions $ \pos ->
                            mkWallProjectile pos wallAttackDesc knownInnerLeftWallX

                    return
                        ( [mkMsg $ NewUpdateProjectileMsgAddsM mkWallProjs]
                        , cooldownSec
                        , cooldownSecs
                        )

            _ -> return ([], waveCooldownTtl - timeStep, waveCooldownSecs)

        let
            update = \p -> p
                { _data = (_data p)
                    { _waveCooldownSecs = waveCooldownSecs'
                    , _waveCooldownTtl  = waveCooldownTtl'
                    }
                }

            updateWallSpawnerProjMsg = mkMsgTo (ProjectileMsgUpdate update) wallSpawnerProjId

            finishMsgs
                | null waveCooldownSecs' && waveCooldownTtl' <= timeoutWaveCooldownNegSecs =
                    [ mkMsgTo (ProjectileMsgSetTtl 0.0) wallSpawnerProjId
                    , mkMsgTo EnemyMsgFinishAttack (_ownerId wallSpawnerProj)
                    ]
                | otherwise                                                                = []

        return $ [audioMsg, updateWallSpawnerProjMsg] ++ newWallMsgs ++ finishMsgs
