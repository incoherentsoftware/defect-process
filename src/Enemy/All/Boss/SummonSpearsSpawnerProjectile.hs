module Enemy.All.Boss.SummonSpearsSpawnerProjectile
    ( mkSummonSpearsSpawnerProjectile
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Random.Shuffle  (shuffleM)

import Attack
import Collision
import Constants
import Enemy.All.Boss.AttackDescriptions
import Enemy.All.Boss.Data
import Enemy.All.Boss.SpearProjectile
import Id
import Msg
import Projectile as P
import Util

spawnerOffset              = Pos2 (-827.0) 50.0 :: Pos2
initialWaveCooldownSecs    = 1.25               :: Secs
waveCooldownSecs           = 0.75               :: Secs
timeoutWaveCooldownNegSecs = -1.75              :: Secs

allWaveOffsets =
    [ Pos2 950.0 (-220.0)
    , Pos2 950.0 (-120.0)
    , Pos2 950.0 20.0
    , Pos2 950.0 120.0
    , Pos2 950.0 120.0
    , Pos2 950.0 220.0
    , Pos2 950.0 220.0
    , Pos2 950.0 320.0
    , Pos2 950.0 320.0
    , Pos2 950.0 320.0
    , Pos2 950.0 320.0
    , Pos2 950.0 320.0
    , Pos2 950.0 320.0
    ] :: [Pos2]

data SummonSpearsSpawnerProjData = SummonSpearsSpawnerProjData
    { _waveOffsets     :: [Pos2]
    , _waveCooldownTtl :: Secs
    , _spearAttackDesc :: AttackDescription
    }

mkSummonSpearsSpawnerProjData :: MonadIO m => BossEnemyData -> m SummonSpearsSpawnerProjData
mkSummonSpearsSpawnerProjData enemyData = do
    waveOffsets <- liftIO $ shuffleM allWaveOffsets
    return $ SummonSpearsSpawnerProjData
        { _waveOffsets     = waveOffsets
        , _waveCooldownTtl = initialWaveCooldownSecs
        , _spearAttackDesc = _spearProjectile $ _attackDescs enemyData
        }

mkSummonSpearsSpawnerProjectile :: MonadIO m => Pos2 -> BossEnemyData -> MsgId -> m (Some Projectile)
mkSummonSpearsSpawnerProjectile pos bossEnemyData bossEnemyMsgId = do
    msgId                 <- newId
    spearsSpawnerProjData <- mkSummonSpearsSpawnerProjData bossEnemyData
    let dummyHbx           = dummyHitbox $ pos `vecAdd` spawnerOffset

    return . Some $ (mkProjectile spearsSpawnerProjData msgId dummyHbx maxSecs)
        { _think   = thinkSummonSpearsSpawnerProj
        , _ownerId = bossEnemyMsgId
        }

thinkSummonSpearsSpawnerProj :: MonadIO m      => ProjectileThink SummonSpearsSpawnerProjData m
thinkSummonSpearsSpawnerProj spearsSpawnerProj  =
    let
        spearsSpawnerProjData = P._data spearsSpawnerProj
        waveCooldownTtl       = _waveCooldownTtl spearsSpawnerProjData
        spearsSpawnerProjId   = P._msgId spearsSpawnerProj
    in do
        (newSpearsMsgs, waveCooldownTtl', waveOffsets') <- case _waveOffsets spearsSpawnerProjData of
            (offset:offsets)
                | waveCooldownTtl <= 0.0 ->
                    let
                        spearsAttackDesc = _spearAttackDesc spearsSpawnerProjData
                        spearsPos        = hitboxCenter $ projectileHitbox spearsSpawnerProj
                        mkSpearProj      = mkSpearProjectile (spearsPos `vecAdd` offset) spearsAttackDesc
                    in return
                        ( [mkMsg $ NewUpdateProjectileMsgAddM mkSpearProj]
                        , waveCooldownSecs
                        , offsets
                        )

            waveOffsets -> return ([], waveCooldownTtl - timeStep, waveOffsets)

        let
            update = \p -> p
                { _data = (_data p)
                    { _waveOffsets     = waveOffsets'
                    , _waveCooldownTtl = waveCooldownTtl'
                    }
                }

            updateSpearsSpawnerProjMsg = mkMsgTo (ProjectileMsgUpdate update) spearsSpawnerProjId

            finishMsgs
                | null waveOffsets' && waveCooldownTtl' <= timeoutWaveCooldownNegSecs =
                    [ mkMsgTo (ProjectileMsgSetTtl 0.0) spearsSpawnerProjId
                    , mkMsgTo EnemyMsgFinishAttack (_ownerId spearsSpawnerProj)
                    ]
                | otherwise                                                           = []

        return $ updateSpearsSpawnerProjMsg:newSpearsMsgs ++ finishMsgs
