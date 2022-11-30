module Enemy.All.Boss.SummonFlyingSpawnerProjectile
    ( mkSummonFlyingSpawnerProjectile
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Random.Shuffle  (shuffleM)

import Attack
import Collision
import Constants
import Enemy.All.Boss.AttackDescriptions
import Enemy.All.Boss.Data
import Enemy.All.Boss.FlyingProjectile
import Id
import Msg
import Projectile as P
import Util

spawnerOffset              = Pos2 (-827.0) 50.0 :: Pos2
initialWaveCooldownSecs    = 1.0                :: Secs
waveCooldownSecs           = 1.0                :: Secs
timeoutWaveCooldownNegSecs = -1.0               :: Secs

allWaveOffsets =
    [ Pos2 (-870.0) (-300.0)
    , Pos2 (-670.0) (-300.0)
    , Pos2 (-470.0) (-300.0)
    , Pos2 (-270.0) (-300.0)
    , Pos2 0.0 (-300.0)
    , Pos2 270.0 (-300.0)
    , Pos2 470.0 (-300.0)
    , Pos2 670.0 (-300.0)
    , Pos2 870.0 (-300.0)
    ] :: [Pos2]

data SummonFlyingSpawnerProjData = SummonFlyingSpawnerProjData
    { _waveOffsets     :: [Pos2]
    , _waveCooldownTtl :: Secs
    , _flyingAttackDesc  :: AttackDescription
    }

mkSummonFlyingSpawnerProjData :: MonadIO m => BossEnemyData -> m SummonFlyingSpawnerProjData
mkSummonFlyingSpawnerProjData enemyData = do
    waveOffsets <- liftIO $ shuffleM allWaveOffsets
    return $ SummonFlyingSpawnerProjData
        { _waveOffsets      = waveOffsets
        , _waveCooldownTtl  = initialWaveCooldownSecs
        , _flyingAttackDesc = _flyingProjectile $ _attackDescs enemyData
        }

mkSummonFlyingSpawnerProjectile :: MonadIO m => Pos2 -> BossEnemyData -> MsgId -> m (Some Projectile)
mkSummonFlyingSpawnerProjectile pos bossEnemyData bossEnemyMsgId = do
    msgId                 <- newId
    flyingSpawnerProjData <- mkSummonFlyingSpawnerProjData bossEnemyData
    let dummyHbx           = dummyHitbox $ pos `vecAdd` spawnerOffset

    return . Some $ (mkProjectile flyingSpawnerProjData msgId dummyHbx maxSecs)
        { _think   = thinkSummonFlyingSpawnerProj
        , _ownerId = bossEnemyMsgId
        }

thinkSummonFlyingSpawnerProj :: MonadIO m => ProjectileThink SummonFlyingSpawnerProjData m
thinkSummonFlyingSpawnerProj flyingSpawnerProj =
    let
        flyingSpawnerProjData = P._data flyingSpawnerProj
        waveCooldownTtl     = _waveCooldownTtl flyingSpawnerProjData
        flyingSpawnerProjId   = P._msgId flyingSpawnerProj
    in do
        (newFlyingMsgs, waveCooldownTtl', waveOffsets') <- case _waveOffsets flyingSpawnerProjData of
            (offset:offsets)
                | waveCooldownTtl <= 0.0 ->
                    let
                        flyingAttackDesc = _flyingAttackDesc flyingSpawnerProjData
                        flyingPos        = hitboxCenter $ projectileHitbox flyingSpawnerProj
                        mkFlyingProj     = mkFlyingProjectile (flyingPos `vecAdd` offset) flyingAttackDesc
                    in return
                        ( [mkMsg $ NewUpdateProjectileMsgAddM mkFlyingProj]
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

            updateFlyingSpawnerProjMsg = mkMsgTo (ProjectileMsgUpdate update) flyingSpawnerProjId

            finishMsgs
                | null waveOffsets' && waveCooldownTtl' <= timeoutWaveCooldownNegSecs =
                    [ mkMsgTo (ProjectileMsgSetTtl 0.0) flyingSpawnerProjId
                    , mkMsgTo EnemyMsgFinishAttack (_ownerId flyingSpawnerProj)
                    ]
                | otherwise                                                           = []

        return $ updateFlyingSpawnerProjMsg:newFlyingMsgs ++ finishMsgs
