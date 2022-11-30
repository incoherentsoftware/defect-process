module Player.MovementSkill.All.GrappleSkill.EnemyPullDummyProjectile
    ( mkEnemyPullDummyProjectile
    ) where

import Control.Monad.IO.Class (MonadIO)

import Collision
import Configs.All.PlayerSkill.Grapple
import Id
import Msg
import Player.MovementSkill.All.GrappleSkill.Util
import Projectile as P
import Util

data DummyProjData = DummyProjData
    { _prevEnemyPos :: Pos2
    , _targetPos    :: Pos2
    , _pullVel      :: Vel2
    , _enemyId      :: MsgId
    , _config       :: GrappleConfig
    }

mkEnemyPullDummyProjectile :: MonadIO m => Pos2 -> Vel2 -> MsgId -> GrappleConfig -> m (Some Projectile)
mkEnemyPullDummyProjectile targetPos pullVel enemyId cfg =
    let
        dummyProjData = DummyProjData
            { _prevEnemyPos = zeroPos2
            , _targetPos    = targetPos
            , _pullVel      = pullVel
            , _enemyId      = enemyId
            , _config       = cfg
            }
        dummyHbx      = DummyHitbox targetPos
    in do
        msgId <- newId
        return . Some $ (mkProjectile dummyProjData msgId dummyHbx (_enemyPullDummyProjSecs cfg))
            { _think = thinkEnemyPullDummyProj
            }

thinkEnemyPullDummyProj :: MsgsRead ThinkProjectileMsgsPhase m => ProjectileThink DummyProjData m
thinkEnemyPullDummyProj dummyProj = think <$> readMsgs
    where
        think :: [InfoMsgPayload] -> [Msg ThinkProjectileMsgsPhase]
        think msgDatas
            | enemyWillOvershoot || enemyStuck =
                [ mkMsgTo (ProjectileMsgSetTtl 0.0) dummyProjId
                , mkMsgToEx (EnemyMsgSetVelocity $ _enemyAfterPullVel cfg) enemyId MsgAfterNormalOrder
                , mkMsg PlayerMsgCancelMovementSkill
                , dummyProjUpdateMsg
                ]
            | otherwise                        =
                [ mkMsgToEx (EnemyMsgSetHangtime $ _enemyPullHangtimeSecs cfg) enemyId MsgEndOrder
                , mkMsgToEx (EnemyMsgSetVelocity pullVel) enemyId MsgAfterNormalOrder
                , dummyProjUpdateMsg
                ]
            where
                dummyProjData = _data dummyProj
                cfg           = _config (dummyProjData :: DummyProjData)
                prevEnemyPos  = _prevEnemyPos dummyProjData
                enemyId       = _enemyId dummyProjData

                processEnemyPos :: [InfoMsgPayload] -> Pos2
                processEnemyPos []     = prevEnemyPos
                processEnemyPos (d:ds) = case d of
                    InfoMsgEnemyPos enHbx enId
                        | enId == enemyId -> hitboxBotCenter enHbx
                    _                     -> processEnemyPos ds

                targetPos   = _targetPos dummyProjData
                pullVel     = _pullVel dummyProjData
                dummyProjId = P._msgId dummyProj

                enemyPos           = processEnemyPos msgDatas
                enemyWillOvershoot = calcWillOvershoot enemyPos pullVel targetPos
                enemyStuck         = vecDistSq enemyPos prevEnemyPos <= _enemyPullMinBreakDist cfg ** 2
                updateProj         = \p -> p
                    { _data = (_data p) {_prevEnemyPos = enemyPos}
                    }
                dummyProjUpdateMsg = mkMsgTo (ProjectileMsgUpdate updateProj) dummyProjId
