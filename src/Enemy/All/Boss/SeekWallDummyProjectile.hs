module Enemy.All.Boss.SeekWallDummyProjectile
    ( mkSeekWallDummyProjectile
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import qualified Data.Set as S

import Collision
import Constants
import Enemy as E
import Enemy.All.Boss.Data
import Id
import Msg
import Projectile as P
import Util

projOffsetY     = -165.0       :: PosY
projWidth       = 1920.0 * 2.0 :: Float
projHeight      = 50.0         :: Float
projTimeoutSecs = 1000.1       :: Secs

data SeekWallDummyProjData = SeekWallDummyProjData
    { _timeoutTtl    :: Secs
    , _bossEnemyMgId :: MsgId
    }

mkSeekWallDummyProjectile :: MonadIO m => Pos2 -> MsgId -> m (Some Projectile)
mkSeekWallDummyProjectile (Pos2 x y) bossEnemyMsgId =
    let
        seekWallDummyProjData = SeekWallDummyProjData
            { _timeoutTtl    = projTimeoutSecs
            , _bossEnemyMgId = bossEnemyMsgId
            }

        pos = Pos2 (x - projWidth / 2.0) (y - projHeight + projOffsetY)
        hbx = rectHitbox pos projWidth projHeight
    in do
        msgId <- newId
        return . Some $ (mkProjectile seekWallDummyProjData msgId hbx maxSecs)
            { _registeredCollisions = S.fromList [ProjRegisteredSurfaceCollision]
            , _think                = thinkSeekWallDummyProj
            , _processCollisions    = processSeekWallDummyProjCollisions
            }

bossEnemyUpdateKnownInnerWallsMsg :: AllowMsgWrite p EnemyMsgPayload => PosX -> PosX -> SeekWallDummyProjData -> Msg p
bossEnemyUpdateKnownInnerWallsMsg knownInnerLeftWallX knownInnerRightWallX seekWallDummyProjData =
    mkMsgTo (EnemyMsgUpdate update) (_bossEnemyMgId seekWallDummyProjData)
    where
        update = \e -> e
            { E._data = (E._data e)
                { _knownInnerLeftWallX  = knownInnerLeftWallX
                , _knownInnerRightWallX = knownInnerRightWallX
                }
            }

thinkSeekWallDummyProj :: Monad m => ProjectileThink SeekWallDummyProjData m
thinkSeekWallDummyProj seekWallDummyProj = return $ if
    | timeoutTtl' <= 0.0 ->
        let hbx = projectileHitbox seekWallDummyProj
        in
            [ bossEnemyUpdateKnownInnerWallsMsg (hitboxLeft hbx) (hitboxRight hbx) seekWallDummyProjData
            , mkMsgTo (ProjectileMsgSetTtl 0.0) (_bossEnemyMgId seekWallDummyProjData)
            ]

    | otherwise ->
        let
            update = \proj -> proj
                { P._data = (P._data proj) {_timeoutTtl = timeoutTtl'}
                }
        in [mkMsgTo (ProjectileMsgUpdate update) seekWallDummyProjId]

    where
        seekWallDummyProjData = P._data seekWallDummyProj
        timeoutTtl'           = _timeoutTtl seekWallDummyProjData - timeStep
        seekWallDummyProjId   = P._msgId seekWallDummyProj

processSeekWallDummyProjCollisions :: ProjectileProcessCollisions SeekWallDummyProjData
processSeekWallDummyProjCollisions collisions seekWallDummyProj =
    [ bossEnemyUpdateKnownInnerWallsMsg innerLeftWallX innerRightWallX (P._data seekWallDummyProj)
    , mkMsgTo (ProjectileMsgSetTtl 0.0) (P._msgId seekWallDummyProj)
    ]
    where
        processSurfaceCollision :: ProjectileCollision -> [Hitbox] -> [Hitbox]
        processSurfaceCollision collision !hbxs = case collision of
            ProjSurfaceCollision hbx _ -> hbx:hbxs
            _                          -> hbxs

        surfaceHbxs     = foldr processSurfaceCollision [] collisions
        projHbx         = projectileHitbox seekWallDummyProj
        innerLeftWallX  = fromMaybe (hitboxLeft projHbx) (maybeMinimum $ map hitboxRight surfaceHbxs)
        innerRightWallX = fromMaybe (hitboxRight projHbx) (maybeMaximum $ map hitboxLeft surfaceHbxs)
