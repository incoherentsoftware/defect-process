module Player.Weapon.All.Scythe.RecallSlashProjectile
    ( mkRecallSlashProjectile
    ) where

import Control.Monad          (guard)
import Control.Monad.IO.Class (MonadIO)
import Data.Functor           ((<&>))
import Data.Maybe             (fromMaybe, listToMaybe)

import Attack.Description
import Attack.Projectile
import Collision
import Constants
import Id
import InfoMsg.Util
import Msg
import Player.Weapon.All.Scythe.Data
import Projectile as P
import Util

playerOffsetY    = -50.0 :: Float
slashRadius      = 300.0 :: Float
initialWaitSecs  = 0.283 :: Secs
waitIntervalSecs = 0.1   :: Secs

data RecallSlashProjectileData = RecallSlashProjectileData
    { _delayedAttackDatas         :: [(Secs, Pos2)]
    , _recallSlashAtkDescriptions :: [AttackDescription]
    , _targetPlayerPos            :: Pos2
    }

calculateDelayedAttackDatas :: Pos2 -> Pos2 -> [(Secs, Pos2)]
calculateDelayedAttackDatas floatingAtkPos (Pos2 playerX playerY) = zip waitSecs positions
    where
        playerPos = Pos2 playerX (playerY + playerOffsetY)
        dirVec    = toVec2 $ vecNormalize (playerPos `vecSub` floatingAtkPos)
        dist      = vecDist floatingAtkPos playerPos
        num       = ceiling (dist / slashRadius) :: Int

        offsets
            | dist <= slashRadius = [toPos2 $ dirVec `vecMul` (dist / 2.0)]
            | otherwise           =
                [ toPos2 $ dirVec `vecMul` magnitude
                | i <- [0..num - 1]
                , let interval  = dist / fromIntegral num
                , let magnitude = fromIntegral i * interval + interval / 2.0
                ]

        waitSecs  = initialWaitSecs:[initialWaitSecs + i * waitIntervalSecs | i <- [1..]]
        positions = map (floatingAtkPos `vecAdd`) offsets

mkRecallSlashProjectile :: MonadIO m => Pos2 -> Pos2 -> ScytheAttackDescriptions -> m (Some Projectile)
mkRecallSlashProjectile floatingAtkPos playerPos scytheAtkDescs =
    let
        recallSlashAtkDescs = cycle
            [ _recallSlash1 scytheAtkDescs
            , _recallSlash2 scytheAtkDescs
            , _recallSlash3 scytheAtkDescs
            ]

        recallSlashProjData = RecallSlashProjectileData
            { _delayedAttackDatas         = calculateDelayedAttackDatas floatingAtkPos playerPos
            , _recallSlashAtkDescriptions = recallSlashAtkDescs
            , _targetPlayerPos            = playerPos
            }

        dummyHbx = dummyHitbox floatingAtkPos
    in do
        msgId <- newId
        return . Some $ (mkProjectile recallSlashProjData msgId dummyHbx maxSecs)
            { _think = thinkRecallSlashProjectile
            }

mkDelayedAttackMsgs :: RecallSlashProjectileData -> [Msg ThinkProjectileMsgsPhase]
mkDelayedAttackMsgs recallSlashProjData = fromMaybe [] $ do
    (ttl, pos) <- listToMaybe $ _delayedAttackDatas recallSlashProjData
    guard $ ttl - timeStep <= 0.0
    atkDesc    <- listToMaybe $ _recallSlashAtkDescriptions recallSlashProjData
    return [mkMsg $ NewUpdateProjectileMsgAddM (mkPlayerAttackProjectile pos RightDir atkDesc)]

readPlayerPos :: MsgsRead ThinkProjectileMsgsPhase m => m (Maybe Pos2)
readPlayerPos = processMsgs <$> readMsgs
    where
        processMsgs :: [InfoMsgPayload] -> Maybe Pos2
        processMsgs []     = Nothing
        processMsgs (p:ps) = case p of
            InfoMsgPlayer playerInfo -> Just $ playerInfoPos playerInfo
            _                        -> processMsgs ps

updateDelayedAttackDatas :: [(Secs, Pos2)] -> [(Secs, Pos2)]
updateDelayedAttackDatas = \case
    []                           -> []
    ((ttl, pos):delayedAtkDatas) ->
        let ttl' = ttl - timeStep
        in if
            | ttl' > 0.0 -> (ttl', pos):updateDelayedAttackDatas delayedAtkDatas
            | otherwise  -> updateDelayedAttackDatas delayedAtkDatas

updateRecallSlashProjectileData
    :: MsgsReadWrite ThinkProjectileMsgsPhase m
    => Bool
    -> RecallSlashProjectileData
    -> m RecallSlashProjectileData
updateRecallSlashProjectileData isAtkMsgs recallSlashProjData =
    let
        recallSlashAtkDescs = _recallSlashAtkDescriptions recallSlashProjData
        recallSlashAtkDescs'
            | isAtkMsgs     = recallSlashAtkDescs
            | otherwise     = safeTail recallSlashAtkDescs

        targetPlayerPos = _targetPlayerPos recallSlashProjData
        delayedAtkDatas = _delayedAttackDatas recallSlashProjData
    in readPlayerPos <&> \case
        Just playerPos
            | vecDist playerPos targetPlayerPos > slashRadius / 2.0 && isAtkMsgs ->
                let
                    floatingAtkPos   = maybe playerPos snd (listToMaybe delayedAtkDatas)
                    delayedAtkDatas' =
                        [ (ttl - initialWaitSecs, pos)
                        | (ttl, pos) <- safeTail $ calculateDelayedAttackDatas floatingAtkPos playerPos
                        ]
                in recallSlashProjData
                    { _delayedAttackDatas         = delayedAtkDatas'
                    , _recallSlashAtkDescriptions = recallSlashAtkDescs'
                    , _targetPlayerPos            = playerPos
                    }

        _ -> recallSlashProjData
            { _delayedAttackDatas         = updateDelayedAttackDatas delayedAtkDatas
            , _recallSlashAtkDescriptions = recallSlashAtkDescs'
            }

thinkRecallSlashProjectile :: MsgsReadWrite ThinkProjectileMsgsPhase m => ProjectileThink RecallSlashProjectileData m
thinkRecallSlashProjectile recallSlashProj = do
    let
        recallSlashProjData = _data recallSlashProj
        atkMsgs             = mkDelayedAttackMsgs recallSlashProjData
        isAtkMsgs           = not $ null atkMsgs
    writeMsgs atkMsgs

    recallSlashProjData' <- updateRecallSlashProjectileData isAtkMsgs recallSlashProjData
    let
        ttl
            | null (_delayedAttackDatas recallSlashProjData') = 0.0
            | otherwise                                       = _ttl recallSlashProj

        update    = \p -> p
            { _data = recallSlashProjData'
            , _ttl  = ttl
            }
        updateMsg = mkMsgTo (ProjectileMsgUpdate update) (P._msgId recallSlashProj)

    return $ updateMsg:atkMsgs
