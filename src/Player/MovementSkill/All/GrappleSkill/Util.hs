module Player.MovementSkill.All.GrappleSkill.Util
    ( calcWillOvershoot
    , readIsPlayerResetPrevHitboxMsg
    ) where

import Constants
import Msg
import Util

calcWillOvershoot :: Pos2 -> Vel2 -> Pos2 -> Bool
calcWillOvershoot pos@(Pos2 x _) vel targetPos@(Pos2 targetX _) = distOvershoot || horizPosOvershoot
    where
        pos'@(Pos2 x' _)  = pos `vecAdd` toPos2 (vel `vecMul` timeStep)
        distOvershoot     = vecDistSq pos' targetPos > vecDistSq pos targetPos
        horizPosOvershoot = (x' < targetX && x > targetX) || (x' > targetX && x < targetX)

readIsPlayerResetPrevHitboxMsg :: MsgsRead UpdatePlayerMsgsPhase m => m Bool
readIsPlayerResetPrevHitboxMsg = processMsgs <$> readMsgs
    where
        processMsgs :: [PlayerMsgPayload] -> Bool
        processMsgs []     = False
        processMsgs (p:ps) = case p of
            PlayerMsgResetPrevHitbox -> True
            _                        -> processMsgs ps
