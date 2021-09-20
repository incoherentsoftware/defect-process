module Player.AimBody
    ( module Player.AimBody.Types
    , vertAngle
    , horizAngle
    , calculateShoulderPos
    , calculateAimVec
    , calculateAimAngle
    , calculateAimAngleDir
    , calculateAimOverlayAngle
    ) where

import Configs.All.Player
import Player.AimBody.Types
import Util

vertAngle  = pi / 2.0 :: Radians
horizAngle = pi       :: Radians

calculateShoulderPos :: PlayerConfig -> Pos2 -> Pos2
calculateShoulderPos playerCfg (Pos2 playerX playerY) = Pos2 playerX shoulderY
    where
        shoulderOffsetY = vecY (_legsHipsOffset playerCfg) + vecY (_leadShoulderHipsOffset playerCfg)
        shoulderY       = playerY + shoulderOffsetY

calculateAimVec :: PlayerConfig -> Pos2 -> Pos2 -> Vec2
calculateAimVec playerCfg playerPos@(Pos2 playerX _) (Pos2 aimX aimY) = Vec2 (aimX - playerX) (aimY - playerY')
    where playerY' = vecY $ calculateShoulderPos playerCfg playerPos

calculateAimAngle :: PlayerConfig -> Pos2 -> Pos2 -> Radians
calculateAimAngle playerCfg playerPos aimPos = atan2 aimVecY aimVecX
    where Vec2 aimVecX aimVecY = calculateAimVec playerCfg playerPos aimPos

calculateAimAngleDir :: Radians -> Direction
calculateAimAngleDir angle
    | abs angle > vertAngle = LeftDir
    | otherwise             = RightDir

calculateAimOverlayAngle :: Radians -> Radians
calculateAimOverlayAngle aimAngle
    | aimAngle < -vertAngle = aimAngle + horizAngle
    | aimAngle > vertAngle  = aimAngle - horizAngle
    | otherwise             = aimAngle
