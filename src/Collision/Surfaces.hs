module Collision.Surfaces
    ( checkPlayerSurfaceCollisions
    , checkEnemySurfaceCollisions
    , checkRoomItemSurfaceCollisions
    ) where

import Collision
import Enemy.Types
import Level.Room.Item.Types
import Msg
import Player
import Util
import World.Surface

roofCollideOffset     = 1.0 :: Float
minPrevHitboxMoveDist = 0.1 :: Float

data CollisionFlag
    = GroundFlag
    | NoFlag
    deriving Eq

type CollisionResponse = (Msg ThinkCollisionMsgsPhase, CollisionFlag)

processPlatformSurfaceCollision :: CollisionEntity e => e -> Hitbox -> Maybe CollisionResponse
processPlatformSurfaceCollision collisionEntity surfaceHbx = Just (msg, GroundFlag)
    where
        surfaceTop        = hitboxTop surfaceHbx
        collisionEntityId = collisionEntityMsgId collisionEntity
        msg               = mkMsgTo (CollisionMsgTouchingGround surfaceTop PlatformSurface) collisionEntityId

processSpeedRailSurfaceCollision :: CollisionEntity e => Direction -> e -> Hitbox -> Maybe CollisionResponse
processSpeedRailSurfaceCollision dir collisionEntity surfaceHbx = Just (msg, GroundFlag)
    where
        surfaceTop        = hitboxTop surfaceHbx
        surfaceType       = SpeedRailSurface dir
        collisionEntityId = collisionEntityMsgId collisionEntity
        msg               = mkMsgTo (CollisionMsgTouchingGround surfaceTop surfaceType) collisionEntityId

processGeneralSurfaceCollision :: CollisionEntity e => e -> Hitbox -> Maybe CollisionResponse
processGeneralSurfaceCollision collisionEntity surfaceHbx
    | not (movingLeft || movingRight) =
        let underOrOver = collisionEntityLeft < surfaceRight || collisionEntityRight > surfaceLeft
        in if
            | collisionEntityBot == surfaceTop                                                                ->
                Just groundResponse
            | collisionEntityLeft == surfaceRight                                                             ->
                Nothing
            | collisionEntityRight == surfaceLeft                                                             ->
                Nothing
            | velY > 0.0 && collisionEntityBot > surfaceTop && collisionEntityTop < surfaceTop && underOrOver ->
                Just groundResponse
            | velY < 0.0 && collisionEntityTop < surfaceBot && collisionEntityBot > surfaceBot && underOrOver ->
                Just roofResponse
            | otherwise                                                                                       ->
                Nothing

    | velY == 0.0 = if
        | collisionEntityBot == surfaceTop                   -> Nothing
        | collisionEntityTop == surfaceBot                   -> Nothing
        | movingRight && collisionEntityRight >= surfaceLeft -> Just rightWallResponse
        | movingLeft && collisionEntityLeft <= surfaceRight  -> Just leftWallResponse
        | otherwise                                          -> Nothing

    | otherwise =
        let
            respFn :: Float -> Float -> (Float -> Float -> Bool) -> CollisionResponse -> Maybe CollisionResponse
            respFn y0 y1 yOp yResponse
                | y0 == y1    = Just yResponse
                | y0 `yOp` y1 =
                    if
                        | movingLeft && collisionEntityLeft < surfaceRight  -> Just $ if
                            | abs (collisionEntityLeft - surfaceRight) < abs (y0 - y1) -> wallResponse
                            | otherwise                                                -> yResponse
                        | movingRight && collisionEntityRight > surfaceLeft -> Just $ if
                            | abs (collisionEntityRight - surfaceLeft) < abs (y0 - y1) -> wallResponse
                            | otherwise                                                -> yResponse
                        | collisionEntityRight == surfaceLeft               -> Nothing
                        | collisionEntityLeft == surfaceRight               -> Nothing
                        | otherwise                                         -> Just yResponse
                | otherwise   = Just wallResponse
        in if
            | velY < 0.0 -> respFn collisionEntityTop surfaceBot (<) roofResponse
            | otherwise  -> respFn collisionEntityBot surfaceTop (>) groundResponse

    where
        Vel2 velX velY         = collisionEntityVel collisionEntity
        collisionEntityHbx     = collisionEntityHitbox collisionEntity
        collisionEntityPrevHbx = collisionEntityPrevHitbox collisionEntity
        leftDiff               = hitboxLeft collisionEntityHbx - hitboxLeft collisionEntityPrevHbx
        rightDiff              = hitboxRight collisionEntityHbx - hitboxRight collisionEntityPrevHbx
        movingRight            = velX > 0.0 || leftDiff >= minPrevHitboxMoveDist
        movingLeft             = velX < 0.0 || rightDiff <= -minPrevHitboxMoveDist

        collisionEntityId    = collisionEntityMsgId collisionEntity
        collisionEntityLeft  = hitboxLeft collisionEntityHbx
        collisionEntityTop   = hitboxTop collisionEntityHbx
        collisionEntityRight = hitboxRight collisionEntityHbx
        collisionEntityBot   = hitboxBot collisionEntityHbx

        surfaceLeft  = hitboxLeft surfaceHbx
        surfaceTop   = hitboxTop surfaceHbx
        surfaceRight = hitboxRight surfaceHbx
        surfaceBot   = hitboxBot surfaceHbx

        groundResponse =
            ( mkMsgTo (CollisionMsgTouchingGround surfaceTop GeneralSurface) collisionEntityId
            , GroundFlag
            )
        roofResponse   =
            ( mkMsgTo (CollisionMsgTouchingRoof (surfaceBot + roofCollideOffset)) collisionEntityId
            , NoFlag
            )

        wallResponse =
            let
                (wallX, wallType)
                    | collisionEntityLeft < surfaceLeft = (surfaceLeft, RightWallSurface)
                    | otherwise                         = (surfaceRight, LeftWallSurface)
            in
                ( mkMsgTo (CollisionMsgTouchingWall wallX surfaceTop wallType) collisionEntityId
                , NoFlag
                )

        rightWallResponse =
            ( mkMsgTo (CollisionMsgTouchingWall surfaceLeft surfaceTop RightWallSurface) collisionEntityId
            , NoFlag
            )
        leftWallResponse  =
            ( mkMsgTo (CollisionMsgTouchingWall surfaceRight surfaceTop LeftWallSurface) collisionEntityId
            , NoFlag
            )

checkFallOffSurfaces :: CollisionEntity e => e -> [Surface] -> [Msg ThinkCollisionMsgsPhase]
checkFallOffSurfaces collisionEntity surfaces = maybe [] (checkFallOff surfaces) fallOffHbx
    where
        checkFallOff :: [Surface] -> Hitbox -> [Msg ThinkCollisionMsgsPhase]
        checkFallOff [] _     =
            let collisionEntityId = collisionEntityMsgId collisionEntity
            in [mkMsgTo CollisionMsgWillFallOffGround collisionEntityId]
        checkFallOff (s:ss) h
            | h `intersectsHitbox` _hitbox (s :: Surface) = []
            | otherwise                                   = checkFallOff ss h

        hbx              = collisionEntityHitbox collisionEntity
        Pos2 x y         = hitboxTopLeft hbx
        width            = hitboxWidth hbx
        height           = hitboxHeight hbx
        velX             = vecX $ collisionEntityVel collisionEntity
        fallOffHbx
            | velX < 0.0 = Just $ rectHitbox (Pos2 (x - width) y) width height
            | velX > 0.0 = Just $ rectHitbox (Pos2 (x + width) y) width height
            | otherwise  = Nothing

checkSurfaceCollisions :: CollisionEntity e => e -> [Surface] -> [CollisionResponse]
checkSurfaceCollisions _ [] = []
checkSurfaceCollisions collisionEntity (surface:surfaces)
    | collisionEntityHbx `intersectsHitbox` surfaceHbx =
        let
            collisionResponse = case _type (surface :: Surface) of
                GeneralSurface       -> processGeneralSurfaceCollision
                PlatformSurface      -> processPlatformSurfaceCollision
                SpeedRailSurface dir -> processSpeedRailSurfaceCollision dir
            resp              = collisionResponse collisionEntity surfaceHbx
        in maybe resps (:resps) resp

    | otherwise = resps

    where
        collisionEntityHbx = collisionEntityHitbox collisionEntity
        surfaceHbx         = _hitbox (surface :: Surface)
        resps              = checkSurfaceCollisions collisionEntity surfaces

checkWallProximitySurfaceCollisions :: CollisionEntity e => e -> [Surface] -> [Msg ThinkCollisionMsgsPhase]
checkWallProximitySurfaceCollisions collisionEntity surfaces =
    case collisionEntityWallProximityHitbox collisionEntity of
        Nothing              -> []
        Just wallProxmityHbx ->
            let
                checkCollisions :: [Surface] -> [Msg ThinkCollisionMsgsPhase]
                checkCollisions [] = []
                checkCollisions (s:ss)
                    | wallProxmityHbx `intersectsHitbox` surfaceHbx =
                        let
                            hbx         = collisionEntityHitbox collisionEntity
                            leftOffset  = hitboxLeft hbx - hitboxRight surfaceHbx
                            rightOffset = hitboxLeft surfaceHbx - hitboxRight hbx

                            (offsetX, wallSurfType)
                                | abs leftOffset < abs rightOffset = (leftOffset, LeftWallSurface)
                                | otherwise                        = (rightOffset, RightWallSurface)

                            collisionEntityId = collisionEntityMsgId collisionEntity
                        in [mkMsgTo (CollisionMsgWallProximity offsetX wallSurfType) collisionEntityId]

                    | otherwise = checkCollisions ss

                    where surfaceHbx = _hitbox (s :: Surface)
            in checkCollisions $ filter isGeneralSurface surfaces

checkCollisionEntitySurfaceCollisions :: CollisionEntity e => e -> [Surface] -> [Msg ThinkCollisionMsgsPhase]
checkCollisionEntitySurfaceCollisions collisionEntity surfaces = fallOffMsgs ++ collisionMsgs ++ wallProximityMsgs
    where
        hasGroundFlag                   = any (== GroundFlag) collisionFlags
        fallOffMsgs
            | hasGroundFlag             = checkFallOffSurfaces collisionEntity surfaces
            | otherwise                 = []
        (collisionMsgs, collisionFlags) = unzip $ checkSurfaceCollisions collisionEntity surfaces
        wallProximityMsgs               = checkWallProximitySurfaceCollisions collisionEntity surfaces

checkPlayerSurfaceCollisions :: Player -> [Surface] -> [Msg ThinkCollisionMsgsPhase]
checkPlayerSurfaceCollisions player surfaces = checkCollisionEntitySurfaceCollisions player surfaces

checkEnemySurfaceCollisions :: [Some Enemy] -> [Surface] -> [Msg ThinkCollisionMsgsPhase]
checkEnemySurfaceCollisions enemies surfaces =
    concatMap (\(Some e) -> checkCollisionEntitySurfaceCollisions e surfaces) enemies

checkRoomItemSurfaceCollisions :: [Some RoomItem] -> [Surface] -> [Msg ThinkCollisionMsgsPhase]
checkRoomItemSurfaceCollisions roomItems surfaces =
    concatMap (\(Some ri) -> checkCollisionEntitySurfaceCollisions ri surfaces) roomItems
