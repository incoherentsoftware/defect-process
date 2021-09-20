module Collision.OutOfBounds
    ( checkPlayerOutOfBounds
    , checkEnemyOutOfBounds
    ) where

import Collision
import Enemy as E
import Level.Room.Bounds
import Level.Room.Types
import Msg
import Player
import Util

outOfBoundsOffset = 5.0   :: Float
minOffTopOffset   = -50.0 :: Float

checkPlayerOutOfBounds :: Player -> Room -> [Msg ThinkCollisionMsgsPhase]
checkPlayerOutOfBounds player roomBounds = unexpectedMsgs ++ expectedMsgs
    where
        unexpectedMsgs = checkPlayerUnexpectedOutOfBounds player roomBounds
        expectedMsgs   = checkPlayerExpectedOutOfBounds player roomBounds

checkPlayerExpectedOutOfBounds :: Player -> Room -> [Msg ThinkCollisionMsgsPhase]
checkPlayerExpectedOutOfBounds player room = outOfBoundsMsgs ++ outOfArenaWallsMsgs
    where
        movePlayerInBoundsHorizMsg = \xOffset ->
            mkMsgEx (PlayerMsgUpdatePosition (`vecAdd` Pos2 xOffset 0.0)) MsgFrontOrder

        playerHbx         = playerHitbox player
        roomBounds        = _bounds room
        topBounds         = _topBounds (roomBounds :: RoomBounds)
        playerOffTopDiff  = hitboxTop playerHbx - topBounds
        isPlayerOffTop    = playerOffTopDiff < 0.0
        innerTopLeftDiff  = _innerTopLeftBounds roomBounds + outOfBoundsOffset - hitboxLeft playerHbx
        innerTopRightDiff = _innerTopRightBounds roomBounds - outOfBoundsOffset - hitboxRight playerHbx
        rightDiff         = hitboxRight playerHbx - _rightBounds (roomBounds :: RoomBounds)

        outOfBoundsHorizMsgs
            | innerTopLeftDiff > 0 && isPlayerOffTop  = [movePlayerInBoundsHorizMsg innerTopLeftDiff]
            | innerTopRightDiff < 0 && isPlayerOffTop = [movePlayerInBoundsHorizMsg innerTopRightDiff]
            | rightDiff > 0                           = [movePlayerInBoundsHorizMsg (-rightDiff)]
            | otherwise                               = []
        outOfBoundsVertMsgs
            | playerOffTopDiff < minOffTopOffset      =
                let updatePos = \(Pos2 x y) -> Pos2 x (max y (topBounds + minOffTopOffset))
                in [mkMsgEx (PlayerMsgUpdatePosition updatePos) MsgFrontOrder]
            | otherwise                               = []
        outOfBoundsMsgs                               = outOfBoundsHorizMsgs ++ outOfBoundsVertMsgs

        outOfArenaWallsMsgs = case _arenaWallsBounds roomBounds of
            Nothing               -> []
            Just arenaWallsBounds ->
                let
                    wallsInnerLeftBounds  = _innerLeftBounds arenaWallsBounds
                    wallsInnerRightBounds = _innerRightBounds arenaWallsBounds
                    wallsInnerLeftDiff    = wallsInnerLeftBounds + outOfBoundsOffset - hitboxLeft playerHbx
                    wallsInnerRightDiff   = wallsInnerRightBounds - outOfBoundsOffset - hitboxRight playerHbx
                in if
                    | wallsInnerLeftDiff > 0 && isPlayerOffTop  -> [movePlayerInBoundsHorizMsg wallsInnerLeftDiff]
                    | wallsInnerRightDiff < 0 && isPlayerOffTop -> [movePlayerInBoundsHorizMsg wallsInnerRightDiff]
                    | otherwise                                 -> []

-- shouldn't happen normally, this is a collision bug (or debug commands) if this results in any messages
checkPlayerUnexpectedOutOfBounds :: Player -> Room -> [Msg ThinkCollisionMsgsPhase]
checkPlayerUnexpectedOutOfBounds player room = case _arenaWallsBounds roomBounds of
    Nothing ->
        let
            outOfBounds =
                playerTop >= roomBottom ||
                playerRight <= _leftBounds roomBounds ||
                playerLeft >= _rightBounds roomBounds
        in if
            | outOfBounds ->
                let resetPos = _playerSpawnPos room
                in [mkMsgEx (PlayerMsgSetPosition resetPos) MsgEndOrder]
            | otherwise   -> []

    Just arenaWallsBounds ->
        let
            arenaInnerLeftBounds  = _innerLeftBounds arenaWallsBounds
            arenaInnerRightBounds = _innerRightBounds arenaWallsBounds
            outOfArenaWallsBounds =
                playerTop >= roomBottom ||
                playerRight <= arenaInnerLeftBounds ||
                playerLeft >= arenaInnerRightBounds
        in if
            | outOfArenaWallsBounds ->
                let
                    resetX   = arenaInnerLeftBounds + (arenaInnerRightBounds - arenaInnerLeftBounds) / 2.0
                    resetY   = _topBounds (arenaWallsBounds :: RoomArenaWallsBounds)
                    resetPos = Pos2 resetX resetY
                in [mkMsgEx (PlayerMsgSetPosition resetPos) MsgEndOrder]
            | otherwise             -> []

    where
        playerHbx   = playerHitbox player
        playerTop   = hitboxTop playerHbx
        playerRight = hitboxRight playerHbx
        playerLeft  = hitboxLeft playerHbx
        roomBounds  = _bounds room
        roomBottom  = _bottomBounds roomBounds

-- shouldn't happen normally, this is a collision bug (or debug commands) if this results in any messages
checkEnemyOutOfBounds :: [Some Enemy] -> Room -> [Msg ThinkCollisionMsgsPhase]
checkEnemyOutOfBounds [] _               = []
checkEnemyOutOfBounds ((Some e):es) room = msgs ++ checkEnemyOutOfBounds es room
    where
        enemyHbx    = enemyHitbox e
        roomBounds  = _bounds room
        outOfBounds =
            hitboxTop enemyHbx >= _bottomBounds roomBounds ||
            hitboxRight enemyHbx <= _leftBounds roomBounds ||
            hitboxLeft enemyHbx >= _rightBounds roomBounds

        msgs
            | outOfBounds = [mkMsgTo EnemyMsgSetDead (E._msgId e)]
            | otherwise   = []
