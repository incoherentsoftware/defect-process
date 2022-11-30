module Player.MovementSkill.All.TeleportSkill.DummyCollisionProjectile
    ( mkDummyCollisionProjectile
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (fromMaybe)
import qualified Data.Set as S

import Collision.Hitbox
import Id
import Msg
import Projectile
import Util
import World.Surface

dummySecs             = 0.001 :: Secs
hitboxTooCloseEpsilon = 5.0   :: Float
hitboxGroundEpsilon   = 1.0   :: Float

data TeleportMovement
    = TeleportLeft
    | TeleportRight
    | TeleportUp
    | TeleportDown
    deriving Eq

data DummyData = DummyData
    { _teleportPos      :: Pos2
    , _teleportMovement :: TeleportMovement
    , _playerHitbox     :: Hitbox
    }

mkDummyData :: Pos2 -> TeleportMovement -> Hitbox -> DummyData
mkDummyData teleportPos teleportMovement playerHbx = DummyData
    { _teleportPos      = teleportPos
    , _teleportMovement = teleportMovement
    , _playerHitbox     = playerHbx
    }

mkDummyCollisionProjectile :: MonadIO m => Hitbox -> Pos2 -> m (Some Projectile)
mkDummyCollisionProjectile playerHbx teleportPos@(Pos2 teleportX teleportY) =
    let
        Pos2 playerX playerY               = hitboxBotCenter playerHbx
        teleportMovement
            | teleportX `approxEq` playerX = if
                | teleportY < playerY -> TeleportUp
                | otherwise           -> TeleportDown
            | otherwise                    = if
                | teleportX < playerX -> TeleportLeft
                | otherwise           -> TeleportRight

        -- calculate swept collision box between current and intended teleport position
        playerWidthHalf           = hitboxWidth playerHbx / 2.0
        leftX
            | teleportX < playerX = teleportX - playerWidthHalf
            | otherwise           = hitboxLeft playerHbx
        rightX
            | teleportX > playerX = teleportX + playerWidthHalf
            | otherwise           = hitboxRight playerHbx
        topY
            | teleportY < playerY = teleportY - hitboxHeight playerHbx
            | otherwise           = hitboxTop playerHbx
        bottomY
            | teleportY > playerY = teleportY
            | otherwise           = hitboxBot playerHbx
        hbx                       = rectHitbox (Pos2 leftX topY) (rightX - leftX) (bottomY - topY)

        dummyData = mkDummyData teleportPos teleportMovement playerHbx
    in do
        msgId <- newId
        return . Some $ (mkProjectile dummyData msgId hbx dummySecs)
            { _registeredCollisions = S.fromList [ProjRegisteredSurfaceCollision]
            , _processCollisions    = processDummyCollisions
            }

processDummyCollisions :: ProjectileProcessCollisions DummyData
processDummyCollisions projCollisions dummyProj =
    [mkMsgEx (PlayerMsgSetPosition targetPos) MsgAfterNormalOrder] ++ playerAirMsgs
    where
        dummyData = _data dummyProj
        playerHbx = _playerHitbox dummyData

        isPlatformCollision :: ProjectileCollision -> Bool
        isPlatformCollision = \case
            (ProjSurfaceCollision _ PlatformSurface) -> True
            _                                        -> False

        ignoreHitbox :: Hitbox -> Bool
        ignoreHitbox hbx = case _teleportMovement dummyData of
            TeleportLeft  -> tooCloseRight || tooCloseTop || tooCloseBot
            TeleportRight -> tooCloseLeft || tooCloseTop || tooCloseBot
            TeleportUp    -> tooCloseLeft || tooCloseRight || tooCloseBot
            TeleportDown  -> tooCloseLeft || tooCloseRight || tooCloseTop
            where
                hbxApproxEq :: (Hitbox -> Float) -> (Hitbox -> Float) -> Bool
                hbxApproxEq hbxF playerHbxF = approxEqEx (hbxF hbx) (playerHbxF playerHbx) hitboxTooCloseEpsilon

                tooCloseLeft  = hitboxRight `hbxApproxEq` hitboxLeft
                tooCloseRight = hitboxLeft `hbxApproxEq` hitboxRight
                tooCloseTop   = hitboxBot `hbxApproxEq` hitboxTop
                tooCloseBot   = hitboxTop `hbxApproxEq` hitboxBot

        projCollisions' = filter (not . isPlatformCollision) projCollisions
        surfaceHbxs     = map projectileCollisionHitbox projCollisions'

        teleportPos@(Pos2 teleportX teleportY) = _teleportPos dummyData
        teleportMovement                       = _teleportMovement dummyData
        targetPos                              = case filter (not . ignoreHitbox) surfaceHbxs of
            []         -> teleportPos
            (hbx:hbxs) -> case teleportMovement of
                TeleportLeft  ->
                    let x = fromMaybe (hitboxRight hbx) (maybeMaximum $ map hitboxRight hbxs)
                    in Pos2 x teleportY
                TeleportRight ->
                    let x = fromMaybe (hitboxLeft hbx) (maybeMinimum $ map hitboxLeft hbxs)
                    in Pos2 x teleportY
                TeleportUp    ->
                    let y = fromMaybe (hitboxBot hbx) (maybeMaximum $ map hitboxBot hbxs)
                    in Pos2 teleportX y
                TeleportDown  ->
                    let y = fromMaybe (hitboxTop hbx) (maybeMinimum $ map hitboxTop hbxs)
                    in Pos2 teleportX y

        touchingGround = or
            [ approxEqEx (hitboxTop hbx) (hitboxBot playerHbx) hitboxGroundEpsilon
            | hbx <- surfaceHbxs
            ]

        playerAirMsgs
            | teleportMovement == TeleportUp || not touchingGround = [mkMsg PlayerMsgForceInAir]
            | otherwise                                            = []
