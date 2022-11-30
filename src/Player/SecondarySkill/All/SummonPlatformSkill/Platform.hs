module Player.SecondarySkill.All.SummonPlatformSkill.Platform
    ( mkSummonedPlatform
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Set as S

import Collision
import Configs
import Configs.All.PlayerSkill
import Configs.All.PlayerSkill.SummonPlatform
import Constants
import FileCache
import Id
import InfoMsg.Util
import Msg
import Projectile as P
import Util
import Window.Graphics
import World.Surface
import World.ZIndex

platformWidth  = 183.0 :: Float
platformHeight = 23.0  :: Float

data SummonedPlatformSprites = SummonedPlatformSprites
    { _appearIdle :: Sprite
    , _disappear  :: Sprite
    }

mkSummonedPlatformSprites :: (FileCache m, GraphicsRead m, MonadIO m) => m SummonedPlatformSprites
mkSummonedPlatformSprites =
    SummonedPlatformSprites <$>
    loadPackSpr "summon-platform-appear-idle.spr" <*>
    loadPackSpr "summon-platform-disappear.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/player/player-skills.pack" f

data SummonedPlatformData = SummonedPlatformData
    { _timeoutTtl :: Secs
    , _sprite     :: Sprite
    , _sprites    :: SummonedPlatformSprites
    , _playerInfo :: Maybe PlayerInfo
    , _config     :: SummonPlatformConfig
    }

mkSummonedPlatformData :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => m SummonedPlatformData
mkSummonedPlatformData = do
    sprs <- mkSummonedPlatformSprites
    cfg  <- readConfig _playerSkill _summonPlatform
    return $ SummonedPlatformData
        { _timeoutTtl = _platformTimeoutSecs cfg
        , _sprite     = _appearIdle sprs
        , _sprites    = sprs
        , _playerInfo = Nothing
        , _config     = cfg
        }

summonedPlatformSurface :: ProjectileSurface SummonedPlatformData
summonedPlatformSurface platformProj = Just $ mkPlatformSurface (projectileHitbox platformProj)

summonedPlatformHitbox :: Pos2 -> Hitbox
summonedPlatformHitbox (Pos2 x y) = rectHitbox pos platformWidth platformHeight
    where pos = Pos2 (x - platformWidth / 2.0) y

mkSummonedPlatform :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> m (Some Projectile)
mkSummonedPlatform pos = do
    platformProjData <- mkSummonedPlatformData
    msgId            <- newId
    let hbx           = summonedPlatformHitbox pos

    return . Some $ (mkProjectile platformProjData msgId hbx maxSecs)
        { _registeredCollisions = S.fromList [ProjRegisteredPlayerCollision]
        , _surface              = summonedPlatformSurface
        , _think                = thinkSummonedPlatform
        , _update               = updateSummonedPlatform
        , _draw                 = drawSummonedPlatform
        , _processCollisions    = processCollisions
        }

isSummonedPlatformSprite :: Projectile SummonedPlatformData -> (SummonedPlatformSprites -> Sprite) -> Bool
isSummonedPlatformSprite platformProj spritesF = spr == spritesF sprs
    where
        platformProjData = _data platformProj
        spr              = _sprite platformProjData
        sprs             = _sprites platformProjData

thinkSummonedPlatform :: Monad m => ProjectileThink SummonedPlatformData m
thinkSummonedPlatform platformProj = return $ if
    | isDisappearSpr && spriteFinished spr    -> [mkMsgTo (ProjectileMsgSetTtl 0.0) (P._msgId platformProj)]
    | timeoutTtl <= 0.0 && not isDisappearSpr ->
        -- clear player platform dropping flag to prevent usual behavior of not allowing landing on another platform
        -- too close to the original platform dropped from
        [mkMsg PlayerMsgResetPlatformDropping]
    | otherwise                               -> []
    where
        isDisappearSpr   = isSummonedPlatformSprite platformProj _disappear
        platformProjData = _data platformProj
        spr              = _sprite platformProjData
        timeoutTtl       = _timeoutTtl platformProjData

readPlayerInfo :: MsgsRead UpdateProjectileMsgsPhase m => m (Maybe PlayerInfo)
readPlayerInfo = processMsgs <$> readMsgs
    where
        processMsgs :: [InfoMsgPayload] -> Maybe PlayerInfo
        processMsgs []     = Nothing
        processMsgs (p:ps) = case p of
            InfoMsgPlayer playerInfo -> Just playerInfo
            _                        -> processMsgs ps

updateSummonedPlatform :: MsgsRead UpdateProjectileMsgsPhase m => ProjectileUpdate SummonedPlatformData m
updateSummonedPlatform platformProj =
    let
        platformProjData = _data platformProj
        timeoutTtl       = _timeoutTtl platformProjData
        spr              = _sprite platformProjData
        disappearSpr     = _disappear $ _sprites platformProjData
        spr'             = if
            | timeoutTtl <= 0.0 && spr /= disappearSpr -> disappearSpr
            | otherwise                                -> updateSprite spr
    in do
        playerInfo <- readPlayerInfo

        let
            hbx  = projectileHitbox platformProj
            hbx' = case playerInfoPos <$> playerInfo of
                Just (Pos2 playerX _)
                    | isSummonedPlatformSprite platformProj _appearIdle ->
                        summonedPlatformHitbox $ Pos2 playerX (hitboxTop hbx)
                _                                                       -> hbx

        return $ platformProj
            { _data   = platformProjData
                { _timeoutTtl = max 0.0 (timeoutTtl - timeStep)
                , _sprite     = spr'
                , _playerInfo = playerInfo
                }
            , _hitbox = const hbx'
            }

resetSummonedPlatformTimeout :: Projectile SummonedPlatformData -> Projectile SummonedPlatformData
resetSummonedPlatformTimeout platformProj = platformProj
    { _data = platformProjData {_timeoutTtl = _platformTimeoutSecs $ _config platformProjData}
    }
    where platformProjData = _data platformProj

processCollisions :: ProjectileProcessCollisions SummonedPlatformData
processCollisions collisions platformProj = case collisions of
    []            -> []
    (collision:_) -> case collision of
        ProjPlayerCollision _
            | touchingGround -> [mkMsgTo (ProjectileMsgUpdate resetSummonedPlatformTimeout) (P._msgId platformProj)]
        _                    -> []
        where
            playerInfo     = _playerInfo $ _data platformProj
            touchingGround = maybe False _touchingGround playerInfo

drawSummonedPlatform :: (GraphicsReadWrite m, MonadIO m) => ProjectileDraw SummonedPlatformData m
drawSummonedPlatform platformProj =
    let
        platformProjData = _data platformProj
        spr              = _sprite (platformProjData :: SummonedPlatformData)
        pos              = hitboxTopCenter $ projectileHitbox platformProj
        timeoutTtl       = _timeoutTtl platformProjData
        velX             = case _playerInfo platformProjData of
            Just playerInfo
                | not (_touchingWall playerInfo) && timeoutTtl > 0.0 -> vecX $ _vel (playerInfo :: PlayerInfo)
            _                                                        -> 0.0
    in do
        pos' <- graphicsLerpPos pos (Vel2 velX 0.0)
        drawSprite pos' RightDir worldProjectileZIndex spr
