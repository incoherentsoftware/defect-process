module Enemy.All.Boss.LightingFakeProjectile
    ( mkLightingFakeProjectile
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Functor           ((<&>))

import Collision
import Constants
import Id
import Msg
import Projectile as P
import Util
import Window.Graphics
import World.ZIndex

targetAlpha       = 70  :: Alpha
darkenMultiplier  = 0.3 :: Float
lightenMultiplier = 0.1 :: Float

data LightingFakeProjStatus
    = DarkeningStatus
    | LighteningStatus
    deriving Show

data LightingFakeProjData = LightingFakeProjData
    { _status  :: LightingFakeProjStatus
    , _opacity :: Opacity
    }

mkLightingFakeProjData :: LightingFakeProjData
mkLightingFakeProjData = LightingFakeProjData
    { _status  = DarkeningStatus
    , _opacity = Opacity 0.0
    }

mkLightingFakeProjectile :: MonadIO m => MsgId -> m (Some Projectile)
mkLightingFakeProjectile ownerId = do
    msgId  <- newId
    let hbx = DummyHitbox zeroPos2

    return . Some $ (mkProjectile mkLightingFakeProjData msgId hbx maxSecs)
        { _ownerId = ownerId
        , _update  = updateLightingFakeProj
        , _draw    = drawLightingFakeProj
        }

isOwnerAlive :: MsgsRead UpdateProjectileMsgsPhase m => Projectile LightingFakeProjData -> m Bool
isOwnerAlive darkenFakeProj = processMsgs <$> readMsgs
    where
        processMsgs :: [InfoMsgPayload] -> Bool
        processMsgs []     = False
        processMsgs (p:ps) = case p of
            InfoMsgEnemyPos _ msgId
                | msgId == _ownerId darkenFakeProj -> True
            _                                      -> processMsgs ps

updateLightingFakeProj :: MsgsRead UpdateProjectileMsgsPhase m => ProjectileUpdate LightingFakeProjData m
updateLightingFakeProj darkenFakeProj =
    let
        darkenFakeProjData = _data darkenFakeProj
        opacity            = _opacity darkenFakeProjData
        alpha              = opacityToAlpha opacity
        status             = _status darkenFakeProjData
        opacity'           = case status of
            DarkeningStatus
                | alpha < targetAlpha -> increaseOpacity (darkenMultiplier * timeStep) targetAlpha opacity
            LighteningStatus          -> decreaseOpacity (lightenMultiplier * timeStep) opacity
            _                         -> opacity

        ttl = case status of
            LighteningStatus
                | isMinOpacity opacity' -> 0.0
            _                           -> _ttl darkenFakeProj
    in do
        status' <- isOwnerAlive darkenFakeProj <&> \case
            True  -> status
            False -> LighteningStatus

        return $ darkenFakeProj
            { _data = darkenFakeProjData
                { _status  = status'
                , _opacity = opacity'
                }
            , _ttl  = ttl
            }

drawLightingFakeProj :: (GraphicsReadWrite m, MonadIO m) => ProjectileDraw LightingFakeProjData m
drawLightingFakeProj darkenFakeProj =
    let
        opacity = _opacity $ _data darkenFakeProj
        color   = Color 0 0 0 (opacityToAlpha opacity)
    in do
        setCameraSpace CameraScreenSpace
        drawRect zeroPos2 virtualRenderWidth virtualRenderHeight color levelImageLayerZIndex
        setCameraSpace CameraWorldSpace
