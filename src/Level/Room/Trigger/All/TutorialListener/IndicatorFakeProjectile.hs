module Level.Room.Trigger.All.TutorialListener.IndicatorFakeProjectile
    ( mkIndicatorFakeProjectile
    ) where

import Control.Monad.IO.Class (MonadIO)

import Collision
import FileCache
import Msg
import Projectile
import Util
import Window.Graphics
import World.ZIndex

indicatorLeftSpritePos  = Pos2 510.0 3140.0 :: Pos2
indicatorRightSpritePos = Pos2 616.0 3140.0 :: Pos2

indicatorSpritePath =
    PackResourceFilePath "data/levels/level-items.pack" "item-pickup-indicator.spr" :: PackResourceFilePath

data IndicatorFakeProjectileData = IndicatorFakeProjectileData
    { _sprite :: Sprite
    }

mkIndicatorFakeProjectileData :: (FileCache m, GraphicsRead m, MonadIO m) => m IndicatorFakeProjectileData
mkIndicatorFakeProjectileData = IndicatorFakeProjectileData <$> loadPackSprite indicatorSpritePath

mkIndicatorFakeProjectile :: (FileCache m, GraphicsRead m, MonadIO m) => MsgId -> m (Some Projectile)
mkIndicatorFakeProjectile msgId = do
    indicatorFakeProjData <- mkIndicatorFakeProjectileData
    let dummyHbx           = dummyHitbox zeroPos2

    return . Some $ (mkProjectile indicatorFakeProjData msgId dummyHbx maxSecs)
        { _update = update
        , _draw   = draw
        }

update :: Monad m => ProjectileUpdate IndicatorFakeProjectileData m
update indicatorFakeProj =
    let
        indicatorFakeProjData = _data indicatorFakeProj
        spr                   = updateSprite $ _sprite indicatorFakeProjData
    in return $ indicatorFakeProj
        { _data = indicatorFakeProjData {_sprite = spr}
        }

draw :: (GraphicsReadWrite m, MonadIO m) => ProjectileDraw IndicatorFakeProjectileData m
draw indicatorFakeProj = do
    let spr = _sprite $ _data indicatorFakeProj
    drawSprite indicatorLeftSpritePos RightDir levelItemLabelZIndex spr
    drawSprite indicatorRightSpritePos LeftDir levelItemLabelZIndex spr
