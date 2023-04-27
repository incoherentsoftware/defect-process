module Window.Graphics.Sprite
    ( module Window.Graphics.Sprite.Types
    , module Window.Graphics.Sprite.Parse
    , drawSprite
    , drawSpriteRotated
    , drawSpriteScaled
    , drawSpriteEx
    , drawSpriteRect
    , updateSprite
    , spriteFinished
    , spriteIsLastFrameIndex
    , spriteLoopFrameCount
    , spriteImage
    , spriteImageWidth
    , spriteImageHeight
    , finishSprite
    , advanceSprite
    , isSpriteFrameTag
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (listToMaybe)

import Constants
import Util
import Window.Graphics.Image
import Window.Graphics.Opacity
import Window.Graphics.Sprite.Parse
import Window.Graphics.Sprite.Types
import Window.Graphics.Types
import Window.Graphics.Util

frameSecsEpsilon = 0.001 :: Float

drawSprite :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> Direction -> ZIndex -> Sprite -> m ()
drawSprite pos dir zIndex spr = drawSpriteEx pos dir zIndex 0.0 FullOpacity NonScaled spr

drawSpriteRotated :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> Direction -> ZIndex -> Radians -> Sprite -> m ()
drawSpriteRotated pos dir zIndex angle spr = drawSpriteEx pos dir zIndex angle FullOpacity NonScaled spr

drawSpriteScaled :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> Direction -> ZIndex -> DrawScale -> Sprite -> m ()
drawSpriteScaled pos dir zIndex drawScale spr = drawSpriteEx pos dir zIndex 0.0 FullOpacity drawScale spr

drawSpriteEx
    :: (GraphicsReadWrite m, MonadIO m)
    => Pos2
    -> Direction
    -> ZIndex
    -> Radians
    -> Opacity
    -> DrawScale
    -> Sprite
    -> m ()
drawSpriteEx pos dir zIndex angle opacity drawScale spr = case _images spr of
    (img:_) -> drawImageEx pos dir zIndex angle opacity drawScale img
    []      -> return ()

drawSpriteRect :: (GraphicsReadWrite m, MonadIO m) => Pos2 -> Float -> Float -> ZIndex -> Sprite -> m ()
drawSpriteRect pos width height zIndex spr = case _images spr of
    (img:_) -> drawImageRect pos width height zIndex img
    []      -> return ()

updateSprite :: Sprite -> Sprite
updateSprite spr
    | spriteFinished spr = spr
    | otherwise          = spr
        { _frameSecs    = secs'
        , _images       = images'
        , _frameIndex   = frameIndex'
        , _frameChanged = newFrame
        , _elapsedSecs  = _elapsedSecs spr + timeStep
        }
            where
                sec         = maybe 0.0 (subtract timeStep) (listToMaybe $ _frameSecs spr)
                advFrame    = sec <= frameSecsEpsilon
                images      = _images spr
                lastFrame   = null $ safeTail images
                newFrame    = advFrame && not lastFrame
                secs        = safeTail $ _frameSecs spr
                secs'       = if advFrame then secs else sec:secs
                frameIndex  = _frameIndex spr
                frameIndex' = if newFrame then frameIndex + 1 else frameIndex

                -- let last frame linger when sprite is done
                images' = if newFrame then safeTail images else images

spriteFinished :: Sprite -> Bool
spriteFinished = null . _frameSecs

spriteIsLastFrameIndex :: Sprite -> Bool
spriteIsLastFrameIndex = null . safeTail . _frameSecs

spriteLoopFrameCount :: Sprite -> FrameIndex
spriteLoopFrameCount spr = case _loopData spr of
    Nothing       -> 0
    Just loopData ->
        let
            startIndex = _startFrameIndex (loopData :: LoopData)
            endIndex   = _endFrameIndex (loopData :: LoopData)
            frameIndex = _frameIndex spr
        in (frameIndex - startIndex) `div` (endIndex + 1 - startIndex)

spriteImage :: Sprite -> Maybe Image
spriteImage = listToMaybe . _images

spriteImageWidth :: Sprite -> Float
spriteImageWidth = maybe 0.0 imageWidth . spriteImage

spriteImageHeight :: Sprite -> Float
spriteImageHeight = maybe 0.0 imageHeight . spriteImage

finishSprite :: Sprite -> Sprite
finishSprite spr = spr
    { _images    = drop numRemainingFrames (_images spr)
    , _frameSecs = []
    }
    where
        frameIndex         = _int $ _frameIndex spr
        numRemainingFrames = max 0 (_numFrames spr - 1 - frameIndex)

-- not very efficient, fine for current use though
advanceSprite :: FrameIndex -> Sprite -> Sprite
advanceSprite frameIndex !spr
    | _frameIndex spr >= frameIndex || spriteFinished spr = spr
    | otherwise                                           = advanceSprite frameIndex (updateSprite spr)

isSpriteFrameTag :: FrameTagName -> Sprite -> Bool
isSpriteFrameTag tagName spr = or
    [ _name tag == tagName && sprFrameIndex >= startIndex && sprFrameIndex <= endIndex && withinStep
    | tag <- _frameTags spr
    , let startIndex = _startFrameIndex (tag :: FrameTag)
    , let endIndex   = _endFrameIndex (tag :: FrameTag)
    , let stepIndex  = _stepFrameIndex tag
    , let withinStep = if stepIndex > 1 then sprFrameIndex `mod` stepIndex == startIndex else True
    ]
    where sprFrameIndex = _frameIndex spr
