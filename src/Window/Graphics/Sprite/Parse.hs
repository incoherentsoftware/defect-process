module Window.Graphics.Sprite.Parse
    ( SpriteJSON(_texture)
    , loadSprite
    , parseLoadSprite
    , loadPackSprite
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson.Types       (FromJSON, genericParseJSON, parseJSON)
import Data.Maybe             (fromMaybe, isJust)
import Data.Yaml              (decodeEither')
import GHC.Generics           (Generic)
import qualified Data.ByteString as BS

import FileCache
import Util
import Window.Graphics.Image
import Window.Graphics.Sprite.Types
import Window.Graphics.SubRect
import Window.Graphics.Texture
import Window.Graphics.Types
import Window.Graphics.Util

data SpriteJSON = SpriteJSON
    { _texture          :: String
    , _topLeftOffsets   :: [Pos2]
    , _subRects         :: [SubRect]
    , _frameSecs        :: [Secs]
    , _origins          :: [Pos2]
    , _loopFrameIndices :: Maybe [FrameIndex]
    , _maxLoops         :: Maybe Int
    , _frameTags        :: Maybe [(FrameTagName, [FrameIndex])]
    }
    deriving Generic

instance FromJSON SpriteJSON where
    parseJSON = genericParseJSON aesonFieldDropUnderscore

loadSprite :: (GraphicsRead m, MonadIO m) => FilePath -> m Sprite
loadSprite filePath = do
    filePath'     <- translateResourcePath filePath
    spriteByteStr <- liftIO $ BS.readFile filePath'
    parseLoadSprite filePath' spriteByteStr

parseLoadSprite :: (GraphicsRead m, MonadIO m) => FilePath -> BS.ByteString -> m Sprite
parseLoadSprite filePath spriteByteStr =
    let
        error' :: String -> a
        error' s = error $ filePath ++ ": " ++ s

        spriteJSON = case decodeEither' spriteByteStr of
            Left e     -> error' $ show e
            Right json -> json

        frameTags = do
            (tagName, tagIndices) <- fromMaybe [] (_frameTags (spriteJSON :: SpriteJSON))
            let
                (startFrameIndex, endFrameIndex, stepFrameIndex) = case tagIndices of
                    [index]                           -> (index, index, 1)
                    [startIndex, endIndex]            -> (startIndex, endIndex, 1)
                    [startIndex, endIndex, stepIndex] -> (startIndex, endIndex, stepIndex)
                    _                                 -> error' "invalid frame tag index/indices specified"
            return $ FrameTag
                { _name            = tagName
                , _startFrameIndex = startFrameIndex
                , _endFrameIndex   = endFrameIndex
                , _stepFrameIndex  = stepFrameIndex
                }

        loopFrameIndices = _loopFrameIndices (spriteJSON :: SpriteJSON)
        maxLoops         = _maxLoops (spriteJSON :: SpriteJSON)
        secs             = _frameSecs (spriteJSON :: SpriteJSON)
        numFrames        = length secs

        loopData = case loopFrameIndices of
            Nothing
                | isJust maxLoops       -> error' "maxLoops specified but no loopFrameIndices"
                | otherwise             -> Nothing
            Just [startIndex]           -> Just $ LoopData
                { _startFrameIndex = startIndex
                , _endFrameIndex   = FrameIndex $ numFrames - 1
                , _maxLoops        = maxLoops
                }
            Just [startIndex, endIndex] -> Just $ LoopData
                { _startFrameIndex = startIndex
                , _endFrameIndex   = endIndex
                , _maxLoops        = maxLoops
                }
            Just _                      -> error' "invalid loopFrameIndices"

        loopDataCycle' :: [a] -> [a]
        loopDataCycle' vs = loopDataCycle loopData vs

        texturePath    = _texture (spriteJSON :: SpriteJSON)
        origins        = _origins spriteJSON
        topLeftOffsets = _topLeftOffsets spriteJSON
        subRects       = _subRects (spriteJSON :: SpriteJSON)
    in do
        texture <- loadTexture texturePath
        let
            images =
                [ mkImage texture subRect origin topLeftOffset
                | (subRect, origin, topLeftOffset) <- zip3 subRects origins topLeftOffsets
                ]

        when (length secs /= length origins) $
            error' "frameSecs and origins lengths don't match"

        return $ Sprite
            { _filePath     = filePath
            , _images       = loopDataCycle' images
            , _frameSecs    = loopDataCycle' secs
            , _frameIndex   = 0
            , _numFrames    = numFrames
            , _frameChanged = True
            , _loopData     = loopData
            , _frameTags    = frameTags
            , _elapsedSecs  = 0.0
            }

loadPackSprite :: (FileCache m, GraphicsRead m, MonadIO m) => PackResourceFilePath -> m Sprite
loadPackSprite packResourceFilePath = do
    spriteByteStr <- readFileCachePackResource packResourceFilePath
    parseLoadSprite (_fileName packResourceFilePath) spriteByteStr
