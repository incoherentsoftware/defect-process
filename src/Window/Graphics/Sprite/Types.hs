module Window.Graphics.Sprite.Types
    ( Sprite(..)
    ) where

import Util
import Window.Graphics.Image.Types
import Window.Graphics.Util

data Sprite = Sprite
    { _filePath     :: FilePath
    , _images       :: [Image]
    , _frameSecs    :: [Secs]
    , _frameIndex   :: FrameIndex
    , _numFrames    :: Int
    , _frameChanged :: Bool
    , _loopData     :: Maybe LoopData
    , _frameTags    :: [FrameTag]
    , _elapsedSecs  :: Secs
    }

instance Eq Sprite where
    (==) :: Sprite -> Sprite -> Bool
    (==) a1 a2 = _filePath a1 == _filePath a2
