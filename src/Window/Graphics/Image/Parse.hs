module Window.Graphics.Image.Parse
    ( loadImage
    , loadPackImage
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson.Types       (FromJSON, genericParseJSON, parseJSON)
import Data.Maybe             (fromMaybe)
import Data.Yaml              (decodeEither')
import GHC.Generics           (Generic)
import qualified Data.ByteString as BS

import FileCache
import Util
import Window.Graphics.Image
import Window.Graphics.SubRect
import Window.Graphics.Texture
import Window.Graphics.Types

data ImageJSON = ImageJSON
    { _texture       :: String
    , _topLeftOffset :: Pos2
    , _subRect       :: SubRect
    , _origin        :: Maybe Pos2
    }
    deriving Generic

instance FromJSON ImageJSON where
    parseJSON = genericParseJSON aesonFieldDropUnderscore

loadImage :: (GraphicsRead m, MonadIO m) => FilePath -> m Image
loadImage filePath = do
    filePath'  <- translateResourcePath filePath
    imgByteStr <- liftIO $ BS.readFile filePath'
    parseLoadImage filePath' imgByteStr

parseLoadImage :: (GraphicsRead m, MonadIO m) => FilePath -> BS.ByteString -> m Image
parseLoadImage filePath imgByteStr = case decodeEither' imgByteStr of
    Left e          -> error $ filePath ++ ": " ++ show e
    Right imageJSON ->
        let
            texturePath   = _texture (imageJSON :: ImageJSON)
            topLeftOffset = _topLeftOffset (imageJSON :: ImageJSON)
            subRect       = _subRect (imageJSON :: ImageJSON)
            originPos     = fromMaybe zeroPos2 (_origin imageJSON)
        in do
            texture <- loadTexture texturePath
            return $ mkImage texture subRect originPos topLeftOffset

loadPackImage :: (FileCache m, GraphicsRead m, MonadIO m) => PackResourceFilePath -> m Image
loadPackImage packResourceFilePath = do
    imgByteStr <- readFileCachePackResource packResourceFilePath
    parseLoadImage (_fileName packResourceFilePath) imgByteStr
