module Level.Room.DoorLightOverlay
    ( RoomDoorLightOverlayJSON(..)
    , RoomDoorLightOverlay(..)
    , mkRoomDoorLightOverlay
    , drawRoomDoorLightOverlay
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson.Types       (FromJSON, genericParseJSON, parseJSON)
import Data.Maybe             (fromMaybe)
import GHC.Generics           (Generic)

import FileCache
import Util
import Window.Graphics.Image
import Window.Graphics.Image.Parse
import Window.Graphics.Types
import World.ZIndex

doorLightOverlayImagePath =
    PackResourceFilePath "data/levels/level-items.pack" "door-light-overlay.image" :: PackResourceFilePath

data RoomDoorLightOverlayJSON = RoomDoorLightOverlayJSON
    { _pos    :: Pos2
    , _height :: Maybe Float
    }
    deriving Generic

instance FromJSON RoomDoorLightOverlayJSON where
    parseJSON = genericParseJSON aesonFieldDropUnderscore

data RoomDoorLightOverlay = RoomDoorLightOverlay
    { _pos    :: Pos2
    , _height :: Maybe Float
    , _image  :: Image
    }

mkRoomDoorLightOverlay :: (FileCache m, GraphicsRead m, MonadIO m) => Pos2 -> Maybe Float -> m RoomDoorLightOverlay
mkRoomDoorLightOverlay pos height = RoomDoorLightOverlay pos height <$> loadPackImage doorLightOverlayImagePath

drawRoomDoorLightOverlay :: (GraphicsReadWrite m, MonadIO m) => RoomDoorLightOverlay -> m ()
drawRoomDoorLightOverlay overlay = drawImageCropped pos RightDir playerOverBodyZIndex width height img
    where
        pos    = _pos (overlay :: RoomDoorLightOverlay)
        img    = _image (overlay :: RoomDoorLightOverlay)
        width  = imageWidth img
        height = fromMaybe (imageHeight img) (_height (overlay :: RoomDoorLightOverlay))
