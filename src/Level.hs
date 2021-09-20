module Level
    ( module Level.Types
    , mkLevel
    , thinkLevel
    , updateLevel
    , changeLevelRoom
    ) where

import Control.Monad.IO.Class (MonadIO)

import AppEnv
import Level.Room
import Level.Room.Chooser
import Level.Room.Empty
import Level.Types
import Msg

startingDangerValue = DangerValue 5 :: DangerValue
intervalDangerValue = DangerValue 5 :: DangerValue

mkLevel :: AppEnv SetupMsgsPhase Level
mkLevel = Level startingDangerValue mkEmptyRoom <$> mkRoomChooser

thinkLevel :: Level -> AppEnv ThinkLevelMsgsPhase ()
thinkLevel level = thinkRoom $ _room level

updateLevel :: Level -> AppEnv UpdateLevelMsgsPhase Level
updateLevel level = do
    room <- updateRoom $ _room level
    return $ level {_room = room}

changeLevelRoom :: (MsgsWrite p m, MonadIO m) => Room -> Level -> m Level
changeLevelRoom room level = do
    roomChooser <- setRoomChooserRoom room (_roomChooser level)

    let
        prevRoom            = _room level
        currentDangerValue  = _currentDangerValue level
        currentDangerValue' = case _type prevRoom of
            ArenaRoomType _ -> currentDangerValue + intervalDangerValue
            _               -> currentDangerValue

    return $ level
        { _currentDangerValue = currentDangerValue'
        , _room               = room
        , _roomChooser        = roomChooser
        }
