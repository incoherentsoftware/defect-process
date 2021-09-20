module Level.Room.Chooser
    ( module Level.Room.Chooser.Types
    , mkRoomChooser
    , nextRoomChooserRoomType
    , nextRoomChooserRoomTypeSkipTransition
    , setRoomChooserRoom
    , roomChooserRoomContentType
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Random   (MonadRandom)
import System.Directory       (listDirectory)
import System.FilePath        (hasExtension, takeBaseName)
import System.Random.Shuffle  (shuffleM)
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text as T

import Configs
import Configs.All.Level
import Level.Room
import Level.Room.Chooser.Types
import Util

arenasDirectoryPath = "data/levels/arenas" :: FilePath

-- NOTE: this is modified from the full source, most of the room chooser logic is not included
mkRoomChooser :: (ConfigsRead m, MonadIO m, MonadRandom m) => m RoomChooser
mkRoomChooser =
    let
        listRoomNames :: MonadIO m1 => FilePath -> m1 [FilePath]
        listRoomNames dirPath = do
            dirPath' <- translateResourcePath dirPath
            subDirs  <- filter (not . hasExtension) <$> liftIO (listDirectory dirPath')
            return
                [ name
                | subDir <- subDirs
                , let name = takeBaseName subDir
                , not $ "_" `L.isPrefixOf` name
                ]
    in do
        arenaBaseFileNames <- shuffleM =<< listRoomNames arenasDirectoryPath

        let arenaFileIds = map (ArenaRoomType . T.pack) arenaBaseFileNames
        maxNumArenas    <- readConfig _level _maxNumArenas
        let numArenas    = min maxNumArenas (length arenaFileIds)

        return $ RoomChooser
            { _currentRoomType       = EmptyRoomType
            , _numArenas             = numArenas
            , _visitedArenaRoomNames = S.empty
            }

-- NOTE: this is modified from the full source, most of the room chooser logic is not included
nextRoomChooserRoomType :: RoomChooser -> RoomType
nextRoomChooserRoomType roomChooser = _currentRoomType roomChooser

-- NOTE: this is modified from the full source, most of the room chooser logic is not included
nextRoomChooserRoomTypeSkipTransition :: RoomChooser -> RoomType
nextRoomChooserRoomTypeSkipTransition roomChooser = _currentRoomType roomChooser

-- NOTE: this is modified from the full source, most of the room chooser logic is not included
setRoomChooserRoom :: MonadIO m => Room -> RoomChooser -> m RoomChooser
setRoomChooserRoom currentRoom roomChooser = return $ roomChooser {_currentRoomType = _type currentRoom}

-- NOTE: this is modified from the full source, most of the room chooser logic is not included
roomChooserRoomContentType :: RoomType -> RoomChooser -> RoomContentType
roomChooserRoomContentType roomType _
    | roomType == startingShopRoomType = ShopContentType
    | roomType == endHallwayRoomType   = ShopContentType
    | otherwise                        = NoContentType
