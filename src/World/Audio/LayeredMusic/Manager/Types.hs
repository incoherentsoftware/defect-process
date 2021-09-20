module World.Audio.LayeredMusic.Manager.Types
    ( NowPlaying(..)
    , LayeredMusicManager(..)
    ) where

import Level.Room.Types
import Util
import World.Audio.LayeredMusic.Types

data NowPlaying
    = NoMusicPlaying
    | BattleMusicPlaying
    | PostBattleExplorationMusicPlaying
    | ExplorationMusicPlaying
    | JukeboxMusicPlaying
    deriving (Eq, Show)
    deriving anyclass PrettyShow

data LayeredMusicManager = LayeredMusicManager
    { _nowPlaying       :: NowPlaying
    , _battleMusic      :: LayeredMusic
    , _explorationMusic :: LayeredMusic
    , _currentRoomType  :: RoomType
    , _numVisitedArenas :: Int
    , _numArenas        :: Int
    }
