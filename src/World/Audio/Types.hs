module World.Audio.Types
    ( WorldAudio(..)
    ) where

import qualified Data.Map as M
import qualified Data.Set as S

import Audio.Fmod.Types
import Id
import World.Audio.LayeredMusic.Manager.Types

data WorldAudio = WorldAudio
    { _soundsMap                :: M.Map FilePath FmodSoundIndex
    , _soundUniqueIds           :: S.Set HashedId
    , _soundContinuousHashedIds :: S.Set HashedId
    , _layeredMusicManager      :: LayeredMusicManager
    }
