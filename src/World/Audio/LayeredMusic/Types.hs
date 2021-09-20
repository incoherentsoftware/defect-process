module World.Audio.LayeredMusic.Types
    ( LayeredMusicType(..)
    , LayerIndex(..)
    , LayeredMusic(..)
    ) where

import Data.Aeson.Types (FromJSON, ToJSON)
import GHC.Generics     (Generic)
import qualified Data.Text as T
import qualified Data.Vector as V

import Audio.Fmod.Types
import Util

data LayeredMusicType
    = BattleAMusic
    | ExploreAMusic
    | BattleBMusic
    | ExploreBMusic
    | BattleCMusic
    | ExploreCMusic
    deriving (Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance PrettyShow LayeredMusicType where
    prettyShow :: LayeredMusicType -> T.Text
    prettyShow = \case
        BattleAMusic  -> "Battle Music - Track A"
        ExploreAMusic -> "Ambient Music - Track A"
        BattleBMusic  -> "Battle Music - Track B"
        ExploreBMusic -> "Ambient Music - Track B"
        BattleCMusic  -> "Battle Music - Track C"
        ExploreCMusic -> "Ambient Music - Track C"

newtype LayerIndex = LayerIndex {_int :: Int}
    deriving (Eq, Ord, Show)
    deriving newtype Num

data LayeredMusic = LayeredMusic
    { _type       :: LayeredMusicType
    , _layers     :: V.Vector FmodMusicIndex
    , _layerIndex :: LayerIndex
    }
