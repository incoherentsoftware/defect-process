module Audio.Fmod.Types
    ( FmodSoundIndex(..)
    , mkFmodSoundIndex
    , FmodMusicIndex(..)
    , mkFmodMusicIndex
    ) where

data FmodSoundIndex
    = FmodSoundIndexInvalid
    | FmodSoundIndex Int
    deriving (Eq, Show)

mkFmodSoundIndex :: Int -> FmodSoundIndex
mkFmodSoundIndex idx
    | idx < 0   = FmodSoundIndexInvalid
    | otherwise = FmodSoundIndex idx

data FmodMusicIndex
    = FmodMusicIndexInvalid
    | FmodMusicIndex Int
    deriving Eq

mkFmodMusicIndex :: Int -> FmodMusicIndex
mkFmodMusicIndex idx
    | idx < 0   = FmodMusicIndexInvalid
    | otherwise = FmodMusicIndex idx
