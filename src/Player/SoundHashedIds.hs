module Player.SoundHashedIds
    ( PlayerSoundHashedIds(..)
    , mkPlayerSoundHashedIds
    ) where

import Control.Monad.IO.Class (MonadIO)

import Id

data PlayerSoundHashedIds = PlayerSoundHashedIds
    { _grind :: HashedId
    }

mkPlayerSoundHashedIds :: MonadIO m => m PlayerSoundHashedIds
mkPlayerSoundHashedIds =
    PlayerSoundHashedIds <$>
    (hashId <$> newId)
