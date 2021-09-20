module Menu.SoundIndices
    ( module Menu.SoundIndices.Types
    , mkMenuSoundIndices
    ) where

import Control.Monad.IO.Class (MonadIO)

import Audio.Fmod
import Menu.SoundIndices.Types

confirmSoundPath         = "event:/SFX Events/UI/confirm"           :: FilePath
confirmSmallSoundPath    = "event:/SFX Events/UI/confirm-small"     :: FilePath
confirmAltSoundPath      = "event:/SFX Events/UI/confirm-alt"       :: FilePath
confirmRebindSoundPath   = "event:/SFX Events/UI/confirm-rebind"    :: FilePath
unlockSoundPath          = "event:/SFX Events/UI/unlock"            :: FilePath
cantUnlockSoundPath      = "event:/SFX Events/UI/cant-unlock"       :: FilePath
anyKeySoundPath          = "event:/SFX Events/UI/any-key"           :: FilePath
skillSlotSelectSoundPath = "event:/SFX Events/UI/skill-slot-select" :: FilePath

mkMenuSoundIndices :: MonadIO m => m MenuSoundIndices
mkMenuSoundIndices =
    MenuSoundIndices <$>
    getFmodSoundIndex confirmSoundPath <*>
    getFmodSoundIndex confirmSmallSoundPath <*>
    getFmodSoundIndex confirmAltSoundPath <*>
    getFmodSoundIndex confirmRebindSoundPath <*>
    getFmodSoundIndex unlockSoundPath <*>
    getFmodSoundIndex cantUnlockSoundPath <*>
    getFmodSoundIndex anyKeySoundPath <*>
    getFmodSoundIndex skillSlotSelectSoundPath
