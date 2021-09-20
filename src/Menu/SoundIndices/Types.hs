module Menu.SoundIndices.Types
    ( MenuSoundIndices(..)
    ) where

import Audio.Fmod.Types

data MenuSoundIndices = MenuSoundIndices
    { _confirm         :: FmodSoundIndex
    , _confirmSmall    :: FmodSoundIndex
    , _confirmAlt      :: FmodSoundIndex
    , _confirmRebind   :: FmodSoundIndex
    , _unlock          :: FmodSoundIndex
    , _cantUnlock      :: FmodSoundIndex
    , _anyKey          :: FmodSoundIndex
    , _skillSlotSelect :: FmodSoundIndex
    }
