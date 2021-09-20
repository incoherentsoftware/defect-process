module World.UI.Voiceover.Types
    ( VoiceoverOpacity(..)
    , VoiceoverUI(..)
    ) where

import Util
import Window.Graphics

data VoiceoverOpacity
    = VoiceoverAppearOpacity Float
    | VoiceoverLingerOpacity Secs
    | VoiceoverDisappearOpacity Float

data VoiceoverUI = VoiceoverUI
    { _deathDisplayText :: DisplayText
    , _displayText      :: Maybe DisplayText
    , _opacity          :: VoiceoverOpacity
    }
