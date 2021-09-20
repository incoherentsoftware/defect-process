module World.UI.Types
    ( WorldUI(..)
    ) where

import World.UI.Gold
import World.UI.Health
import World.UI.Icons
import World.UI.InfoText
import World.UI.Meter
import World.UI.Voiceover

data WorldUI = WorldUI
    { _healthUI    :: HealthUI
    , _meterUI     :: MeterUI
    , _iconsUI     :: IconsUI
    , _goldUI      :: GoldUI
    , _infoTextUI  :: InfoTextUI
    , _voiceoverUI :: VoiceoverUI
    }
