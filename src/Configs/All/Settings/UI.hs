module Configs.All.Settings.UI
    ( UiConfig(..)
    ) where

import Data.Aeson.Types (FromJSON, genericParseJSON, parseJSON)
import GHC.Generics     (Generic)
import qualified Data.Vector as V

import Util
import Window.Graphics.Opacity
import Window.Graphics.Util

data UiConfig = UiConfig
    { _overlayScale                    :: DrawScale
    , _crosshairCursorHotspotPos       :: Pos2
    , _overlayPos                      :: Pos2
    , _healthbarInnerOffset            :: Pos2
    , _healthbarDamageFadeMultiplier   :: Float
    , _meterOffset                     :: Pos2
    , _meterUnitRelativeOffsets        :: [Pos2]
    , _inactiveIconOpacity             :: Opacity
    , _inactiveIconRelativeScale       :: Float
    , _weaponActiveIconOffset          :: Pos2
    , _weaponInactiveIconOffset        :: Pos2
    , _gunActiveIconOffset             :: Pos2
    , _gunInactiveIconOffset           :: Pos2
    , _movementSkillIconOffset         :: Pos2
    , _secondarySkillNeutralIconOffset :: Pos2
    , _secondarySkillUpIconOffset      :: Pos2
    , _secondarySkillDownIconOffset    :: Pos2
    , _doubleJumpIconOffset            :: Pos2
    , _goldBackdropHeight              :: Float
    , _goldBackdropBorderWidth         :: Float
    , _goldRightPos                    :: Pos2
    , _goldFloatingTextOffset          :: Pos2
    , _goldFloatingTextFadeMultiplier  :: Float
    , _goldFloatingTextVelY            :: VelY
    , _goldFloatingTextIntervalSecs    :: Secs
    , _infoTextPosY                    :: PosY
    , _infoTextIntervalOffsetY         :: PosY
    , _usageBlipRelativeOffsets        :: V.Vector [Pos2]
    }
    deriving Generic

instance FromJSON UiConfig where
    parseJSON value = genericParseJSON aesonFieldDropUnderscore value
