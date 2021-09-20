module World.Screenshake.Types
    ( ScreenshakeMagnitude(..)
    , zeroScreenshakeMagnitude
    , ScreenshakeType(..)
    , Screenshake(..)
    ) where

import Data.Aeson.Types (FromJSON)

import Util
import Window.Graphics.Util

newtype ScreenshakeMagnitude = ScreenshakeMagnitude {_float :: Float}
    deriving (Eq, Ord)
    deriving newtype (Fractional, FromJSON, Num)

zeroScreenshakeMagnitude = ScreenshakeMagnitude 0.0 :: ScreenshakeMagnitude

data ScreenshakeType
    = NoScreenshake
    | ScreenshakeOnFrame FrameIndex ScreenshakeMagnitude
    | ScreenshakeOnHit ScreenshakeMagnitude

data Screenshake = Screenshake
    { _magnitude    :: ScreenshakeMagnitude
    , _pulseTrigger :: Secs
    }
