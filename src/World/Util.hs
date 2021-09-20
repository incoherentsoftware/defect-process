module World.Util
    ( goldTextColor
    , GoldValue(..)
    ) where

import GHC.Generics     (Generic)
import Data.Aeson.Types (FromJSON, ToJSON)

import Util
import Window.Graphics.Color

goldTextColor = Color 255 243 137 255 :: Color

newtype GoldValue = GoldValue
    { _int :: Int
    }
    deriving (Enum, Eq, Generic, Show)
    deriving PrettyShow via Int
    deriving newtype (FromJSON, Integral, Num, Ord, Real, ToJSON)
