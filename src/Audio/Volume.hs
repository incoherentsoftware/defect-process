module Audio.Volume
    ( Volume
    , mkVolume
    , volumeToInt
    , volumeToFloat
    ) where

import Data.Aeson.Types (FromJSON, ToJSON)
import GHC.Generics     (Generic)

import Util

minValue = 0   :: Int
maxValue = 100 :: Int

newtype Volume = Volume Int
    deriving (Generic, Show)
    deriving anyclass PrettyShow
    deriving newtype (FromJSON, ToJSON)

mkVolume :: Int -> Volume
mkVolume val
    | val < minValue = Volume minValue
    | val > maxValue = Volume maxValue
    | otherwise      = Volume val

volumeToInt :: Volume -> Int
volumeToInt (Volume val) = val

volumeToFloat :: Volume -> Float
volumeToFloat (Volume val) = realToFrac val / realToFrac maxValue
