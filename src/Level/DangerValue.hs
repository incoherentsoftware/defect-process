module Level.DangerValue
    ( DangerValue(..)
    ) where

import Data.Aeson.Types (FromJSON)

import Util

newtype DangerValue = DangerValue Int
    deriving (Eq, Ord, Show)
    deriving anyclass PrettyShow
    deriving newtype (FromJSON, Num)
