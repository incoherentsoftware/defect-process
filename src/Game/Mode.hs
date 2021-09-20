module Game.Mode
    ( GameMode(..)
    ) where

import Data.Aeson.Types (FromJSON)
import GHC.Generics     (Generic)

data GameMode
    = WorldMode
    | PauseMenuMode
    | MainMenuMode
    | UnlocksMenuMode
    deriving (Eq, FromJSON, Generic, Read)
