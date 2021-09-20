module Player.Gun.Types
    ( GunType
    ) where

import Data.Aeson.Types (FromJSON)

data GunType
instance FromJSON GunType
instance Ord GunType
