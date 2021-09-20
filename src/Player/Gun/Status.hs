module Player.Gun.Status
    ( Shootable(..)
    , GunStatus(..)
    , isGunStatusActive
    ) where

data Shootable
    = Shootable
    | NotShootable
    deriving Eq

data GunStatus
    = ActiveStatus Shootable
    | InactiveStatus
    deriving Eq

isGunStatusActive :: GunStatus -> Bool
isGunStatusActive = \case
    ActiveStatus _ -> True
    InactiveStatus -> False
