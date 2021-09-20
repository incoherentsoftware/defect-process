module Player.Upgrade.Manager
    ( maxMeterUpgradeCount
    , PlayerUpgradeManager(..)
    , mkPlayerUpgradeManager
    , playerUpgradeManagerCount
    , givePlayerUpgradeManagerUpgrade
    ) where

import qualified Data.Map as M

import Player.Upgrade

maxMeterUpgradeCount = 5 :: Int

data PlayerUpgradeManager = PlayerUpgradeManager
    { _counts :: M.Map PlayerUpgradeType Int
    }

mkPlayerUpgradeManager :: PlayerUpgradeManager
mkPlayerUpgradeManager = PlayerUpgradeManager
    { _counts = M.empty
    }

playerUpgradeManagerCount :: PlayerUpgradeType -> PlayerUpgradeManager -> Int
playerUpgradeManagerCount upgradeType upgradeManager = M.findWithDefault 0 upgradeType (_counts upgradeManager)

givePlayerUpgradeManagerUpgrade :: PlayerUpgradeType -> PlayerUpgradeManager -> PlayerUpgradeManager
givePlayerUpgradeManagerUpgrade upgradeType upgradeManager
    | upgradeType == MeterUpgradeType && meterUpgradeCount >= maxMeterUpgradeCount = upgradeManager
    | otherwise                                                                    = upgradeManager
        { _counts = M.insertWith (+) upgradeType 1 (_counts upgradeManager)
        }
    where meterUpgradeCount = playerUpgradeManagerCount MeterUpgradeType upgradeManager
