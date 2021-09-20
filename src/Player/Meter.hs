module Player.Meter
    ( MeterValue(..)
    , PlayerMeter(..)
    , mkPlayerMeter
    , playerMeterValue
    , gainPlayerMeter
    , spendPlayerMeter
    , setPlayerMeterUpgradeCount
    , fillPlayerMeterFull
    ) where

import Data.Aeson.Types (FromJSON)

newtype MeterValue = MeterValue
    { _int :: Int
    }
    deriving (Eq, Ord)
    deriving newtype (FromJSON, Num)

minMeterValue        = MeterValue 0  :: MeterValue
defaultMaxMeterValue = MeterValue 10 :: MeterValue

data PlayerMeter = PlayerMeter
    { _value    :: MeterValue
    , _maxValue :: MeterValue
    }

mkPlayerMeter :: PlayerMeter
mkPlayerMeter = PlayerMeter
    { _value    = defaultMaxMeterValue
    , _maxValue = defaultMaxMeterValue
    }

playerMeterValue :: PlayerMeter -> MeterValue
playerMeterValue = _value

gainPlayerMeter :: MeterValue -> PlayerMeter -> PlayerMeter
gainPlayerMeter meterVal playerMeter
    | meterVal <= minMeterValue = playerMeter
    | otherwise                 =
        let meter = min (_maxValue playerMeter) (_value playerMeter + meterVal)
        in playerMeter {_value = meter}

spendPlayerMeter :: MeterValue -> PlayerMeter -> PlayerMeter
spendPlayerMeter meterVal playerMeter
    | meterVal <= minMeterValue = playerMeter
    | otherwise                 =
        let meter = max minMeterValue (_value playerMeter - meterVal)
        in playerMeter {_value = meter}

setPlayerMeterUpgradeCount :: Int -> PlayerMeter -> PlayerMeter
setPlayerMeterUpgradeCount upgradeCount playerMeter = playerMeter
    { _value    = value
    , _maxValue = maxMeterVal
    }
    where
        maxMeterVal = max minMeterValue (defaultMaxMeterValue + MeterValue upgradeCount)
        value       = if
            | maxMeterVal > _maxValue playerMeter -> maxMeterVal
            | otherwise                           -> min (_value playerMeter) maxMeterVal

fillPlayerMeterFull :: PlayerMeter -> PlayerMeter
fillPlayerMeterFull playerMeter = playerMeter {_value = _maxValue playerMeter}
