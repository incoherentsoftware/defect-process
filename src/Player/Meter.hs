module Player.Meter
    ( MeterValue(..)
    , PlayerMeter(..)
    , mkPlayerMeter
    , playerMeterValue
    , gainPlayerMeter
    , spendPlayerMeter
    , setPlayerMeterUpgradeCount
    , fillPlayerMeterFull
    , isPlayerMeterFull
    ) where

import Data.Aeson.Types (FromJSON)
import qualified Data.Set as S

import Id
import {-# SOURCE #-} Attack.Types

newtype MeterValue = MeterValue
    { _int :: Int
    }
    deriving (Eq, Ord)
    deriving newtype (FromJSON, Num)

minMeterValue        = MeterValue 0  :: MeterValue
defaultMaxMeterValue = MeterValue 10 :: MeterValue

data PlayerMeter = PlayerMeter
    { _value        :: MeterValue
    , _maxValue     :: MeterValue
    , _gainMeterIds :: S.Set (Id Attack)
    }

mkPlayerMeter :: PlayerMeter
mkPlayerMeter = PlayerMeter
    { _value        = defaultMaxMeterValue
    , _maxValue     = defaultMaxMeterValue
    , _gainMeterIds = S.empty
    }

playerMeterValue :: PlayerMeter -> MeterValue
playerMeterValue = _value

gainPlayerMeter :: Id Attack -> MeterValue -> PlayerMeter -> PlayerMeter
gainPlayerMeter meterId meterVal playerMeter
    | meterId /= NullId && meterId `S.member` gainMeterIds = playerMeter
    | meterVal <= minMeterValue                            = playerMeter
    | otherwise                                            = playerMeter
        { _value        = min (_maxValue playerMeter) (_value playerMeter + meterVal)
        , _gainMeterIds = meterId `S.insert` gainMeterIds
        }
    where gainMeterIds = _gainMeterIds playerMeter

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

isPlayerMeterFull :: PlayerMeter -> Bool
isPlayerMeterFull playerMeter = _value playerMeter >= _maxValue playerMeter
