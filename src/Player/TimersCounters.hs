module Player.TimersCounters
    ( PlayerTimersCounters(..)
    , mkPlayerTimersCounters
    , updatePlayerTimersCounters
    ) where

import Constants
import Player.Flags
import Util

fallOffSpeedRailCooldown = 0.1        :: Secs
graceJumpWindowSecs      = 5.0 / 60.0 :: Secs

data PlayerTimersCounters = PlayerTimersCounters
    { _airStallAttacksCounter :: Int
    , _doubleJumpCounter      :: Int
    , _graceJumpTtl           :: Secs
    , _noSpeedRailTtl         :: Secs
    , _hurtInvincibleTtl      :: Secs
    }

mkPlayerTimersCounters :: PlayerTimersCounters
mkPlayerTimersCounters = PlayerTimersCounters
    { _airStallAttacksCounter = 0
    , _doubleJumpCounter      = 0
    , _graceJumpTtl           = graceJumpWindowSecs
    , _noSpeedRailTtl         = 0.0
    , _hurtInvincibleTtl      = 0.0
    }

updatePlayerTimersCounters :: PlayerFlags -> PlayerTimersCounters -> PlayerTimersCounters
updatePlayerTimersCounters flags timersCounters = timersCounters
    { _graceJumpTtl      = graceJumpTtl
    , _noSpeedRailTtl    = noSpeedRailTtl
    , _hurtInvincibleTtl = hurtInvincibleTtl
    }
    where
        graceJumpTtl
            | _touchingGround flags = graceJumpWindowSecs
            | _jumped flags         = 0.0
            | otherwise             = max 0.0 (_graceJumpTtl timersCounters - timeStep)

        noSpeedRailTtl
            | _onSpeedRail flags && _willFallOffGround flags = fallOffSpeedRailCooldown
            | otherwise                                      = max 0.0 (_noSpeedRailTtl timersCounters - timeStep)

        hurtInvincibleTtl
            | _phased flags = 0.0
            | otherwise     = max 0.0 (_hurtInvincibleTtl timersCounters - timeStep)
