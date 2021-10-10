module Stats.Manager
    ( StatsManager(..)
    , mkStatsManager
    , updateStatsManager
    ) where

import qualified Data.List as L

import Game.Mode
import Msg
import World.Util

data StatsManager = StatsManager
    { _acquiredGold      :: GoldValue
    , _numBoughtHealth   :: Int
    , _numPauseMenuViews :: Int
    }

mkStatsManager :: StatsManager
mkStatsManager = StatsManager
    { _acquiredGold      = GoldValue 0
    , _numBoughtHealth   = 0
    , _numPauseMenuViews = 0
    }

processMessages :: MsgsReadWrite UpdateStatsManagerMsgsPhase m => StatsManager -> m StatsManager
processMessages statsManager =
    let
        processPlayerMsg :: StatsManager -> PlayerMsgPayload -> StatsManager
        processPlayerMsg !sm p = case p of
            PlayerMsgTouchingGold gold -> sm {_acquiredGold = _acquiredGold sm + gold}
            PlayerMsgBuyHealth _       -> sm {_numBoughtHealth = _numBoughtHealth sm + 1}
            _                          -> sm

        processWorldMsgs :: MsgsWrite UpdateStatsManagerMsgsPhase m => StatsManager -> [WorldMsgPayload] -> m ()
        processWorldMsgs _ []      = return ()
        processWorldMsgs sm (p:ps) = case p of
            WorldMsgDeactivate -> writeMsgs
                [ mkMsg $ ConsoleMsgAddProgressTotalGold (_acquiredGold sm)
                , mkMsg ConsoleMsgSaveProgress
                ]
            _                  -> processWorldMsgs sm ps
    in do
        statsManager' <- L.foldl' processPlayerMsg statsManager <$> readMsgs
        processWorldMsgs statsManager' =<< readMsgs
        return statsManager'

updateStatsManager :: MsgsReadWrite UpdateStatsManagerMsgsPhase m => GameMode -> StatsManager -> m StatsManager
updateStatsManager prevGameMode statsManager = do
    statsManager' <- processMessages statsManager
    return $ statsManager'
        { _numPauseMenuViews =
            let numPauseMenuViews = _numPauseMenuViews statsManager
            in if
                | prevGameMode == PauseMenuMode -> numPauseMenuViews + 1
                | otherwise                     -> numPauseMenuViews
        }
