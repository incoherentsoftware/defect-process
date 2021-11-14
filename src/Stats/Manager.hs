module Stats.Manager
    ( StatsManager(..)
    , mkStatsManager
    , updateStatsManager
    ) where

import Control.Monad.State (execStateT, get, lift, modify)
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
            PlayerMsgBuyHealth _ -> sm {_numBoughtHealth = _numBoughtHealth sm + 1}
            _                    -> sm

        processUiMsg :: StatsManager -> UiMsgPayload -> StatsManager
        processUiMsg !sm p = case p of
            UiMsgGainedGold goldValue
                | goldValue > GoldValue 0 -> sm {_acquiredGold = goldValue + _acquiredGold sm}
            _                             -> sm

        processWorldMsgs :: MsgsWrite UpdateStatsManagerMsgsPhase m => StatsManager -> [WorldMsgPayload] -> m ()
        processWorldMsgs _ []      = return ()
        processWorldMsgs sm (p:ps) = case p of
            WorldMsgDeactivate -> writeMsgs
                [ mkMsg $ ConsoleMsgAddProgressTotalGold (_acquiredGold sm)
                , mkMsg ConsoleMsgSaveProgress
                ]
            _                  -> processWorldMsgs sm ps
    in flip execStateT statsManager $ do
        playerMsgs <- lift readMsgs
        modify $ \sm -> L.foldl' processPlayerMsg sm playerMsgs

        uiMsgs <- lift readMsgs
        modify $ \sm -> L.foldl' processUiMsg sm uiMsgs

        worldMsgs <- lift readMsgs
        get >>= \sm -> lift $ processWorldMsgs sm worldMsgs

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
