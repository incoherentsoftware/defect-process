module Level.Room.Trigger.All.TutorialListener
    ( mkTutorialListenerTrigger
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.List as L

import Id
import InfoMsg.Util
import Level.Room.Trigger
import Level.Room.Trigger.All.TutorialListener.IndicatorFakeProjectile
import Level.Room.Trigger.Util
import Msg

data IndicatorStatus
    = IndicatorNotShownStatus
    | IndicatorIsShowingStatus
    | IndicatorAlreadyShownStatus
    deriving Eq

data ListenerData = ListenerData
    { _indicatorStatus     :: IndicatorStatus
    , _indicatorFakeProjId :: MsgId
    }

mkListenerData :: MonadIO m => m ListenerData
mkListenerData = ListenerData IndicatorNotShownStatus <$> newId

mkTutorialListenerTrigger :: MonadIO m => m RoomTrigger
mkTutorialListenerTrigger = do
    trigger      <- mkRoomTrigger
    listenerData <- mkListenerData
    return $ (trigger :: RoomTrigger) {_think = think listenerData}

think :: MsgsRead ThinkLevelMsgsPhase m => ListenerData -> RoomTriggerThink m
think listenerData _ trigger =
    let
        indicatorStatus = _indicatorStatus listenerData
        indicatorId     = _indicatorFakeProjId listenerData

        processUiMsg :: [Msg ThinkLevelMsgsPhase] -> UiMsgPayload -> [Msg ThinkLevelMsgsPhase]
        processUiMsg !ms d = case d of
            UiMsgInsufficientMeter _ _ ->
                let
                    indicatorMsgs = case indicatorStatus of
                        IndicatorNotShownStatus ->
                            [mkMsg $ NewThinkProjectileMsgAddM (mkIndicatorFakeProjectile indicatorId)]
                        _                       -> []

                    updateTriggerMsg =
                        let
                            listenerData' = case indicatorStatus of
                                IndicatorNotShownStatus -> listenerData {_indicatorStatus = IndicatorIsShowingStatus}
                                _                       -> listenerData
                        in updateTriggerThinkMessage (think listenerData') trigger
                in updateTriggerMsg:indicatorMsgs ++ ms

            _ -> ms

        processInfoMsgs :: [InfoMsgPayload] -> [Msg ThinkLevelMsgsPhase]
        processInfoMsgs []     = []
        processInfoMsgs (p:ps) = case p of
            InfoMsgPlayer playerInfo
                | isPlayerInfoMeterFull playerInfo && indicatorStatus == IndicatorIsShowingStatus ->
                    let listenerData' = listenerData {_indicatorStatus = IndicatorAlreadyShownStatus}
                    in
                        [ updateTriggerThinkMessage (think listenerData') trigger
                        , mkMsgTo (ProjectileMsgSetTtl 0.0) indicatorId
                        ]

            _ -> processInfoMsgs ps
    in do
        uiMsgs   <- L.foldl' processUiMsg [] <$> readMsgs
        infoMsgs <- processInfoMsgs <$> readMsgs
        return $ uiMsgs ++ infoMsgs
