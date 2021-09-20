module Msg
    ( module Msg.Payload
    , module Msg.Types
    , mkMsg
    , mkMsgEx
    , mkMsgTo
    , mkMsgToEx
    , readMsgs
    ) where

import Id
import Msg.Payload
import Msg.Types

mkMsg :: AllowMsgWrite p a => a -> Msg p
mkMsg payload = mkMsgEx payload MsgNormalOrder

mkMsgEx :: AllowMsgWrite p a => a -> MsgOrder -> Msg p
mkMsgEx payload msgOrder = mkMsgToEx payload NullId msgOrder

mkMsgTo :: AllowMsgWrite p a => a -> MsgId -> Msg p
mkMsgTo payload msgTo = mkMsgToEx payload msgTo MsgNormalOrder

mkMsgToEx :: AllowMsgWrite p a => a -> MsgId -> MsgOrder -> Msg p
mkMsgToEx payload msgTo msgOrder = Msg $ MsgInternal
    { _payload = toMsgPayload payload
    , _to      = msgTo
    , _order   = msgOrder
    }

readMsgs :: forall p m a. (MsgsRead p m, AllowMsgRead p a) => m [a]
readMsgs = readMsgsTo @p NullId
