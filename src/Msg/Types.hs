module Msg.Types
    ( module Msg.Phase
    , MsgId
    , MsgOrder(..)
    , MsgInternal(..)
    , Msg(..)
    , MsgsRead(..)
    , MsgsWrite(..)
    , MsgsReadWrite
    ) where

import Id
import Msg.Phase
import {-# SOURCE #-} Msg.Payload

type MsgId = Id MsgInternal

data MsgOrder
    = MsgFrontOrder
    | MsgNormalOrder
    | MsgAfterNormalOrder
    | MsgEndOrder
    deriving (Eq, Ord)

data MsgInternal = MsgInternal
    { _payload :: MsgPayload
    , _to      :: MsgId
    , _order   :: MsgOrder
    }

newtype Msg p = Msg MsgInternal

class Monad m => MsgsRead p m | m -> p where
    readMsgsTo :: AllowMsgRead p a => MsgId -> m [a]

class Monad m => MsgsWrite p m | m -> p where
    writeMsgs :: [Msg p] -> m ()

type MsgsReadWrite p m = (MsgsRead p m, MsgsWrite p m)
