module Msg.Types
    ( Msg
    ) where

data MsgInternal

newtype Msg p = Msg MsgInternal
