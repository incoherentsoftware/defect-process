module Level.Room.Item.Pickup.Types
    ( ItemPickupIsBuyConfirmOnInteract
    , ItemPickupBuyConfirmStartMessages
    , ItemPickupThinkBuyConfirm
    , ItemPickupUpdateBuyConfirm
    , ItemPickupDrawBuyConfirm
    , ItemPickupBuyConfirmData(..)
    , ItemPickupStatus(..)
    , ItemPickupData(..)
    ) where

import qualified Data.Text as T

import AppEnv
import Level.Room.Item.Types
import Level.Room.Types
import Msg.Payload
import Msg.Phase
import Msg.Types
import Window.Graphics
import World.Util

type ItemPickupIsBuyConfirmOnInteract m  = m Bool
type ItemPickupBuyConfirmStartMessages m = RoomItem ItemPickupData -> m [Msg ThinkLevelMsgsPhase]
type ItemPickupThinkBuyConfirm m         = RoomItem ItemPickupData -> m [Msg ThinkLevelMsgsPhase]
type ItemPickupUpdateBuyConfirm m        = ItemPickupData -> m ItemPickupBuyConfirmData
type ItemPickupDrawBuyConfirm m          = RoomItem ItemPickupData -> m ()

data ItemPickupBuyConfirmData = ItemPickupBuyConfirmData
    { _selectDisplayText                :: DisplayText
    , _upAliasInputDisplayText          :: InputDisplayText
    , _downAliasInputDisplayText        :: InputDisplayText
    , _confirmDisplayText               :: DisplayText
    , _interactAliasInputDisplayText    :: InputDisplayText
    , _selectedLineIndex                :: Int
    , _replace0DisplayText              :: DisplayText
    , _replace1DisplayText              :: DisplayText
    , _replace2DisplayText              :: DisplayText
    , _line0DisplayText                 :: DisplayText
    , _line1DisplayText                 :: DisplayText
    , _line1SelectedDisplayText         :: DisplayText
    , _line2DisplayText                 :: DisplayText
    , _line2SelectedDisplayText         :: DisplayText
    , _line3DisplayText                 :: DisplayText
    , _line3SelectedDisplayText         :: DisplayText
    , _line4DisplayText                 :: DisplayText
    , _slotsOverlayNeutralSelectedImage :: Image
    , _slotsOverlayUpSelectedImage      :: Image
    , _slotsOverlayDownSelectedImage    :: Image
    , _literalEmptyText                 :: DisplayText
    }

data ItemPickupStatus
    = ItemPickupNormalStatus
    | ItemPickupIndicatorStatus Sprite
    | ItemPickupReappearStatus Sprite
    | ItemPickupBuyConfirmStatus
    deriving Eq

data ItemPickupData = ItemPickupData
    { _name                    :: T.Text
    , _buyMsgPayload           :: PlayerMsgPayload
    , _cost                    :: GoldValue
    , _touchingPlayer          :: Bool
    , _roomType                :: RoomType
    , _image                   :: Image
    , _costInputDisplayText    :: InputDisplayText
    , _buyInfoInputDisplayText :: InputDisplayText
    , _status                  :: ItemPickupStatus
    , _buyConfirmData          :: ItemPickupBuyConfirmData
    , _isBuyConfirmOnInteract  :: ItemPickupIsBuyConfirmOnInteract (AppEnv ThinkLevelMsgsPhase)
    , _buyConfirmStartMessages :: ItemPickupBuyConfirmStartMessages (AppEnv ThinkLevelMsgsPhase)
    , _thinkBuyConfirm         :: ItemPickupThinkBuyConfirm (AppEnv ThinkLevelMsgsPhase)
    , _updateBuyConfirm        :: ItemPickupUpdateBuyConfirm (AppEnv UpdateLevelMsgsPhase)
    , _drawBuyConfirmOverlay   :: ItemPickupDrawBuyConfirm (AppEnv DrawMsgsPhase)
    }
