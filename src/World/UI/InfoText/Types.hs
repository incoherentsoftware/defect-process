module World.UI.InfoText.Types
    ( SecondarySkillInfoTexts(..)
    , InfoTextType(..)
    , InfoTextUI(..)
    ) where

import Util
import Window.Graphics

data SecondarySkillInfoTexts = SecondarySkillInfoTexts
    { _neutral :: DisplayText
    , _up      :: DisplayText
    , _down    :: DisplayText
    }

data InfoTextType
    = NoInfoTextType
    | MoveControlsInfoTextType InputDisplayText InputDisplayText
    | EquipmentInfoTextType Secs InputDisplayText (Maybe InputDisplayText)
    | SecondarySkillInfoTextType Secs InputDisplayText SecondarySkillInfoTexts

data InfoTextUI = InfoTextUI
    { _infoTextType                      :: InfoTextType
    , _moveControlsLeftInputDisplayText  :: InputDisplayText
    , _moveControlsRightInputDisplayText :: InputDisplayText
    , _equipmentInfoInputDisplayText     :: InputDisplayText
    , _switchWeaponInputDisplayText      :: InputDisplayText
    , _switchGunInputDisplayText         :: InputDisplayText
    , _secondarySkillLiteralEmptyText    :: DisplayText
    , _secondarySkillSymbolDisplayText   :: SymbolDisplayText
    , _secondarySkillInputDisplayText    :: InputDisplayText
    , _secondarySkillOverlayImage        :: Image
    }
