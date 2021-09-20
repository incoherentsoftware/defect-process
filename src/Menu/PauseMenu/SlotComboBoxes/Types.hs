module Menu.PauseMenu.SlotComboBoxes.Types
    ( PauseMenuSlotComboBoxes(..)
    ) where

import Window.Graphics.UiControls

data PauseMenuSlotComboBoxes = PauseMenuSlotComboBoxes
    { _neutralSlot :: ComboBox
    , _upSlot      :: ComboBox
    , _downSlot    :: ComboBox
    }
