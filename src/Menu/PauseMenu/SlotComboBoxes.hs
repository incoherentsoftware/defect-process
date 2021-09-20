module Menu.PauseMenu.SlotComboBoxes
    ( neutralSlotText
    , upSlotText
    , downSlotText
    , mkSlotComboBoxes
    , updateSlotComboBoxes
    , isSlotComboBoxesExpanded
    , isSlotComboBoxesPressed
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import qualified SDL

import FileCache
import Menu.PauseMenu.SlotComboBoxes.Types
import Menu.PauseMenu.Types
import Menu.Util
import Util
import Window
import Window.Graphics.UiControls

neutralSlotText = "Neutral Input" :: T.Text
upSlotText      = "Up Input"      :: T.Text
downSlotText    = "Down Input"    :: T.Text

slotComboBoxValueOffset = Pos2 3.0 27.0     :: Pos2
neutralSlotComboBoxPos  = Pos2 850.0 636.0  :: Pos2
upSlotComboBoxPos       = Pos2 968.0 636.0  :: Pos2
downSlotComboBoxPos     = Pos2 1086.0 636.0 :: Pos2

secondarySkillSlotComboBoxImagePath =
    PackResourceFilePath "data/menu/pause-menu.pack" "secondary-skill-slot-combo-box.image" :: PackResourceFilePath

mkSlotComboBoxes :: (FileCache m, GraphicsRead m, MonadIO m) => m PauseMenuSlotComboBoxes
mkSlotComboBoxes =
    let
        mkComboBox' = \pos defaultTxt -> mkComboBox
            pos
            defaultTxt
            [neutralSlotText, upSlotText, downSlotText]
            Font16
            menuOptionBtnColor
            hoverMenuOptionBtnColor
            slotComboBoxValueOffset
            secondarySkillSlotComboBoxImagePath
    in do
        neutralComboBox <- mkComboBox' neutralSlotComboBoxPos neutralSlotText
        upComboBox      <- mkComboBox' upSlotComboBoxPos upSlotText
        downComboBox    <- mkComboBox' downSlotComboBoxPos downSlotText

        return $ PauseMenuSlotComboBoxes
            { _neutralSlot = neutralComboBox
            , _upSlot      = upComboBox
            , _downSlot    = downComboBox
            }

updateSlotComboBoxes
    :: InputRead m
    => PauseMenuSelection
    -> Bool
    -> Bool
    -> Bool
    -> PauseMenuSlotComboBoxes
    -> m PauseMenuSlotComboBoxes
updateSlotComboBoxes selection isNeutralHelpEntry isUpHelpEntry isDownHelpEntry comboBoxes = do
    inputState           <- readInputState
    let changeSlotPressed = MenuSlotChangeAlias `aliasPressed` inputState

    comboBoxes' <-
        let
            neutralComboBox = _neutralSlot comboBoxes
            upComboBox      = _upSlot comboBoxes
            downComboBox    = _downSlot comboBoxes
        in do
            isNeutralComboBoxHover <- (isNeutralHelpEntry &&) <$> isComboBoxCursorHover neutralComboBox
            isUpComboBoxHover      <- (isUpHelpEntry &&) <$> isComboBoxCursorHover upComboBox
            isDownComboBoxHover    <- (isDownHelpEntry &&) <$> isComboBoxCursorHover downComboBox

            return $ case selection of
                PauseMenuSecondarySkillLeftSelection
                    | changeSlotPressed && not (_isExpanded neutralComboBox) ->
                        comboBoxes {_neutralSlot = neutralComboBox {_isExpanded = True}}
                    | changeSlotPressed && isUpComboBoxHover                 ->
                        comboBoxes {_upSlot = upComboBox {_isExpanded = True}}
                    | changeSlotPressed && isDownComboBoxHover               ->
                        comboBoxes {_downSlot = downComboBox {_isExpanded = True}}

                PauseMenuSecondarySkillMidSelection
                    | changeSlotPressed && not (_isExpanded upComboBox) ->
                        comboBoxes {_upSlot = upComboBox {_isExpanded = True}}
                    | changeSlotPressed && isNeutralComboBoxHover       ->
                        comboBoxes {_neutralSlot = neutralComboBox {_isExpanded = True}}
                    | changeSlotPressed && isDownComboBoxHover          ->
                        comboBoxes {_downSlot = downComboBox {_isExpanded = True}}

                PauseMenuSecondarySkillRightSelection
                    | changeSlotPressed && not (_isExpanded downComboBox) ->
                        comboBoxes {_downSlot = downComboBox {_isExpanded = True}}
                    | changeSlotPressed && isNeutralComboBoxHover         ->
                        comboBoxes {_neutralSlot = neutralComboBox {_isExpanded = True}}
                    | changeSlotPressed && isUpComboBoxHover              ->
                        comboBoxes {_upSlot = upComboBox {_isExpanded = True}}

                _ -> comboBoxes

    let
        updateComboBoxEx' = \defaultTxt slotF ->
            let
                prevComboBox = slotF comboBoxes
                comboBox     = slotF comboBoxes'
            in if
                | _isExpanded prevComboBox && _isExpanded comboBox ->
                    let isClick = changeSlotPressed || SDL.ButtonLeft `mousePressed` inputState
                    in updateComboBoxEx ComboBoxActiveStatus defaultTxt isClick comboBox
                | otherwise                                        ->
                    updateComboBox ComboBoxInactiveStatus defaultTxt comboBox

    neutralComboBox <- updateComboBoxEx' neutralSlotText _neutralSlot
    upComboBox      <- updateComboBoxEx' upSlotText _upSlot
    downComboBox    <- updateComboBoxEx' downSlotText _downSlot

    return $ comboBoxes'
        { _neutralSlot = neutralComboBox
        , _upSlot      = upComboBox
        , _downSlot    = downComboBox
        }

isSlotComboBoxesExpanded :: PauseMenuSlotComboBoxes -> Bool
isSlotComboBoxesExpanded slotComboBoxes = or
    [ _isExpanded $ _neutralSlot slotComboBoxes
    , _isExpanded $ _upSlot slotComboBoxes
    , _isExpanded $ _downSlot slotComboBoxes
    ]

isSlotComboBoxesPressed :: PauseMenuSlotComboBoxes -> Bool
isSlotComboBoxesPressed slotComboBoxes = or
    [ _isPressed (_neutralSlot slotComboBoxes :: ComboBox)
    , _isPressed (_upSlot slotComboBoxes :: ComboBox)
    , _isPressed (_downSlot slotComboBoxes :: ComboBox)
    ]
