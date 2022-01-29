module Level.Room.Event.SlotMachine.Util
    ( SlotsChoice(..)
    , formatSlotsChoice
    , parseFormattedSlotsChoice
    , isSlotsChoiceTextPlus
    , slotsChoiceTextColor
    , isSlotsChoicesAllPlus
    , isSlotsChoicesAllMinus
    ) where

import Data.Aeson.Types (FromJSON)
import GHC.Generics     (Generic)
import qualified Data.Text as T
import qualified Data.Text.Read as T

import Window.Graphics.Color
import World.Util

plusTextColor  = Color 145 255 3 255 :: Color
minusTextColor = Color 255 33 33 255 :: Color

data SlotsChoice
    = Plus25PercentChoice
    | Minus25PercentChoice
    | Plus50PercentChoice
    | Minus50PercentChoice
    | Plus75PercentChoice
    | Minus75PercentChoice
    | Plus100PercentChoice
    | Minus100PercentChoice
    | Plus200Choice
    | Minus200Choice
    | Plus50Choice
    | Minus50Choice
    | Plus100Choice
    | Minus100Choice
    | Plus150Choice
    | Minus150Choice
    deriving (Eq, Generic)
    deriving anyclass FromJSON

formatSlotsChoice :: SlotsChoice -> T.Text
formatSlotsChoice = \case
    Plus25PercentChoice   -> "{SlotsPlusSymbol}+25%"
    Minus25PercentChoice  -> "{SlotsMinusSymbol}-25%"
    Plus50PercentChoice   -> "{SlotsPlusSymbol}+50%"
    Minus50PercentChoice  -> "{SlotsMinusSymbol}-50%"
    Plus75PercentChoice   -> "{SlotsPlusSymbol}+75%"
    Minus75PercentChoice  -> "{SlotsMinusSymbol}-75%"
    Plus100PercentChoice  -> "{SlotsPlusSymbol}+100%"
    Minus100PercentChoice -> "{SlotsMinusSymbol}-100%"
    Plus200Choice         -> "{SlotsPlusSymbol}+200"
    Minus200Choice        -> "{SlotsMinusSymbol}-200"
    Plus50Choice          -> "{SlotsPlusSymbol}+50"
    Minus50Choice         -> "{SlotsMinusSymbol}-50"
    Plus100Choice         -> "{SlotsPlusSymbol}+100"
    Minus100Choice        -> "{SlotsMinusSymbol}-100"
    Plus150Choice         -> "{SlotsPlusSymbol}+150"
    Minus150Choice        -> "{SlotsMinusSymbol}-150"

parseFormattedSlotsChoice :: T.Text -> (GoldValue -> GoldValue)
parseFormattedSlotsChoice txt
    | Just txt' <- T.stripPrefix "{SlotsPlusSymbol}+" txt, Right (n, _) <- T.decimal txt'  =
        \(GoldValue v) -> GoldValue $ if
            | "%" `T.isSuffixOf` txt' -> ceiling $ fromIntegral v * (1.0 + fromIntegral n * 0.01)
            | otherwise               -> v + n
    | Just txt' <- T.stripPrefix "{SlotsMinusSymbol}-" txt, Right (n, _) <- T.decimal txt' =
        \(GoldValue v) -> GoldValue $ if
            | "%" `T.isSuffixOf` txt' -> ceiling $ fromIntegral v * (1.0 - fromIntegral n * 0.01)
            | otherwise               -> v - n
    | otherwise                                                                            = id

isSlotsChoiceTextPlus :: T.Text -> Bool
isSlotsChoiceTextPlus selectionTxt = "{SlotsPlusSymbol}" `T.isInfixOf` selectionTxt

slotsChoiceTextColor :: T.Text -> Color
slotsChoiceTextColor selectionTxt
    | isSlotsChoiceTextPlus selectionTxt = plusTextColor
    | otherwise                          = minusTextColor

isSlotsChoicePlus :: SlotsChoice -> Bool
isSlotsChoicePlus = \case
    Plus25PercentChoice   -> True
    Minus25PercentChoice  -> False
    Plus50PercentChoice   -> True
    Minus50PercentChoice  -> False
    Plus75PercentChoice   -> True
    Minus75PercentChoice  -> False
    Plus100PercentChoice  -> True
    Minus100PercentChoice -> False
    Plus200Choice         -> True
    Minus200Choice        -> False
    Plus50Choice          -> True
    Minus50Choice         -> False
    Plus100Choice         -> True
    Minus100Choice        -> False
    Plus150Choice         -> True
    Minus150Choice        -> False

isSlotsChoicesAllPlus :: [SlotsChoice] -> Bool
isSlotsChoicesAllPlus slotsChoices = and $ map isSlotsChoicePlus slotsChoices

isSlotsChoicesAllMinus :: [SlotsChoice] -> Bool
isSlotsChoicesAllMinus slotsChoices = and $ map (not . isSlotsChoicePlus) slotsChoices
