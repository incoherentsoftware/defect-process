module Window.Graphics.Opacity
    ( minAlpha
    , Opacity(..)
    , decreaseOpacity
    , increaseOpacity
    , opacityToAlpha
    , isMinOpacity
    ) where

import Data.Aeson.Types (FromJSON, Parser, Value(Number), parseJSON, typeMismatch)
import Data.Scientific  (toRealFloat)

import Window.Graphics.Color

minAlpha = 0   :: Alpha
maxAlpha = 255 :: Alpha

data Opacity
    = FullOpacity
    | Opacity Float

instance FromJSON Opacity where
    parseJSON :: Value -> Parser Opacity
    parseJSON (Number v) =
        let v' = toRealFloat v :: Float
        in if
            | v' >= 0.0 && v' <= 1.0 -> return $ Opacity v'
            | otherwise              -> fail "Invalid Opacity, expected [0.0, 1.0]"
    parseJSON val        = typeMismatch "Opacity" val

decreaseOpacity :: Float -> Opacity -> Opacity
decreaseOpacity decreaseVal opacity = Opacity $ max 0.0 (opacityVal - decreaseVal)
    where
        opacityVal = case opacity of
            FullOpacity -> 1.0
            Opacity o   -> o

increaseOpacity :: Float -> Alpha -> Opacity -> Opacity
increaseOpacity increaseVal targetAlpha opacity
    | opacityToAlpha opacity' > targetAlpha = targetOpacity
    | otherwise                             = opacity'
    where
        opacity'      = decreaseOpacity (-increaseVal) opacity
        targetOpacity = Opacity $ fromIntegral targetAlpha / fromIntegral maxAlpha

opacityToAlpha :: Opacity -> Alpha
opacityToAlpha = \case
    FullOpacity -> maxAlpha
    Opacity o   -> max minAlpha . min maxAlpha . round $ o * fromIntegral maxAlpha

isMinOpacity :: Opacity -> Bool
isMinOpacity = \case
    FullOpacity -> False
    Opacity o   -> o <= 0.0
