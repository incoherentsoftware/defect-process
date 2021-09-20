module Window.Graphics.Color
    ( Alpha
    , Color(..)
    , blackColor
    , whiteColor
    , redColor
    , colorToV4
    ) where

import Data.Word (Word8)
import qualified SDL

type Alpha = Word8
data Color = Color Word8 Word8 Word8 Alpha

blackColor = Color 0 0 0 255       :: Color
whiteColor = Color 255 255 255 255 :: Color
redColor   = Color 255 0 0 255     :: Color

colorToV4 :: Color -> SDL.V4 Word8
colorToV4 (Color r g b a) = SDL.V4 r g b a
