module Window.Graphics.Fonts.Types
    ( FontType(..)
    , Font(..)
    , GraphicsFonts(..)
    ) where

import qualified SDL.Font

data FontType
    = Font16
    | Font22
    | Font26
    | Font29
    | Font32
    | Font36
    | Font44
    | AltFont36
    deriving Show

data Font = Font
    { _type          :: FontType
    , _sdlFont       :: SDL.Font.Font
    , _sdlFontStyles :: [SDL.Font.Style]
    }

instance Show Font where
    show :: Font -> String
    show font = show (_type font) ++ show (_sdlFontStyles font)

data GraphicsFonts = GraphicsFonts
    { _font16    :: Font
    , _font22    :: Font
    , _font26    :: Font
    , _font29    :: Font
    , _font32    :: Font
    , _font36    :: Font
    , _font44    :: Font
    , _altFont36 :: Font
    }
