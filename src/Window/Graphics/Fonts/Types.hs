module Window.Graphics.Fonts.Types
    ( FontType(..)
    , Font(..)
    , GraphicsFonts(..)
    ) where

import qualified SDL.Font

data FontType
    = Font12
    | Font14
    | Font16
    | Font22
    | Font26
    | Font32
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
    { _font12    :: Font
    , _font14    :: Font
    , _font16    :: Font
    , _font22    :: Font
    , _font26    :: Font
    , _font32    :: Font
    , _font44    :: Font
    , _altFont36 :: Font
    }
