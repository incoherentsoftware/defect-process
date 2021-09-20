module Constants
    ( timeStep
    , virtualRenderWidth
    , virtualRenderHeight
    , minWindowWidth
    , minWindowHeight
    ) where

import Util

timeStep = 1.0 / 120.0 :: Secs

virtualRenderWidth  = 1920.0 :: Float
virtualRenderHeight = 1080.0 :: Float

minWindowWidth  = 1024 :: Int
minWindowHeight = 768  :: Int
