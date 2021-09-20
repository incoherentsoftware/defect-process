module World.Screenshake
    ( module World.Screenshake.Types
    , mkScreenshake
    , updateScreenshake
    , drawScreenshake
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State    (execState, get, modify, put, when)
import System.Random          (randomRIO)
import qualified Data.List.NonEmpty as NE

import Constants
import Util
import Window.Graphics
import World.Screenshake.Types

pulseFreq            = 0.05                     :: Secs
pulseDecayMultiplier = 0.5                      :: Float
pulseMinThreshold    = ScreenshakeMagnitude 1.0 :: ScreenshakeMagnitude

mkScreenshake :: ScreenshakeMagnitude -> Screenshake
mkScreenshake magnitude = Screenshake
    { _magnitude    = magnitude
    , _pulseTrigger = pulseFreq
    }

updateScreenshake :: Screenshake -> Screenshake
updateScreenshake screenshake = screenshake
    { _magnitude    = magnitude'
    , _pulseTrigger = pulseTrigger'
    }
    where
        magnitude                                    = _magnitude screenshake
        magnitude'
            | magnitude > zeroScreenshakeMagnitude && pulseTrigger <= 0.0 = flip execState magnitude $ do
                modify $ (* ScreenshakeMagnitude pulseDecayMultiplier)
                get >>= \m -> when (m < pulseMinThreshold) (put zeroScreenshakeMagnitude)
            | otherwise                                                   = magnitude

        pulseTrigger              = _pulseTrigger screenshake
        pulseTrigger'
            | pulseTrigger <= 0.0 = pulseFreq
            | otherwise           = maxZero $ pulseTrigger - timeStep

drawScreenshake :: (GraphicsReadWrite m, MonadIO m) => Screenshake -> m ()
drawScreenshake screenshake
    | magnitudeVal > 0.0 =
        when (pulseTrigger <= timeStep) $ do
            offsetX <- liftIO $ randomRIO (-magnitudeVal, magnitudeVal)
            offsetY <- (* (magnitudeVal - abs offsetX)) <$> randomChoice (1.0 NE.:| [-1.0])
            setCameraOffset (Pos2 offsetX offsetY)
    | otherwise          = setCameraOffset zeroPos2
        where
            magnitudeVal = _float (_magnitude screenshake :: ScreenshakeMagnitude)
            pulseTrigger = _pulseTrigger screenshake
