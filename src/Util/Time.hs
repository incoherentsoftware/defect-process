module Util.Time
    ( Time(..)
    , mkTime
    , updateTime
    , updateTimeDiffSecs
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock        (UTCTime, diffUTCTime, getCurrentTime)

import Util

data Time = Time
    { _utcTime  :: UTCTime
    , _diffSecs :: Secs
    }

mkTime :: MonadIO m => m Time
mkTime = do
    utcTime <- liftIO getCurrentTime
    return $ Time
        { _utcTime  = utcTime
        , _diffSecs = 0.0
        }

updateTime :: MonadIO m => Time -> m Time
updateTime prevTime = do
    utcTime <- liftIO getCurrentTime

    let
        prevUTCTime  = _utcTime prevTime
        prevDiffSecs = _diffSecs prevTime
        diffSecs     = realToFrac (diffUTCTime utcTime prevUTCTime) + prevDiffSecs

    return $ prevTime
        { _utcTime  = utcTime
        , _diffSecs = diffSecs
        }

updateTimeDiffSecs :: Secs -> Time -> Time
updateTimeDiffSecs diffSecs time = time {_diffSecs = diffSecs}
