module Window.Graphics.Util
    ( WindowMode(..)
    , Lerp(..)
    , CameraSpace(..)
    , FrameIndex(..)
    , ZIndex(..)
    , DrawScale(..)
    , drawScaleToFloat
    , FrameTagName(..)
    , FrameTag(..)
    , LoopData(..)
    , loopDataCycle
    , surfaceWidthHeight
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson.Types       (FromJSON, Parser, ToJSON, Value(Number), parseJSON, typeMismatch)
import Data.Scientific        (toRealFloat)
import Data.String            (IsString)
import Foreign.Storable       (peek)
import GHC.Generics           (Generic)
import qualified Data.Text as T
import qualified SDL
import qualified SDL.Raw.Types

data WindowMode
    = FullscreenMode
    | FullscreenDesktopMode
    | WindowedMode
    deriving (Eq, Generic, Show)

instance FromJSON WindowMode
instance ToJSON WindowMode

newtype Lerp = Lerp {_float :: Float}
    deriving (Eq, Ord)
    deriving newtype (Fractional, Num)

data CameraSpace
    = CameraScreenSpace
    | CameraWorldSpace

newtype FrameIndex = FrameIndex {_int :: Int}
    deriving (Enum, Eq, Ord, Show)
    deriving newtype (Bounded, FromJSON, Integral, Num, Real)

newtype ZIndex = ZIndex Int
    deriving (Eq, Ord, Show)
    deriving newtype Num

instance FromJSON ZIndex where
    parseJSON :: Value -> Parser ZIndex
    parseJSON (Number scale) = return . ZIndex . round $ toRealFloat scale
    parseJSON value          = typeMismatch "ZIndex" value

data DrawScale
    = NonScaled
    | Scaled Float

instance FromJSON DrawScale where
    parseJSON :: Value -> Parser DrawScale
    parseJSON (Number scale) = return $ Scaled (toRealFloat scale)
    parseJSON value          = typeMismatch "DrawScale" value

drawScaleToFloat :: DrawScale -> Float
drawScaleToFloat = \case
    NonScaled -> 1.0
    Scaled f  -> f

newtype FrameTagName = FrameTagName T.Text
    deriving Eq
    deriving newtype (FromJSON, IsString)

data FrameTag = FrameTag
    { _name            :: FrameTagName
    , _startFrameIndex :: FrameIndex
    , _endFrameIndex   :: FrameIndex
    , _stepFrameIndex  :: FrameIndex
    }

data LoopData = LoopData
    { _startFrameIndex :: FrameIndex
    , _endFrameIndex   :: FrameIndex
    , _maxLoops        :: Maybe Int
    }

loopDataCycle :: Maybe LoopData -> [a] -> [a]
loopDataCycle loopData vs = case loopData of
    Nothing        -> vs
    Just loopData' ->
        let
            startIndex = _int $ _startFrameIndex (loopData' :: LoopData)
            endIndex   = _int $ _endFrameIndex (loopData' :: LoopData)
            numLooping = endIndex + 1 - startIndex

            start     = take startIndex vs
            cycledMid = cycle $ take numLooping (drop startIndex vs)
            mid       = case _maxLoops loopData' of
                Just maxLoops -> take ((maxLoops + 1) * numLooping) cycledMid
                Nothing       -> cycledMid
            end       = drop (endIndex + 1) vs
        in start ++ mid ++ end

surfaceWidthHeight :: MonadIO m => SDL.Surface -> m (Int, Int)
surfaceWidthHeight (SDL.Surface rawSurface _) = do
    rawSurfaceData <- liftIO $ peek rawSurface
    let width       = fromIntegral $ SDL.Raw.Types.surfaceW rawSurfaceData
    let height      = fromIntegral $ SDL.Raw.Types.surfaceH rawSurfaceData
    return (width, height)
