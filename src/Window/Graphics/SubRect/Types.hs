module Window.Graphics.SubRect.Types
    ( SubRect(..)
    ) where

import Data.Aeson.Types (FromJSON, Parser, Value(Array, Number), parseJSON, typeMismatch)
import Data.Scientific  (Scientific, toRealFloat)
import Foreign.C.Types  (CInt)
import qualified Data.Vector as V

data SubRect = SubRect
    { _x      :: CInt
    , _y      :: CInt
    , _width  :: CInt
    , _height :: CInt
    }

instance FromJSON SubRect where
    parseJSON :: Value -> Parser SubRect
    parseJSON value@(Array v) =
        let
            toCInt :: Scientific -> CInt
            toCInt = (fromIntegral :: Num b => Integer -> b) . round . (toRealFloat :: Scientific -> Float)
        in case V.toList v of
            [Number x, Number y, Number w, Number h] -> return $ SubRect
                { _x      = toCInt x
                , _y      = toCInt y
                , _width  = toCInt w
                , _height = toCInt h
                }
            _                                        -> typeMismatch "SubRect" value
    parseJSON value           = typeMismatch "SubRect" value
