module Util
    ( Some(..)
    , PrettyShow(..)
    , Degrees(..)
    , FileName
    , Secs
    , Radians
    , Distance
    , OffsetX
    , OffsetY
    , Speed
    , SpeedX
    , SpeedY
    , Acceleration
    , VecX
    , VecY
    , PosX
    , PosY
    , VelX
    , VelY
    , maxSecs
    , Direction(..)
    , flipDirection
    , directionNeg
    , directionPos
    , Vec(vecToTuple)
    , vecX
    , vecY
    , vecMagnitude
    , vecNormalize
    , vecCross
    , vecDot
    , vecSub
    , vecAdd
    , vecMul
    , vecDiv
    , vecDistSq
    , vecDist
    , vecFlip
    , vecRotate
    , vecRoundXY
    , vecFlipRotate
    , Vec2(..)
    , toVec2
    , zeroVec2
    , Pos2(..)
    , mkPos2
    , toPos2
    , zeroPos2
    , Vel2(..)
    , mkVel2
    , toVel2
    , zeroVel2
    , toDegrees
    , toRadians
    , approxEq
    , approxEqEx
    , randomChoice
    , maxZero
    , safeTail
    , safeInit
    , maybeLast
    , nubAdjacent
    , maybeMinimum
    , maybeMaximum
    , (!!?)
    , whenM
    , unlessM
    , stripSuffix
    , aesonFieldDropUnderscore
    , getResourceDirectory
    , translateResourcePath
    , traceIO'
    ) where

import Control.Exception      (SomeException)
import Control.Monad          (unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson.Types       (FromJSON, Options, Parser, Value(Array, Number))
import Data.Aeson.Types       (defaultOptions, fieldLabelModifier, parseJSON, typeMismatch)
import Data.Maybe             (listToMaybe)
import Data.Scientific        (toRealFloat)
import Debug.Trace            (traceIO)
import GHC.Generics           (Generic)
import System.Environment     (getExecutablePath)
import System.FilePath        (dropFileName)
import System.Random          (randomRIO)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Vector as V

data Some a where
    Some :: a d -> Some a

class Show a => PrettyShow a where
    prettyShow :: a -> T.Text
    prettyShow = T.pack . show

instance PrettyShow Int
instance PrettyShow SomeException

newtype Degrees = Degrees Float
    deriving (Eq, Ord)
    deriving newtype (FromJSON, Num, Real, Fractional)

type FileName     = FilePath
type Secs         = Float
type Radians      = Float
type Distance     = Float
type OffsetX      = Float
type OffsetY      = Float
type Speed        = Float
type SpeedX       = Float
type SpeedY       = Float
type Acceleration = Float

type VecX = Float
type VecY = Float
type PosX = VecX
type PosY = VecX
type VelX = VecX
type VelY = VecY

maxSecs :: Secs
maxSecs = 9999999.0

data Direction
    = LeftDir
    | RightDir
    deriving (Eq, Generic, Show)
    deriving anyclass FromJSON

flipDirection :: Direction -> Direction
flipDirection = \case
    LeftDir  -> RightDir
    RightDir -> LeftDir

directionNeg :: Direction -> Float
directionNeg dir = if dir == LeftDir then -1.0 else 1.0

directionPos :: Direction -> Float
directionPos dir = if dir == LeftDir then 1.0 else -1.0

class Vec v where
    mkVec      :: (VecX, VecY) -> v
    vecToTuple :: v -> (VecX, VecY)

vecX :: Vec v => v -> VecX
vecX = fst . vecToTuple

vecY :: Vec v => v -> VecY
vecY = snd . vecToTuple

vecMagnitude :: Vec v => v -> Float
vecMagnitude v = sqrt (x ** 2 + y ** 2)
    where (x, y) = vecToTuple v

vecNormalize :: Vec v => v -> v
vecNormalize v
    | mag < 0.001 = v
    | otherwise   = mkVec (x / mag, y / mag)
    where
        mag    = vecMagnitude v
        (x, y) = vecToTuple v

vecCross :: Vec v => v -> v -> Float
vecCross v1 v2 = (x1 * y2) - (y1 * x2)
    where
        (x1, y1) = vecToTuple v1
        (x2, y2) = vecToTuple v2

vecDot :: Vec v => v -> v -> Float
vecDot v1 v2 = x1 * x2 + y1 * y2
    where
        (x1, y1) = vecToTuple v1
        (x2, y2) = vecToTuple v2

vecSub :: Vec v => v -> v -> v
vecSub v1 v2 = mkVec (x1 - x2, y1 - y2)
    where
        (x1, y1) = vecToTuple v1
        (x2, y2) = vecToTuple v2

vecAdd :: Vec v => v -> v -> v
vecAdd v1 v2 = mkVec (x1 + x2, y1 + y2)
    where
        (x1, y1) = vecToTuple v1
        (x2, y2) = vecToTuple v2

vecMul :: Vec v => v -> Float -> v
vecMul v f = mkVec (f * x, f * y)
    where (x, y) = vecToTuple v

vecDiv :: Vec v => v -> Float -> v
vecDiv v f = vecMul v (1.0 / f)

vecDistSq :: Vec v => v -> v -> Float
vecDistSq v1 v2 = (x1 - x2) ** 2 + (y1 - y2) ** 2
    where
        (x1, y1) = vecToTuple v1
        (x2, y2) = vecToTuple v2

vecDist :: Vec v => v -> v -> Float
vecDist v1 v2 = sqrt $ vecDistSq v1 v2

vecFlip :: Vec v => v -> Direction -> v
vecFlip v dir = mkVec (x', y)
    where
        (x, y) = vecToTuple v
        x'     = x * directionNeg dir

vecRotate :: Vec v => v -> Radians -> v
vecRotate v angle = mkVec (x', y')
    where
        (x, y) = vecToTuple v
        x'     = x * cos angle - y * sin angle
        y'     = x * sin angle + y * cos angle

vecFlipRotate :: Vec v => v -> Direction -> Radians -> v
vecFlipRotate v dir angle = vecRotate (vecFlip v dir) angle

vecRoundXY :: Vec v => v -> v
vecRoundXY v = mkVec (x', y')
    where
        (x, y) = vecToTuple v
        x'     = fromIntegral $ round x
        y'     = fromIntegral $ round y

data Vec2 = Vec2 VecX VecY

instance Vec Vec2 where
    mkVec :: (VecX, VecY) -> Vec2
    mkVec (x, y) = Vec2 x y

    vecToTuple :: Vec2 -> (VecX, VecY)
    vecToTuple (Vec2 x y) = (x, y)

toVec2 :: Vec v => v -> Vec2
toVec2 = mkVec . vecToTuple

zeroVec2 :: Vec2
zeroVec2 = Vec2 0.0 0.0

instance FromJSON Vec2 where
    parseJSON :: Value -> Parser Vec2
    parseJSON (Array v)
        | [Number x, Number y] <- V.toList v = return $ Vec2 (toRealFloat x) (toRealFloat y)
    parseJSON v                              = typeMismatch "Vec2" v

data Pos2 = Pos2 VecX VecY
    deriving Show
    deriving anyclass PrettyShow

instance Vec Pos2 where
    mkVec :: (VecX, VecY) -> Pos2
    mkVec (x, y) = Pos2 x y

    vecToTuple :: Pos2 -> (VecX, VecY)
    vecToTuple (Pos2 x y) = (x, y)

instance FromJSON Pos2 where
    parseJSON :: Value -> Parser Pos2
    parseJSON (Array v)
        | [Number x, Number y] <- V.toList v = return $ Pos2 (toRealFloat x) (toRealFloat y)
    parseJSON v                              = typeMismatch "Pos2" v

mkPos2 :: (VecX, VecY) -> Pos2
mkPos2 = mkVec

toPos2 :: Vec v => v -> Pos2
toPos2 = mkVec . vecToTuple

zeroPos2 :: Pos2
zeroPos2 = Pos2 0.0 0.0

data Vel2 = Vel2 VecX VecY
    deriving Show

instance Vec Vel2 where
    mkVec :: (VecX, VecY) -> Vel2
    mkVec (x, y) = Vel2 x y

    vecToTuple :: Vel2 -> (VecX, VecY)
    vecToTuple (Vel2 x y) = (x, y)

instance FromJSON Vel2 where
    parseJSON :: Value -> Parser Vel2
    parseJSON value@(Array v) = case V.toList v of
        [Number x, Number y] -> return $ Vel2 (toRealFloat x) (toRealFloat y)
        _                    -> typeMismatch "Vel2" value
    parseJSON value           = typeMismatch "Vel2" value

mkVel2 :: (VecX, VecY) -> Vel2
mkVel2 = mkVec

toVel2 :: Vec v => v -> Vel2
toVel2 = mkVec . vecToTuple

zeroVel2 :: Vel2
zeroVel2 = Vel2 0.0 0.0

toDegrees :: Radians -> Degrees
toDegrees radians = Degrees $ radians * (180.0 / pi)

toRadians :: Degrees -> Radians
toRadians (Degrees angle) = angle * (pi / 180.0)

approxEq :: Float -> Float -> Bool
approxEq f1 f2 = approxEqEx f1 f2 0.1

approxEqEx :: Float -> Float -> Float -> Bool
approxEqEx f1 f2 epsilon = abs (f1 - f2) <= epsilon

randomChoice :: MonadIO m => NE.NonEmpty a -> m a
randomChoice xs = liftIO $ (xs NE.!!) <$> randomRIO (0, NE.length xs - 1)

maxZero :: Float -> Float
maxZero = max 0.0

safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs = tail xs

safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast xs = Just $ last xs

nubAdjacent :: Eq a => [a] -> [a]
nubAdjacent []     = []
nubAdjacent (x1:x2:xs)
    | x1 == x2     = nubAdjacent $ x1:xs
    | otherwise    = x1:x2:nubAdjacent xs
nubAdjacent (x:xs) = x:nubAdjacent xs

maybeMinimum :: Ord a => [a] -> Maybe a
maybeMinimum [] = Nothing
maybeMinimum xs = Just $ minimum xs

maybeMaximum :: Ord a => [a] -> Maybe a
maybeMaximum [] = Nothing
maybeMaximum xs = Just $ maximum xs

(!!?) :: [a] -> Int -> Maybe a
(!!?) [] _ = Nothing
(!!?) xs idx
    | idx < 0   = Nothing
    | otherwise = listToMaybe $ drop idx xs

whenM :: Monad m => m Bool -> m () -> m ()
whenM p s = flip when s =<< p

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p s = flip unless s =<< p

stripSuffix :: String -> String -> Maybe String
stripSuffix suffix str
    | suffix `L.isSuffixOf` str = Just $ take (length str - length suffix) str
    | otherwise                 = Nothing

aesonFieldDropUnderscore :: Options
aesonFieldDropUnderscore = defaultOptions
    { fieldLabelModifier = \case
        '_':cs -> cs
        cs     -> cs
    }

getResourceDirectory :: MonadIO m => m FilePath
getResourceDirectory = dropFileName <$> liftIO getExecutablePath

-- relative paths would be fine except we need absolute paths for macOS app bundles
translateResourcePath :: MonadIO m => FilePath -> m FilePath
translateResourcePath filePath = (++ filePath) <$> getResourceDirectory

traceIO' :: (MonadIO m, Show a) => a -> m ()
traceIO' = liftIO . traceIO . show
