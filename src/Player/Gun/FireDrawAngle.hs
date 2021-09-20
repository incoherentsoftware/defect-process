module Player.Gun.FireDrawAngle
    ( GunFireDrawAngle(..)
    , isForwardsGunFireDrawAngle
    , flipGunFireDrawAngle
    , calculateGunFireDrawAngle
    , restrictAngleByGunFireDrawAngle
    , gunFireDrawAngleDelta
    ) where

import Data.Aeson.Types (FromJSON, FromJSONKey(fromJSONKey), defaultJSONKeyOptions, genericFromJSONKey)
import Data.Bifunctor   (bimap)
import GHC.Generics     (Generic)

import Util

vertAngleEpsilon = 0.01 :: Radians

data GunFireDrawAngle
    = FireDraw90Degrees
    | FireDraw45Degrees
    | FireDraw0Degrees
    | FireDrawNeg45Degrees
    | FireDrawNeg90Degrees
    | BackFireDraw90Degrees
    | BackFireDraw45Degrees
    | BackFireDraw0Degrees
    | BackFireDrawNeg45Degrees
    | BackFireDrawNeg90Degrees
    deriving (Eq, FromJSON, Generic, Ord, Show)

instance FromJSONKey GunFireDrawAngle where
    fromJSONKey = genericFromJSONKey defaultJSONKeyOptions

isForwardsGunFireDrawAngle :: GunFireDrawAngle -> Bool
isForwardsGunFireDrawAngle = \case
    FireDraw90Degrees        -> True
    FireDraw45Degrees        -> True
    FireDraw0Degrees         -> True
    FireDrawNeg45Degrees     -> True
    FireDrawNeg90Degrees     -> True
    BackFireDraw90Degrees    -> False
    BackFireDraw45Degrees    -> False
    BackFireDraw0Degrees     -> False
    BackFireDrawNeg45Degrees -> False
    BackFireDrawNeg90Degrees -> False

flipGunFireDrawAngle :: GunFireDrawAngle -> GunFireDrawAngle
flipGunFireDrawAngle = \case
    FireDraw90Degrees        -> BackFireDraw90Degrees
    FireDraw45Degrees        -> BackFireDraw45Degrees
    FireDraw0Degrees         -> BackFireDraw0Degrees
    FireDrawNeg45Degrees     -> BackFireDrawNeg45Degrees
    FireDrawNeg90Degrees     -> BackFireDrawNeg90Degrees
    BackFireDraw90Degrees    -> FireDraw90Degrees
    BackFireDraw45Degrees    -> FireDraw45Degrees
    BackFireDraw0Degrees     -> FireDraw0Degrees
    BackFireDrawNeg45Degrees -> FireDrawNeg45Degrees
    BackFireDrawNeg90Degrees -> FireDrawNeg90Degrees

gunFireDrawAngleBounds :: GunFireDrawAngle -> (Radians, Radians)
gunFireDrawAngleBounds = bimap toRadians toRadians . \case
    FireDraw90Degrees        -> (Degrees 67.5, Degrees 90.0)
    FireDraw45Degrees        -> (Degrees 22.5, Degrees 67.5)
    FireDraw0Degrees         -> (Degrees (-22.5), Degrees 22.5)
    FireDrawNeg45Degrees     -> (Degrees (-67.5), Degrees (-22.5))
    FireDrawNeg90Degrees     -> (Degrees (-90.0), Degrees (-67.5))
    BackFireDraw90Degrees    -> (Degrees 90.0, Degrees 112.5)
    BackFireDraw45Degrees    -> (Degrees 112.5, Degrees 157.5)
    BackFireDraw0Degrees     -> (Degrees (-180.0), Degrees 180.0)
    BackFireDrawNeg45Degrees -> (Degrees (-157.5), Degrees (-112.5))
    BackFireDrawNeg90Degrees -> (Degrees (-112.5), Degrees (-90.0))

gunFireDrawAngleMinBound :: GunFireDrawAngle -> Radians
gunFireDrawAngleMinBound = fst . gunFireDrawAngleBounds

gunFireDrawAngleMaxBound :: GunFireDrawAngle -> Radians
gunFireDrawAngleMaxBound = snd . gunFireDrawAngleBounds

calculateGunFireDrawAngle :: Direction -> Radians -> GunFireDrawAngle
calculateGunFireDrawAngle dir aimAngle = case dir of
    RightDir -> fireDrawAngle
    LeftDir  -> flipGunFireDrawAngle fireDrawAngle
    where
        minAngle = gunFireDrawAngleMinBound
        maxAngle = gunFireDrawAngleMaxBound

        fireDrawAngle
            | aimAngle < minAngle BackFireDrawNeg45Degrees = BackFireDraw0Degrees
            | aimAngle < minAngle BackFireDrawNeg90Degrees = BackFireDrawNeg45Degrees
            | aimAngle < minAngle FireDrawNeg90Degrees     = BackFireDrawNeg90Degrees
            | aimAngle < minAngle FireDrawNeg45Degrees     = FireDrawNeg90Degrees
            | aimAngle < minAngle FireDraw0Degrees         = FireDrawNeg45Degrees
            | aimAngle > maxAngle BackFireDraw45Degrees    = BackFireDraw0Degrees
            | aimAngle > maxAngle BackFireDraw90Degrees    = BackFireDraw45Degrees
            | aimAngle > maxAngle FireDraw90Degrees        = BackFireDraw90Degrees
            | aimAngle > maxAngle FireDraw45Degrees        = FireDraw90Degrees
            | aimAngle > maxAngle FireDraw0Degrees         = FireDraw45Degrees
            | otherwise                                    = FireDraw0Degrees

restrictAngleByGunFireDrawAngle :: Radians -> Direction -> GunFireDrawAngle -> Radians
restrictAngleByGunFireDrawAngle angle dir fireDrawAngle = case fireDrawAngle' of
    BackFireDraw0Degrees
        | angle' < 0.0 && angle' > backFire45NegDegMinAngle -> backFire45NegDegMinAngle
        | angle' > 0.0 && angle' < backFire45DegMaxAngle    -> backFire45DegMaxAngle
    BackFireDraw90Degrees                                   ->
        max (gunFireDrawAngleMinBound BackFireDraw90Degrees + vertAngleEpsilon) angle'
    FireDraw90Degrees                                       ->
        min (gunFireDrawAngleMaxBound FireDraw90Degrees - vertAngleEpsilon) angle'
    BackFireDrawNeg90Degrees                                ->
        min (gunFireDrawAngleMaxBound BackFireDrawNeg90Degrees - vertAngleEpsilon) angle'
    FireDrawNeg90Degrees                                    ->
        max (gunFireDrawAngleMinBound FireDrawNeg90Degrees + vertAngleEpsilon) angle'
    _                                                       -> angle'
    where
        fireDrawAngle' = case dir of
            RightDir -> fireDrawAngle
            LeftDir  -> flipGunFireDrawAngle fireDrawAngle

        (minAngle, maxAngle)     = gunFireDrawAngleBounds fireDrawAngle'
        backFire45NegDegMinAngle = gunFireDrawAngleMinBound BackFireDrawNeg45Degrees
        backFire45DegMaxAngle    = gunFireDrawAngleMaxBound BackFireDraw45Degrees
        angle'
            | angle < minAngle   = minAngle
            | angle > maxAngle   = maxAngle
            | otherwise          = angle

gunFireDrawAngleDelta :: Radians -> Direction -> GunFireDrawAngle -> Radians
gunFireDrawAngleDelta angle dir fireDrawAngle = case dir of
    RightDir -> case fireDrawAngle of
        BackFireDraw0Degrees
            | angle < 0.0 -> angle + toRadians' BackFireDraw0Degrees
        _                 -> angle - toRadians' fireDrawAngle

    LeftDir -> case fireDrawAngle of
        FireDraw0Degrees
            | angle < 0.0 -> angle + toRadians' BackFireDraw0Degrees
        _                 -> angle - toRadians' (flipGunFireDrawAngle fireDrawAngle)

    where
        toRadians' :: GunFireDrawAngle -> Radians
        toRadians' = toRadians . \case
            FireDraw90Degrees        -> Degrees 90.0
            FireDraw45Degrees        -> Degrees 45.0
            FireDraw0Degrees         -> Degrees 0.0
            FireDrawNeg45Degrees     -> Degrees $ -45.0
            FireDrawNeg90Degrees     -> Degrees $ -90.0
            BackFireDraw90Degrees    -> Degrees 90.0
            BackFireDraw45Degrees    -> Degrees 135.0
            BackFireDraw0Degrees     -> Degrees 180.0
            BackFireDrawNeg45Degrees -> Degrees $ -135.0
            BackFireDrawNeg90Degrees -> Degrees $ -90.0
