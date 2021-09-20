module Collision.Hitbox.Types
    ( Hitbox(..)
    , RectHitboxJSON(..)
    ) where

import Data.Aeson.Types (FromJSON, Parser, Value(Array, Number), parseJSON, typeMismatch)
import Data.Scientific  (toRealFloat)
import qualified Data.Vector as V

import Util

data Hitbox
    = RectHitbox Pos2 Float Float Pos2
    | PolyHitbox [Pos2] [Pos2]
    | DummyHitbox Pos2
    deriving Show

instance FromJSON Hitbox where
    parseJSON :: Value -> Parser Hitbox
    parseJSON (Array v) = case V.toList v of
        [Number x, Number y, Number w, Number h] ->
            let
                x' = toRealFloat x
                y' = toRealFloat y
                w' = toRealFloat w
                h' = toRealFloat h
            in return $ RectHitbox zeroPos2 w' h' (Pos2 x' y')
        v'                                       ->
            let
                parseOffsets :: [Value] -> Maybe [Pos2]
                parseOffsets []             = Just []
                parseOffsets (Array xy:xys) = case V.toList xy of
                    [Number x, Number y] -> (Pos2 (toRealFloat x) (toRealFloat y):) <$> parseOffsets xys
                    _                    -> Nothing
                parseOffsets _              = Nothing
            in case parseOffsets v' of
                Just offsets -> return $ PolyHitbox offsets offsets
                Nothing      -> fail "Invalid polygon hitbox"
    parseJSON value     = typeMismatch "Hitbox" value

newtype RectHitboxJSON = RectHitboxJSON
    { _fromJSON :: Hitbox
    }

instance FromJSON RectHitboxJSON where
    parseJSON :: Value -> Parser RectHitboxJSON
    parseJSON (Array v) = case V.toList v of
        [Number x, Number y, Number w, Number h] ->
            let
                x' = toRealFloat x
                y' = toRealFloat y
                w' = toRealFloat w
                h' = toRealFloat h
            in return . RectHitboxJSON $ RectHitbox (Pos2 x' y') w' h' zeroPos2
        _                                        -> fail "Invalid rect hitbox"
    parseJSON value     = typeMismatch "RectHitboxJSON" value
