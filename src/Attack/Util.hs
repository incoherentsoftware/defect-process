module Attack.Util
    ( minHealthValue
    , Health(..)
    , mkHealth
    , decreaseHealth
    , isHealthZero
    , isHealthMax
    , Damage(..)
    , Stagger(..)
    , damageToStagger
    , AttackVel(..)
    , attackVelToVel2
    , flipAttackVel
    , attackVelHasVelX
    , AttackHitEffectType(..)
    ) where


import Data.Aeson.Types (FromJSON, Parser, Value(Array, Null, Number, String), parseJSON, typeMismatch)
import Data.Scientific  (toRealFloat)
import qualified Data.Text as T
import qualified Data.Vector as V

import Util

minHealthValue = 1 :: Int

data Health = Health
    { _value    :: Int
    , _maxValue :: Int
    }

instance Show Health where
    show :: Health -> String
    show = show . _value

instance PrettyShow Health where
    prettyShow :: Health -> T.Text
    prettyShow health = T.pack $ show (_value health) ++ " / " ++ show (_maxValue health)

instance FromJSON Health where
    parseJSON :: Value -> Parser Health
    parseJSON (Number v) = return $ mkHealth (round (toRealFloat v :: Double))
    parseJSON v          = typeMismatch "Health" v

mkHealth :: Int -> Health
mkHealth value = Health
    { _value    = value
    , _maxValue = value
    }

decreaseHealth :: Damage -> Health -> Health
decreaseHealth (Damage dmgVal) health = health
    { _value = max 0 (_value health - dmgVal)
    }

isHealthZero :: Health -> Bool
isHealthZero health = _value health <= 0

isHealthMax :: Health -> Bool
isHealthMax health = _value health >= _maxValue health

newtype Damage = Damage
    { _int :: Int
    }
    deriving (Eq, Ord, Show)
    deriving newtype (FromJSON, Num)

newtype Stagger = Stagger
    { _int :: Int
    }
    deriving (Eq, Ord)
    deriving newtype (FromJSON, Num)

damageToStagger :: Damage -> Stagger
damageToStagger (Damage val) = Stagger val

data AttackVel
    = NoAttackVel
    | AttackVelX VelX
    | AttackVelY VelY
    | AttackVel2 Vel2

instance FromJSON AttackVel where
    parseJSON :: Value -> Parser AttackVel
    parseJSON a@(Array v) = case V.toList v of
        [Number x, Number y] -> return . AttackVel2 $ Vel2 (toRealFloat x) (toRealFloat y)
        [Number x, Null]     -> return $ AttackVelX (toRealFloat x)
        [Null, Number y]     -> return $ AttackVelY (toRealFloat y)
        _                    -> typeMismatch "AttackVel" a
    parseJSON Null        = return NoAttackVel
    parseJSON value       = typeMismatch "AttackVel" value

attackVelToVel2 :: AttackVel -> Vel2 -> Vel2
attackVelToVel2 atkVel vel@(Vel2 velX velY) = case atkVel of
    NoAttackVel        -> vel
    AttackVelX atkVelX -> Vel2 atkVelX velY
    AttackVelY atkVelY -> Vel2 velX atkVelY
    AttackVel2 atkVel2 -> atkVel2

flipAttackVel :: Direction -> AttackVel -> AttackVel
flipAttackVel RightDir atkVel = atkVel
flipAttackVel LeftDir atkVel  = case atkVel of
    NoAttackVel                       -> atkVel
    AttackVelX atkVelX                -> AttackVelX (-atkVelX)
    AttackVelY _                      -> atkVel
    AttackVel2 (Vel2 atkVelX atkVelY) -> AttackVel2 $ Vel2 (-atkVelX) atkVelY

attackVelHasVelX :: AttackVel -> Bool
attackVelHasVelX = \case
    NoAttackVel  -> False
    AttackVelX _ -> True
    AttackVelY _ -> False
    AttackVel2 _ -> True

data AttackHitEffectType
    = NormalHitEffect
    | StrongHitEffect
    | WeakHitEffect

instance FromJSON AttackHitEffectType where
    parseJSON :: Value -> Parser AttackHitEffectType
    parseJSON (String v)
        | v == "normalHit" = return NormalHitEffect
        | v == "strongHit" = return StrongHitEffect
        | v == "weakHit"   = return WeakHitEffect
    parseJSON value        = typeMismatch "AttackHitEffectType" value
