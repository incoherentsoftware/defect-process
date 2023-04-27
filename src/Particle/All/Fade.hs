module Particle.All.Fade
    ( mkFadeParticle
    ) where

import Control.Monad.IO.Class (MonadIO)

import Constants
import Particle as P
import Projectile.Types
import Util
import Window.Graphics

fadeOutMultiplier = 3.5 :: Float

data FadeData = FadeData
    { _dir     :: Direction
    , _opacity :: Opacity
    , _zIndex  :: ZIndex
    , _image   :: Image
    }

mkFadeParticle :: ProjectileVoluntaryClearData -> Some Particle
mkFadeParticle voluntaryClearData =
    let
        fadeData = FadeData
            { _dir     = _dir (voluntaryClearData :: ProjectileVoluntaryClearData)
            , _opacity = FullOpacity
            , _zIndex  = _zIndex (voluntaryClearData :: ProjectileVoluntaryClearData)
            , _image   = _image (voluntaryClearData :: ProjectileVoluntaryClearData)
            }
        pos      = _pos (voluntaryClearData :: ProjectileVoluntaryClearData)
    in Some $ (mkParticle fadeData pos maxSecs)
        { P._draw   = draw
        , P._update = update
        }

draw :: (GraphicsReadWrite m, MonadIO m) => ParticleDraw FadeData m
draw fade = drawImageWithOpacity pos dir zIndex opacity img
    where
        pos      = P._pos fade
        fadeData = P._data fade
        dir      = _dir (fadeData :: FadeData)
        zIndex   = _zIndex (fadeData :: FadeData)
        opacity  = _opacity fadeData
        img      = _image (fadeData :: FadeData)

update :: ParticleUpdate FadeData
update fade = fade
    { _data = fadeData'
    , _ttl  = ttl
    }
    where
        fadeData  = P._data fade
        opacity   = decreaseOpacity (fadeOutMultiplier * timeStep) (_opacity fadeData)
        fadeData' = fadeData {_opacity = opacity}
        ttl       = if
            | isMinOpacity opacity -> 0.0
            | otherwise            -> maxSecs
