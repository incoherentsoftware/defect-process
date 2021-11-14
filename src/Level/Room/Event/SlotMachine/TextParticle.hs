module Level.Room.Event.SlotMachine.TextParticle
    ( mkSlotMachineTextParticle
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T

import Constants
import FileCache
import Particle as P
import Util
import Window.Graphics
import World.ZIndex

screenPos           = Pos2 1870.0 105.0 :: Pos2
opacityDecreaseRate = 0.35              :: Float
lingerSecs          = 5.0               :: Secs

data TextParticleData = TextParticleData
    { _opacity           :: Opacity
    , _lingerTtl         :: Secs
    , _symbolDisplayText :: SymbolDisplayText
    }

mkSlotMachineTextParticle :: (FileCache m, GraphicsRead m, MonadIO m) => SymbolDisplayText -> m (Some Particle)
mkSlotMachineTextParticle symbolDisplayTxt = do
    let
        txt  = _text (symbolDisplayTxt :: SymbolDisplayText)
        txt' = T.takeWhileEnd (/= '}') txt
    symbolDisplayTxt' <- updateSymbolDisplayText txt' symbolDisplayTxt

    let
        textParticleData  = TextParticleData
            { _opacity           = Opacity 1.0
            , _lingerTtl         = lingerSecs
            , _symbolDisplayText = symbolDisplayTxt'
            }

    return . Some $ (mkParticle textParticleData screenPos maxSecs)
        { _draw   = draw
        , _update = update
        }

draw :: (GraphicsReadWrite m, MonadIO m) => ParticleDraw TextParticleData m
draw textParticle =
    let
        pos              = P._pos textParticle
        textParticleData = _data textParticle
        opacity          = _opacity textParticleData
        symbolDisplayTxt = _symbolDisplayText textParticleData
    in do
        setCameraSpace CameraScreenSpace
        drawSymbolDisplayTextRightAlignedEx pos uiFrontZIndex NonScaled opacity symbolDisplayTxt
        setCameraSpace CameraWorldSpace

update :: ParticleUpdate TextParticleData
update textParticle = textParticle
    { _data = textParticleData
        { _opacity   = opacity'
        , _lingerTtl = lingerTtl
        }
    , _ttl  = ttl
    }
    where
        textParticleData           = _data textParticle
        lingerTtl                  = max 0.0 (_lingerTtl textParticleData - timeStep)
        opacity                    = _opacity textParticleData
        opacity'
            | lingerTtl <= 0.0     = decreaseOpacity (opacityDecreaseRate * timeStep) opacity
            | otherwise            = opacity
        ttl
            | isMinOpacity opacity = 0.0
            | otherwise            = P._ttl textParticle
