module Particle.All.Simple
    ( loadSimpleParticle
    , loadSimpleParticleEx
    , loadSimpleParticleRotated
    , loadSimpleParticleRotatedEx
    , loadSimpleParticleWithVel
    ) where

import Control.Monad.IO.Class (MonadIO)

import FileCache
import Particle as P
import Util
import Window.Graphics

fakeAliveSecs = 999.0 :: Secs

data SimpleData = SimpleData
    { _dir    :: Direction
    , _zIndex :: ZIndex
    , _scale  :: DrawScale
    , _sprite :: Sprite
    }

loadSimpleParticle
    :: (FileCache m, GraphicsRead m, MonadIO m)
    => Pos2
    -> Direction
    -> ZIndex
    -> PackResourceFilePath
    -> m (Some Particle)
loadSimpleParticle pos dir zIndex packSprFilePath =
    loadSimpleParticleInternal pos dir zeroVel2 zIndex 0.0 NonScaled packSprFilePath

loadSimpleParticleEx
    :: (FileCache m, GraphicsRead m, MonadIO m)
    => Pos2
    -> Direction
    -> ZIndex
    -> DrawScale
    -> PackResourceFilePath
    -> m (Some Particle)
loadSimpleParticleEx pos dir zIndex scale packSprFilePath =
    loadSimpleParticleInternal pos dir zeroVel2 zIndex 0.0 scale packSprFilePath

loadSimpleParticleRotated
    :: (FileCache m, GraphicsRead m, MonadIO m)
    => Pos2
    -> Direction
    -> ZIndex
    -> Radians
    -> PackResourceFilePath
    -> m (Some Particle)
loadSimpleParticleRotated pos dir zIndex angle packSprFilePath =
    loadSimpleParticleInternal pos dir zeroVel2 zIndex angle NonScaled packSprFilePath

loadSimpleParticleRotatedEx
    :: (FileCache m, GraphicsRead m, MonadIO m)
    => Pos2
    -> Direction
    -> ZIndex
    -> Radians
    -> DrawScale
    -> PackResourceFilePath
    -> m (Some Particle)
loadSimpleParticleRotatedEx pos dir zIndex angle scale packSprFilePath =
    loadSimpleParticleInternal pos dir zeroVel2 zIndex angle scale packSprFilePath

loadSimpleParticleWithVel
    :: (FileCache m, GraphicsRead m, MonadIO m)
    => Pos2
    -> Direction
    -> Vel2
    -> ZIndex
    -> PackResourceFilePath
    -> m (Some Particle)
loadSimpleParticleWithVel pos dir vel zIndex packSprFilePath =
    loadSimpleParticleInternal pos dir vel zIndex 0.0 NonScaled packSprFilePath

loadSimpleParticleInternal
    :: (FileCache m, GraphicsRead m, MonadIO m)
    => Pos2
    -> Direction
    -> Vel2
    -> ZIndex
    -> Radians
    -> DrawScale
    -> PackResourceFilePath
    -> m (Some Particle)
loadSimpleParticleInternal pos dir vel zIndex angle scale packSprFilePath = do
    spr <- loadPackSprite packSprFilePath
    let
        simpleData = SimpleData
            { _dir    = dir
            , _zIndex = zIndex
            , _scale  = scale
            , _sprite = spr
            }

    return . Some $ (mkParticle simpleData pos fakeAliveSecs)
        { _vel    = vel
        , _angle  = angle
        , _draw   = draw
        , _update = update
        }


draw :: (GraphicsReadWrite m, MonadIO m) => ParticleDraw SimpleData m
draw simple = drawSpriteEx pos dir zIndex angle FullOpacity scale spr
    where
        pos        = P._pos simple
        simpleData = _data simple
        dir        = _dir simpleData
        zIndex     = _zIndex simpleData
        angle      = _angle simple
        scale      = _scale simpleData
        spr        = _sprite simpleData

update :: ParticleUpdate SimpleData
update simple = simple
    { _data = simpleData'
    , _ttl   = ttl
    }
    where
        simpleData               = _data simple
        spr                      = updateSprite $ _sprite simpleData
        simpleData'              = simpleData {_sprite = spr}
        ttl
            | spriteFinished spr = 0.0
            | otherwise          = fakeAliveSecs
