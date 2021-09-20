module Attack.Sound
    ( AttackSoundContinuousData(..)
    , AttackSoundType(..)
    , AttackSound(..)
    ) where

import qualified Data.Set as S

import Window.Graphics.Util

data AttackSoundContinuousData = AttackSoundContinuousData
    { _startFrameIndex :: FrameIndex
    , _endFrameIndex   :: Maybe FrameIndex
    }

data AttackSoundType
    = AttackPlaySound FilePath FrameIndex
    | AttackPlaySounds FilePath (S.Set FrameIndex)
    | AttackPlaySoundContinuous FilePath AttackSoundContinuousData
    | AttackNoSound

data AttackSound = AttackSound
    { _type             :: AttackSoundType
    , _hitSoundFilePath :: Maybe FilePath
    }
