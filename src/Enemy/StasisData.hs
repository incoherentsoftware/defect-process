module Enemy.StasisData
    ( EnemyStasisData(..)
    , mkEnemyStasisData
    , updateEnemyStasisData
    , isEnemyStasisDataInStasis
    , isEnemyStasisDataDrawStasis
    , enemyStasisDataSoundMessage
    ) where

import Control.Monad.IO.Class (MonadIO)

import Constants
import Enemy.StasisData.Types
import Id
import Msg
import Util

stasisSoundPath = "event:/SFX Events/Player/Skills/stasis-effect-c" :: FilePath

stasisDrawBlinkOffTtlRanges =
    [ (0.125, 0.25)
    , (0.375, 0.5)
    , (0.625, 0.75)
    ] :: [(Secs, Secs)]

epsilon = 0.001 :: Float

mkEnemyStasisData :: MonadIO m => m EnemyStasisData
mkEnemyStasisData = do
    stasisSoundHashedId <- hashId <$> newId
    return $ EnemyStasisData
        { _stasisTtl           = 0.0
        , _deferredAttackHits  = []
        , _stasisSoundHashedId = stasisSoundHashedId
        }

updateEnemyStasisData :: EnemyStasisData -> EnemyStasisData
updateEnemyStasisData enemyStasisData = enemyStasisData {_stasisTtl = stasisTtl'}
    where
        stasisTtl  = max 0.0 (_stasisTtl enemyStasisData - timeStep)
        stasisTtl' = if
            | approxEqEx stasisTtl 0.0 epsilon -> 0.0
            | otherwise                        -> stasisTtl

isEnemyStasisDataInStasis :: EnemyStasisData -> Bool
isEnemyStasisDataInStasis = (> 0.0) . _stasisTtl

isEnemyStasisDataDrawStasis :: EnemyStasisData -> Bool
isEnemyStasisDataDrawStasis enemyStasisData = isStasis && not isDrawBlinkOff
    where
        isStasis       = isEnemyStasisDataInStasis enemyStasisData
        stasisTtl      = _stasisTtl enemyStasisData
        isDrawBlinkOff = or
            [ stasisTtl >= low && stasisTtl < high
            | (low, high) <- stasisDrawBlinkOffTtlRanges
            ]

enemyStasisDataSoundMessage :: AllowMsgWrite p AudioMsgPayload => Pos2 -> EnemyStasisData -> Msg p
enemyStasisDataSoundMessage pos enemyStasisData = mkMsg $ AudioMsgPlaySoundContinuous stasisSoundPath soundHashedId pos
    where soundHashedId = _stasisSoundHashedId enemyStasisData
