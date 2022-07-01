module World.ZIndex
    ( consoleTextZIndex
    , consoleBackgroundZIndex
    , uiFrontZIndex
    , uiZIndex
    , uiBackZIndex
    , runProgressZIndex
    , screenWipeZIndex
    , debugHitboxZIndex
    , enemyDebugTextZIndex
    , playerLockOnZIndex
    , playerAimOverlayZIndex
    , worldEffectZIndex
    , worldProjectileZIndex
    , worldBehindProjectileZIndex
    , enemyAttackProjectileZIndex
    , playerAttackEffectZIndex
    , enemyHurtParticleZIndex
    , playerGunMuzzleFlashZIndex
    , playerGunOverlayZIndex
    , playerWeaponOverlayZIndex
    , playerMovementSkillOverlayZIndex
    , bossBodyZIndex
    , bossUnderBodyZIndex
    , playerOverBodyZIndex
    , playerBodyZIndex
    , playerOverSpecialLegsZIndex
    , playerSpecialLegsZIndex
    , playerUnderBodyZIndex
    , enemyOverBodyZIndex
    , enemyBodyZIndex
    , enemyBodyTallZIndex
    , enemyBodyBigZIndex
    , enemyUnderBodyZIndex
    , uiInfoTextZIndex
    , goldDropZIndex
    , levelItemLabelZIndex
    , levelItemZIndex
    , levelArenaWallsZIndex
    , levelImageLayerZIndex
    , levelFgImageLayerZIndices
    , levelBgImageLayerZIndices
    ) where

import qualified Data.Set as S

import Window.Graphics.Util

consoleTextZIndex       = ZIndex 0 :: ZIndex
consoleBackgroundZIndex = ZIndex 1 :: ZIndex

uiFrontZIndex     = ZIndex 10 :: ZIndex
uiZIndex          = ZIndex 11 :: ZIndex
uiBackZIndex      = ZIndex 12 :: ZIndex
runProgressZIndex = ZIndex 13 :: ZIndex
screenWipeZIndex  = ZIndex 15 :: ZIndex

debugHitboxZIndex    = ZIndex 20 :: ZIndex
enemyDebugTextZIndex = ZIndex 21 :: ZIndex

playerLockOnZIndex     = ZIndex 30 :: ZIndex
playerAimOverlayZIndex = ZIndex 31 :: ZIndex

worldEffectZIndex           = ZIndex 40 :: ZIndex
worldProjectileZIndex       = ZIndex 41 :: ZIndex
worldBehindProjectileZIndex = ZIndex 42 :: ZIndex

enemyAttackProjectileZIndex = ZIndex 50 :: ZIndex
playerAttackEffectZIndex    = ZIndex 51 :: ZIndex
enemyHurtParticleZIndex     = ZIndex 52 :: ZIndex

playerGunMuzzleFlashZIndex       = ZIndex 60 :: ZIndex
playerGunOverlayZIndex           = ZIndex 61 :: ZIndex
playerWeaponOverlayZIndex        = ZIndex 62 :: ZIndex
playerMovementSkillOverlayZIndex = ZIndex 63 :: ZIndex

bossBodyZIndex      = ZIndex 65 :: ZIndex
bossUnderBodyZIndex = ZIndex 66 :: ZIndex

playerOverBodyZIndex        = ZIndex 70 :: ZIndex
playerBodyZIndex            = ZIndex 71 :: ZIndex
playerOverSpecialLegsZIndex = ZIndex 72 :: ZIndex
playerSpecialLegsZIndex     = ZIndex 73 :: ZIndex
playerUnderBodyZIndex       = ZIndex 74 :: ZIndex
enemyOverBodyZIndex         = ZIndex 75 :: ZIndex
enemyBodyZIndex             = ZIndex 76 :: ZIndex
enemyBodyTallZIndex         = ZIndex 77 :: ZIndex
enemyBodyBigZIndex          = ZIndex 78 :: ZIndex
enemyUnderBodyZIndex        = ZIndex 79 :: ZIndex

uiInfoTextZIndex      = ZIndex 80  :: ZIndex
goldDropZIndex        = ZIndex 81  :: ZIndex
levelItemLabelZIndex  = ZIndex 82  :: ZIndex
levelItemZIndex       = ZIndex 83  :: ZIndex
levelArenaWallsZIndex = ZIndex 110 :: ZIndex

-- [90, 130] reserved for image layers
levelImageLayerZIndex     = ZIndex 110                         :: ZIndex
levelFgImageLayerZIndices = S.fromList (map ZIndex [90..109])  :: S.Set ZIndex
levelBgImageLayerZIndices = S.fromList (map ZIndex [111..130]) :: S.Set ZIndex
