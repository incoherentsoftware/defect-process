module Configs.All.Settings.Debug
    ( DebugConfig(..)
    ) where

import Data.Aeson.Types ((.!=), (.:?), FromJSON, Parser, Value(Object), parseJSON, typeMismatch)
import GHC.Generics     (Generic)

import Game.Mode
import Util
import Window.InputState.Types

data DebugConfig = DebugConfig
    { _startingMode           :: GameMode
    , _menuQuitHotkeyEnabled  :: Bool
    , _disableMenuHelpEntries :: Bool
    , _skipPlayerSpawnAnim    :: Bool
    , _infiniteMeter          :: Bool
    , _devConsoleEnabled      :: Bool

    , _drawEntityHitboxes           :: Bool
    , _drawSurfaceHitboxes          :: Bool
    , _drawPlatformHitboxes         :: Bool
    , _drawArenaHitboxes            :: Bool
    , _drawPortalHitboxes           :: Bool
    , _drawItemHitboxes             :: Bool
    , _drawEnemyDebugText           :: Bool
    , _hideHud                      :: Bool
    , _hideTargeting                :: Bool
    , _hideCursor                   :: Bool
    , _hidePlayer                   :: Bool
    , _disableAI                    :: Bool
    , _enemiesInvincible            :: Bool
    , _playerInvincible             :: Bool
    , _playerDamageMultiplier       :: Float
    , _enemiesDamageMultiplier      :: Float
    , _disableRoomArenaWallsTrigger :: Bool
    , _disableRoomBossTrigger       :: Bool
    , _cameraDebugOffset            :: Pos2
    , _forceLastUsedInputType       :: Maybe InputType
    , _disableFileCache             :: Bool
    , _disablePauseMenuHints        :: Bool
    , _disableEnemyHurtSfx          :: Bool
    , _disablePlayerTaunt           :: Bool
    }
    deriving Generic

instance FromJSON DebugConfig where
    parseJSON :: Value -> Parser DebugConfig
    parseJSON (Object v) =
        DebugConfig <$>
        v .:? "startingMode" .!= MainMenuMode <*>
        v .:? "menuQuitHotkeyEnabled" .!= False <*>
        v .:? "disableMenuHelpEntries" .!= False <*>
        v .:? "skipPlayerSpawnAnim" .!= False <*>
        v .:? "infiniteMeter" .!= False <*>
        v .:? "devConsoleEnabled" .!= False <*>
        v .:? "drawEntityHitboxes" .!= False <*>
        v .:? "drawSurfaceHitboxes" .!= False <*>
        v .:? "drawPlatformHitboxes" .!= False <*>
        v .:? "drawArenaHitboxes" .!= False <*>
        v .:? "drawPortalHitboxes" .!= False <*>
        v .:? "drawItemHitboxes" .!= False <*>
        v .:? "drawEnemyDebugText" .!= False <*>
        v .:? "hideHud" .!= False <*>
        v .:? "hideTargeting" .!= False <*>
        v .:? "hideCursor" .!= False <*>
        v .:? "hidePlayer" .!= False <*>
        v .:? "disableAI" .!= False <*>
        v .:? "enemiesInvincible" .!= False <*>
        v .:? "playerInvincible" .!= False <*>
        v .:? "playerDamageMultiplier" .!= 1.0 <*>
        v .:? "enemiesDamageMultiplier" .!= 1.0 <*>
        v .:? "disableRoomArenaWallsTrigger" .!= False <*>
        v .:? "disableRoomBossTrigger" .!= False <*>
        v .:? "cameraDebugOffset" .!= zeroPos2 <*>
        v .:? "forceLastUsedInputType" <*>
        v .:? "disableFileCache" .!= False <*>
        v .:? "disablePauseMenuHints" .!= False <*>
        v .:? "disableEnemyHurtSfx" .!= False <*>
        v .:? "disablePlayerTaunt" .!= False
    parseJSON value      = typeMismatch "DebugConfig" value
