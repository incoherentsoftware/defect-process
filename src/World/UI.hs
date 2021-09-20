module World.UI
    ( WorldUI
    , mkWorldUI
    , updateWorldUI
    , drawWorldUI
    , resetWorldUIOnChangeWorldRoom
    ) where

import Control.Monad.IO.Class (MonadIO)

import Configs
import Configs.All.Settings
import Configs.All.Settings.Debug
import FileCache
import Msg
import Player
import Util
import Window.Graphics
import Window.InputState
import World.UI.Gold
import World.UI.Health
import World.UI.Icons
import World.UI.InfoText
import World.UI.Meter
import World.UI.Types
import World.UI.Voiceover

mkWorldUI :: (FileCache m, GraphicsRead m, InputRead m, MonadIO m) => m WorldUI
mkWorldUI =
    WorldUI <$>
    mkHealthUI <*>
    mkMeterUI <*>
    mkIconsUI <*>
    mkGoldUI <*>
    mkInfoTextUI <*>
    mkVoiceoverUI

updateWorldUI
    :: (ConfigsRead m, FileCache m, GraphicsRead m, InputRead m, MonadIO m, MsgsReadWrite UpdateWorldUiMsgsPhase m)
    => Player
    -> WorldUI
    -> m WorldUI
updateWorldUI player worldUI =
    let
        updateWorldUiHealthUI :: ConfigsRead m1 => WorldUI -> m1 WorldUI
        updateWorldUiHealthUI ui = do
            healthUI <- updateHealthUI player (_healthUI ui)
            return $ ui {_healthUI = healthUI}

        updateWorldUiMeterUI :: MsgsReadWrite UpdateWorldUiMsgsPhase m1 => WorldUI -> m1 WorldUI
        updateWorldUiMeterUI ui = do
            meterUI <- updateMeterUI player (_meterUI ui)
            return $ ui {_meterUI = meterUI}

        updateWorldUiIconsUI :: MsgsReadWrite UpdateWorldUiMsgsPhase m1 => WorldUI -> m1 WorldUI
        updateWorldUiIconsUI ui = do
            iconsUI <- updateIconsUI $ _iconsUI ui
            return $ ui {_iconsUI = iconsUI}

        updateWorldUiInfoTextUI
            :: (FileCache m1, GraphicsRead m1, InputRead m1, MonadIO m1, MsgsRead UpdateWorldUiMsgsPhase m1)
            => WorldUI
            -> m1 WorldUI
        updateWorldUiInfoTextUI ui = do
            infoTextUI <- updateInfoTextUI $ _infoTextUI ui
            return $ ui {_infoTextUI = infoTextUI}

        updateWorldUiVoiceoverUI :: MsgsReadWrite UpdateWorldUiMsgsPhase m1 => WorldUI -> m1 WorldUI
        updateWorldUiVoiceoverUI ui = do
            voiceoverUI <- updateVoiceoverUI $ _voiceoverUI ui
            return $ ui {_voiceoverUI = voiceoverUI}

        updateWorldUiGoldUI
            :: (ConfigsRead m1, FileCache m1, GraphicsRead m1, MonadIO m1, MsgsRead UpdateWorldUiMsgsPhase m1)
            => WorldUI
            -> m1 WorldUI
        updateWorldUiGoldUI ui = do
            goldUI <- updateGoldUI player (_goldUI ui)
            return $ ui {_goldUI = goldUI}
    in
        updateWorldUiHealthUI worldUI >>=
        updateWorldUiMeterUI >>=
        updateWorldUiIconsUI >>=
        updateWorldUiGoldUI >>=
        updateWorldUiInfoTextUI >>=
        updateWorldUiVoiceoverUI

drawWorldUI :: (ConfigsRead m, GraphicsReadWrite m, InputRead m, MonadIO m) => Player -> WorldUI -> m ()
drawWorldUI player worldUI = unlessM (readSettingsConfig _debug _hideHud) $ do
    setCameraSpace CameraScreenSpace

    drawHealthUI player (_healthUI worldUI)
    drawMeterUI player (_meterUI worldUI)
    drawIconsUI player (_iconsUI worldUI)
    drawGoldUI $ _goldUI worldUI
    drawInfoTextUI $ _infoTextUI worldUI
    drawVoiceoverUI $ _voiceoverUI worldUI

resetWorldUIOnChangeWorldRoom :: WorldUI -> WorldUI
resetWorldUIOnChangeWorldRoom worldUI = worldUI
    { _infoTextUI = resetInfoTextUIOnChangeWorldRoom $ _infoTextUI worldUI
    }
