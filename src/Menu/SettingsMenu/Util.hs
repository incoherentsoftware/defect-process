module Menu.SettingsMenu.Util
    ( SettingsTabButtons(..)
    , mkSettingsTabButtons
    ) where

import Control.Monad.IO.Class (MonadIO)

import Configs
import Configs.All.Settings
import Configs.All.Settings.Menu
import FileCache
import Util
import Window.Graphics
import Window.Graphics.UiControls

inactiveFileNameSuffix = "-inactive.image" :: FileName
selectedFileNameSuffix = "-selected.image" :: FileName

data SettingsTabButtons = SettingsTabButtons
    { _controlsButton :: Button
    , _graphicsButton :: Button
    , _audioButton    :: Button
    , _creditsButton  :: Button
    }

mkSettingsTabButtons
    :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m)
    => PackResourceFilePath
    -> PackResourceFilePath
    -> PackResourceFilePath
    -> PackResourceFilePath
    -> m SettingsTabButtons
mkSettingsTabButtons controlsBtnImgPath graphicsBtnImgPath audioBtnImgPath creditsBtnImgPath =
    SettingsTabButtons <$>
    mkImageButton' _settingsControlsButtonPos controlsBtnImgPath <*>
    mkImageButton' _settingsGraphicsButtonPos graphicsBtnImgPath <*>
    mkImageButton' _settingsAudioButtonPos audioBtnImgPath <*>
    mkImageButton' _settingsCreditsButtonPos creditsBtnImgPath
    where
        mkImageButton' = \posF btnImgPath -> do
            pos <- posF <$> readConfig _settings (_menu :: SettingsConfig -> MenuConfig)

            case stripSuffix inactiveFileNameSuffix (_fileName btnImgPath) of
                Nothing           -> mkImageButton pos btnImgPath
                Just baseFileName ->
                    let btnSelectedImgPath = btnImgPath {_fileName = baseFileName ++ selectedFileNameSuffix}
                    in mkImageButtonEx pos btnImgPath btnSelectedImgPath
