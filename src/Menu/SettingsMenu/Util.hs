module Menu.SettingsMenu.Util
    ( SettingsTabButtons(..)
    , mkSettingsTabButtons
    , EnemyHealthPercent(..)
    , enemyHealthPercentToDamageMultiplier
    , enemyDamageMultiplierToHealthPercent
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson.Types       (FromJSON, ToJSON)
import GHC.Generics           (Generic)

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
    , _gameButton     :: Button
    , _creditsButton  :: Button
    }

mkSettingsTabButtons
    :: (ConfigsRead m, FileCache m, GraphicsRead m, MonadIO m)
    => PackResourceFilePath
    -> PackResourceFilePath
    -> PackResourceFilePath
    -> PackResourceFilePath
    -> PackResourceFilePath
    -> m SettingsTabButtons
mkSettingsTabButtons controlsBtnImgPath graphicsBtnImgPath audioBtnImgPath gameBtnImgPath creditsBtnImgPath =
    SettingsTabButtons <$>
    mkImageButton' _settingsControlsButtonPos controlsBtnImgPath <*>
    mkImageButton' _settingsGraphicsButtonPos graphicsBtnImgPath <*>
    mkImageButton' _settingsAudioButtonPos audioBtnImgPath <*>
    mkImageButton' _settingsGameButtonPos gameBtnImgPath <*>
    mkImageButton' _settingsCreditsButtonPos creditsBtnImgPath
    where
        mkImageButton' = \posF btnImgPath -> do
            pos <- posF <$> readConfig _settings (_menu :: SettingsConfig -> MenuConfig)

            case stripSuffix inactiveFileNameSuffix (_fileName btnImgPath) of
                Nothing           -> mkImageButton pos btnImgPath
                Just baseFileName ->
                    let btnSelectedImgPath = btnImgPath {_fileName = baseFileName ++ selectedFileNameSuffix}
                    in mkImageButtonEx pos btnImgPath btnSelectedImgPath

data EnemyHealthPercent
    = EnemyHealth100Percent
    | EnemyHealth150Percent
    | EnemyHealth200Percent
    | EnemyHealth300Percent
    | EnemyHealth500Percent
    deriving (Bounded, Enum, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

enemyHealthPercentToDamageMultiplier :: EnemyHealthPercent -> Float
enemyHealthPercentToDamageMultiplier = \case
    EnemyHealth100Percent -> 1.0
    EnemyHealth150Percent -> 0.666
    EnemyHealth200Percent -> 0.5
    EnemyHealth300Percent -> 0.333
    EnemyHealth500Percent -> 0.2

enemyDamageMultiplierToHealthPercent :: Float -> EnemyHealthPercent
enemyDamageMultiplierToHealthPercent mult
    | mult `approxEq` 0.666 = EnemyHealth150Percent
    | mult `approxEq` 0.5   = EnemyHealth200Percent
    | mult `approxEq` 0.333 = EnemyHealth300Percent
    | mult `approxEq` 0.2   = EnemyHealth500Percent
    | otherwise             = EnemyHealth100Percent
