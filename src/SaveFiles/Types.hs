module SaveFiles.Types
    ( SaveFilesSettings(..)
    , SaveFilesProgress(..)
    ) where

import Data.Aeson.Types ((.!=), (.:), (.:?), FromJSON, ToJSON, Parser, Value(Object))
import Data.Aeson.Types (genericToJSON, parseJSON, toJSON, typeMismatch)
import GHC.Generics     (Generic)
import qualified Data.Set as S
import qualified Data.Text as T

import Audio.Volume
import Menu.SettingsMenu.Util
import Player.Gun.Types
import Player.MovementSkill.Types
import Player.SecondarySkill.Types
import Player.Weapon.Types
import Util
import Window.Graphics.Util
import Window.InputState.Alias
import World.Audio.LayeredMusic.Types
import World.Util

data SaveFilesSettings = SaveFilesSettings
    { _version                      :: T.Text
    , _controlsInputAliasRawDataMap :: InputAliasRawDataMap
    , _renderWinWidth               :: Int
    , _renderWinHeight              :: Int
    , _renderWinMode                :: WindowMode
    , _renderWinDisplayIndex        :: Int
    , _audioSoundVolume             :: Volume
    , _audioMusicVolume             :: Volume
    , _audioBattleMusic             :: LayeredMusicType
    , _audioExplorationMusic        :: LayeredMusicType
    , _gameEnemyHealthPercent       :: EnemyHealthPercent
    , _gamePauseMenuHints           :: Bool
    }
    deriving Generic

instance FromJSON SaveFilesSettings where
    parseJSON :: Value -> Parser SaveFilesSettings
    parseJSON value@(Object v) =
        let
            version = v .:? "version" >>= \case
                Just vsn -> return vsn
                Nothing  -> v .:? "gameVersion" >>= \case
                    Nothing  -> typeMismatch "SaveFilesSettings" value
                    Just vsn -> return vsn
        in
            SaveFilesSettings <$>
            version <*>
            v .: "controlsInputAliasRawDataMap" <*>
            v .: "renderWinWidth" <*>
            v .: "renderWinHeight" <*>
            v .: "renderWinMode" <*>
            v .: "renderWinDisplayIndex" <*>
            v .: "audioSoundVolume" <*>
            v .: "audioMusicVolume" <*>
            v .:? "audioBattleMusic" .!= BattleAMusic <*>
            v .:? "audioExplorationMusic" .!= ExploreAMusic <*>
            v .:? "gameEnemyHealthPercent" .!= EnemyHealth100Percent <*>
            v .:? "gamePauseMenuHints" .!= True
    parseJSON value = typeMismatch "SaveFilesSettings" value

instance ToJSON SaveFilesSettings where
    toJSON = genericToJSON aesonFieldDropUnderscore

data SaveFilesProgress = SaveFilesProgress
    { _version                 :: T.Text
    , _totalGold               :: GoldValue
    , _unlockedMusic           :: S.Set LayeredMusicType
    , _unlockedWeapons         :: S.Set WeaponType
    , _unlockedGuns            :: S.Set GunType
    , _unlockedMovementSkills  :: S.Set MovementSkillType
    , _unlockedSecondarySkills :: S.Set SecondarySkillType
    }
    deriving Generic

instance FromJSON SaveFilesProgress where
    parseJSON :: Value -> Parser SaveFilesProgress
    parseJSON value@(Object v) =
        let
            version = v .:? "version" >>= \case
                Just vsn -> return vsn
                Nothing  -> v .:? "gameVersion" >>= \case
                    Nothing  -> typeMismatch "SaveFilesProgress" value
                    Just vsn -> return vsn
        in
            SaveFilesProgress <$>
            version <*>
            v .: "totalGold" <*>
            v .: "unlockedMusic" <*>
            v .: "unlockedWeapons" <*>
            v .: "unlockedGuns" <*>
            v .: "unlockedMovementSkills" <*>
            v .: "unlockedSecondarySkills"
    parseJSON value = typeMismatch "SaveFilesProgress" value

instance ToJSON SaveFilesProgress where
    toJSON = genericToJSON aesonFieldDropUnderscore
