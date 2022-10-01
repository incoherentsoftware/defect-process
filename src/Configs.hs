module Configs
    ( Configs(..)
    , ConfigsRead(..)
    , loadConfig
    , mkConfigs
    , readSettingsConfig
    , readEnemyConfig
    , readEnemyLockOnConfig
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Yaml              (FromJSON, decodeFileEither)

import Configs.All.Enemy
import Configs.All.EnemyLockOn
import Configs.All.Level
import Configs.All.Particle
import Configs.All.Player
import Configs.All.PlayerGun
import Configs.All.PlayerSkill
import Configs.All.PlayerWeapon
import Configs.All.Progress
import Configs.All.Settings
import Util

data Configs = Configs
    { _settings     :: SettingsConfig
    , _player       :: PlayerConfig
    , _playerWeapon :: PlayerWeaponConfig
    , _playerGun    :: PlayerGunConfig
    , _playerSkill  :: PlayerSkillConfig
    , _enemy        :: EnemyConfig
    , _enemyLockOn  :: EnemyLockOnConfig
    , _level        :: LevelConfig
    , _particle     :: ParticleConfig
    , _progress     :: ProgressConfig
    }

class Monad m => ConfigsRead m where
    readConfig  :: (Configs -> a) -> (a -> b) -> m b
    readConfigs :: m Configs

loadConfig :: MonadIO m => FromJSON a => FileName -> m a
loadConfig fileName = do
    filePath <- translateResourcePath $ "data/configs/" ++ fileName
    liftIO (decodeFileEither filePath) >>= \case
        Left e    -> error $ "Error loading " ++ filePath ++ ": " ++ show e
        Right cfg -> return cfg

mkConfigs :: MonadIO m => m Configs
mkConfigs =
    Configs <$>
    loadConfig "settings.cfg" <*>
    loadConfig "player.cfg" <*>
    loadConfig "player-weapon.cfg" <*>
    loadConfig "player-gun.cfg" <*>
    loadConfig "player-skill.cfg" <*>
    loadConfig "enemy.cfg" <*>
    loadConfig "enemy-lock-on.cfg" <*>
    loadConfig "level.cfg" <*>
    loadConfig "particle.cfg" <*>
    loadConfig "progress.cfg"

readSettingsConfig :: ConfigsRead m => (SettingsConfig -> a) -> (a -> b) -> m b
readSettingsConfig cfg f = readConfig (cfg . _settings) f

readEnemyConfig :: ConfigsRead m => (EnemyConfig -> a) -> m a
readEnemyConfig f = readConfig _enemy f

readEnemyLockOnConfig :: ConfigsRead m => (EnemyLockOnConfig -> a) -> m a
readEnemyLockOnConfig f = readConfig _enemyLockOn f
