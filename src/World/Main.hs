module World.Main
    ( worldMain
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Functor           ((<&>))

import AppEnv
import Audio.Fmod
import Collision.Manager
import Constants
import Enemy.Manager
import Game.Types
import InfoMsg.Manager
import Level
import Menu.UnlocksMenu
import Msg.Phase
import Particle.Manager
import Player.Think
import Player.Update
import Player.Util
import Projectile.Manager
import Stats.Manager
import Window
import World
import World.Audio
import World.Camera
import World.Messages
import World.ScreenWipe
import World.Screenshake
import World.UI

updateWorld :: GameMode -> World -> AppEnv BaseMsgsPhase World
updateWorld prevGameMode world =
    let
        player          = _player world
        enemyManager    = _enemyManager world
        enemies         = _enemies enemyManager
        level           = _level world
        particleManager = _particleManager world
        worldUI         = _ui world
        statsManager    = _statsManager world
        audio           = _audio world
    in do
        withMsgsPhase @ThinkInfoMsgsPhase (thinkInfoMsgManager world)
        withMsgsPhase @ThinkPlayerMsgsPhase (thinkPlayer player)
        withMsgsPhase @ThinkEnemyMsgsPhase (thinkEnemyManager enemyManager)
        withMsgsPhase @ThinkLevelMsgsPhase (thinkLevel level)
        projectileManager <- withMsgsPhase @ThinkProjectileMsgsPhase $
            thinkProjectileManager $ _projectileManager world
        withMsgsPhase @ThinkCollisionMsgsPhase (checkCollisions player enemies (_room level) projectileManager)

        player'            <- withMsgsPhase @UpdatePlayerMsgsPhase (updatePlayer player)
        enemyManager'      <- withMsgsPhase @UpdateEnemyMsgsPhase (updateEnemyManager enemyManager)
        level'             <- withMsgsPhase @UpdateLevelMsgsPhase (updateLevel level)
        projectileManager' <- withMsgsPhase @UpdateProjectileMsgsPhase (updateProjectileManager projectileManager)
        particleManager'   <- withMsgsPhase @UpdateParticleMsgsPhase (updateParticleManager particleManager)
        worldUI'           <- withMsgsPhase @UpdateWorldUiMsgsPhase (updateWorldUI player' worldUI)
        statsManager'      <- withMsgsPhase @UpdateStatsManagerMsgsPhase (updateStatsManager prevGameMode statsManager)
        audio'             <- withMsgsPhase @UpdateAudioMsgsPhase (updateWorldAudio audio)

        screenWipe <- case _screenWipe world of
            Just sw
                | _active sw -> Just <$> updateWorldScreenWipe sw
            _                -> return Nothing

        let
            screenshake = updateScreenshake $ _screenshake world
            worldCamera = updateWorldCamera player' (_room level') (_camera world)

            world' = world
                { _player            = player'
                , _level             = level'
                , _enemyManager      = enemyManager'
                , _projectileManager = projectileManager'
                , _particleManager   = particleManager'
                , _statsManager      = statsManager'
                , _audio             = audio'
                , _ui                = worldUI'
                , _screenshake       = screenshake
                , _camera            = worldCamera
                , _screenWipe        = screenWipe
                }

        withMsgsPhase @UpdateWorldMsgsPhase (updateWorldMessages world')

worldMain :: Game -> AppEnv BaseMsgsPhase Game
worldMain game =
    let
        world = (_world game) {_status = WorldAliveStatus}

        setMenuAudioAndCursor :: (GraphicsReadWrite m, MonadIO m) => m ()
        setMenuAudioAndCursor = pauseFmodAudioWorld True >> setGraphicsCursor _menu
    in do
        menuPressed <- aliasPressed MenuAlias <$> readInputState

        if
            | menuPressed -> do
                setMenuAudioAndCursor
                return $ game {_mode = PauseMenuMode}

            | Just pendingChange <- _pendingChange world -> do
                world' <- case _screenWipe world of
                    Just screenWipe
                        | _active screenWipe -> do
                            screenWipe' <- updateWorldScreenWipe screenWipe
                            return $ world {_screenWipe = Just screenWipe'}
                    _                        -> do
                        withMsgsPhase @UpdateWorldMsgsPhase $ do
                            w'           <- pendingChange world
                            screenWipeIn <- mkWorldScreenWipeIn
                            return $ w'
                                { _screenWipe    = Just screenWipeIn
                                , _pendingChange = Nothing
                                }
                return $ game {_world = world'}

            | _levelLoadSecs world > timeStep ->
                let world' = world {_levelLoadSecs = _levelLoadSecs world - timeStep}
                in return $ game {_world = world'}

            | worldIsHitlag world -> do
                player <- withMsgsPhase @UpdatePlayerMsgsPhase (updatePlayerBufferedInputInHitlag (_player world))
                return $ game
                    { _world = world
                        { _player      = player
                        , _hitlagTtl   = _hitlagTtl world - timeStep
                        , _screenshake = updateScreenshake $ _screenshake world
                        }
                    }

            | otherwise -> do
                world'   <- updateWorld (_prevMode game) world
                gameMode <- case _status world' of
                    WorldDeadStatus -> do
                        setMenuAudioAndCursor
                        isUnlocksMenuAllAvailableUnlocked game <&> \case
                            False -> UnlocksMenuMode
                            True  -> MainMenuMode

                    _ -> return WorldMode

                return $ game
                    { _mode  = gameMode
                    , _world = world'
                    }
