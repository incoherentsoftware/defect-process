module World.Draw
    ( drawWorld
    ) where

import AppEnv
import Configs
import Configs.All.Settings
import Configs.All.Settings.Debug
import Enemy.Manager
import Game.Types
import Level
import Level.Room
import Msg.Phase
import Particle.Manager
import Player.Draw
import Projectile.Manager
import Window
import World
import World.Camera
import World.RunProgressScreen
import World.ScreenWipe
import World.Screenshake
import World.UI

drawWorld :: Game -> AppEnv DrawMsgsPhase ()
drawWorld game =
    let
        world             = _world (game :: Game)
        player            = _player (world :: World)
        enemyManager      = _enemyManager world
        level             = _level (world :: World)
        room              = _room level
        projectileManager = _projectileManager world
        particleManager   = _particleManager world
        ui                = _ui (world :: World)
        screenshake       = _screenshake world
    in do
        setCameraSpace CameraWorldSpace

        isMouseKbLastUsed <- (== MouseKbInputType) . _lastUsedInputType <$> readInputState
        isHideCursor      <- readSettingsConfig _debug _hideCursor
        showCursor $ isMouseKbLastUsed && not isHideCursor

        drawWorldCamera player room (_camera world)
        drawScreenshake screenshake

        drawRoom room
        drawEnemyManager enemyManager
        drawPlayer player
        drawProjectileManager projectileManager
        drawParticleManager particleManager

        drawWorldUI player ui

        case _screenWipe world of
            Nothing         -> return ()
            Just screenWipe -> do
                drawWorldScreenWipe screenWipe
                drawRunProgressScreen screenWipe (_roomChooser level) (_runProgressScreen world)
