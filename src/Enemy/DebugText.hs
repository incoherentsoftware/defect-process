module Enemy.DebugText
    ( EnemyDebugText
    , mkEnemyDebugText
    , updateEnemyDebugText
    , drawEnemyDebugText
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack.Util
import Configs
import Configs.All.Settings
import Configs.All.Settings.Debug
import Util
import Window.Graphics
import World.ZIndex

data EnemyDebugText = EnemyDebugText
    { _healthDisplayText :: DisplayText
    }

mkEnemyDebugText :: (GraphicsRead m, MonadIO m) => Health -> m EnemyDebugText
mkEnemyDebugText health = do
    healthText <- mkDisplayText (prettyShow health) Font12 whiteColor
    return $ EnemyDebugText {_healthDisplayText  = healthText}

updateEnemyDebugText :: Health -> EnemyDebugText -> EnemyDebugText
updateEnemyDebugText health debugText = EnemyDebugText {_healthDisplayText = healthDispText}
    where healthDispText = updateDisplayText (prettyShow health) (_healthDisplayText debugText)

drawEnemyDebugText :: (ConfigsRead m, GraphicsReadWrite m, MonadIO m) => Pos2 -> EnemyDebugText -> m ()
drawEnemyDebugText pos debugText =
    whenM (readSettingsConfig _debug _drawEnemyDebugText) $
        drawDisplayText pos enemyDebugTextZIndex (_healthDisplayText debugText)
