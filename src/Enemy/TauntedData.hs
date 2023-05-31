module Enemy.TauntedData
    ( EnemyTauntedStatus(..)
    , EnemyTauntedData(..)
    , mkEnemyTauntedData
    , activateEnemyTauntedData
    , updateEnemyTauntedData
    , drawEnemyTauntedData
    ) where

import Control.Monad.IO.Class (MonadIO)

import Enemy.LockOnData
import FileCache
import Util
import Window.Graphics
import World.ZIndex

underlaySpritePath =
    PackResourceFilePath "data/particles/particles-enemy.pack" "enemy-taunted-underlay.spr" :: PackResourceFilePath

data EnemyTauntedStatus
    = EnemyTauntedActive
    | EnemyTauntedInactive

data EnemyTauntedData = EnemyTauntedData
    { _status            :: EnemyTauntedStatus
    , _prevStatus        :: EnemyTauntedStatus
    , _underlaySprite    :: Sprite
    , _underlayDrawScale :: DrawScale
    }

mkEnemyTauntedData :: (FileCache m, GraphicsRead m, MonadIO m) => DrawScale -> m EnemyTauntedData
mkEnemyTauntedData underlayDrawScale = do
    spr <- loadPackSprite underlaySpritePath
    return $ EnemyTauntedData
        { _status            = EnemyTauntedInactive
        , _prevStatus        = EnemyTauntedInactive
        , _underlaySprite    = spr
        , _underlayDrawScale = underlayDrawScale
        }

activateEnemyTauntedData :: EnemyTauntedData -> EnemyTauntedData
activateEnemyTauntedData tauntedData = tauntedData {_status = EnemyTauntedActive}

updateEnemyTauntedData :: EnemyTauntedData -> EnemyTauntedData
updateEnemyTauntedData tauntedData = tauntedData
    { _prevStatus     = _status tauntedData
    , _underlaySprite = updateSprite $ _underlaySprite tauntedData
    }

drawEnemyTauntedData
    :: (GraphicsReadWrite m, MonadIO m)
    => Pos2
    -> Direction
    -> EnemyLockOnData
    -> EnemyTauntedData
    -> m ()
drawEnemyTauntedData pos dir lockOnData tauntedData = case _status tauntedData of
    EnemyTauntedInactive -> return ()
    EnemyTauntedActive   ->
        let
            pos'  = pos `vecAdd` _reticleOffset lockOnData
            scale = _underlayDrawScale tauntedData
            spr   = _underlaySprite tauntedData
        in drawSpriteScaled pos' dir enemyUnderBodyZIndex scale spr
