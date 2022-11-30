module Enemy.All.Boss.Images
    ( EnemyImages(..)
    , mkEnemyImages
    ) where

import Control.Monad.IO.Class (MonadIO)

import FileCache
import Window.Graphics

data EnemyImages = EnemyImages
    { _healthbarBackdrop :: Image
    , _healthbarInner    :: Image
    }

mkEnemyImages :: (FileCache m, GraphicsRead m, MonadIO m) => m EnemyImages
mkEnemyImages =
    EnemyImages <$>
    loadPackImg "healthbar-backdrop.image" <*>
    loadPackImg "healthbar-inner.image"
    where loadPackImg = \f -> loadPackImage $ PackResourceFilePath "data/enemies/boss-enemy.pack" f
