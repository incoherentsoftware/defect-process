module Player.Taunts
    ( PlayerTaunts(..)
    , mkPlayerTaunts
    ) where

import Control.Monad.IO.Class (MonadIO)

import Attack.Description
import FileCache
import Window.Graphics

packPath = \f -> PackResourceFilePath "data/player/player-movement.pack" f

data PlayerTaunts = PlayerTaunts
    { _taunt :: AttackDescription
    }

mkPlayerTaunts :: (FileCache m, GraphicsRead m, MonadIO m) => m PlayerTaunts
mkPlayerTaunts =
    PlayerTaunts <$>
    loadPackAtkDesc "taunt.atk"
    where loadPackAtkDesc = \f -> loadPackAttackDescription $ packPath f

