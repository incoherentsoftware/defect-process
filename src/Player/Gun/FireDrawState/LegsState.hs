module Player.Gun.FireDrawState.LegsState
    ( module Player.Gun.FireDrawState.LegsState.Types
    , mkLegsSprites
    , mkLegsState
    , updateLegsState
    , resetLegsState
    ) where

import Control.Monad.IO.Class (MonadIO)

import FileCache
import Player.Flags
import Player.Gun.FireDrawState.LegsState.Types
import Player.Types
import Util
import Window.Graphics

mkLegsSprites :: (FileCache m, GraphicsRead m, MonadIO m) => m LegsSprites
mkLegsSprites =
    LegsSprites <$>
    loadPackSpr "legs.spr" <*>
    loadPackSpr "air-legs.spr" <*>
    loadPackSpr "legs-walk.spr" <*>
    loadPackSpr "legs-back-walk.spr"
    where loadPackSpr = \f -> loadPackSprite $ PackResourceFilePath "data/player/player-guns.pack" f

mkLegsState :: LegsSprites -> LegsState
mkLegsState legsSprs = LegsState
    { _status  = LegsStandingGround
    , _sprite  = _stand legsSprs
    , _sprites = legsSprs
    }

legsStatus :: Player -> Direction -> LegsStatus
legsStatus player dir
    | inAir                                                         = LegsInAir
    | velX `approxEq` 0.0                                           = LegsStandingGround
    | velX < 0.0 && dir == RightDir || velX > 0.0 && dir == LeftDir = LegsBackWalkingGround
    | otherwise                                                     = LegsWalkingGround
    where
        flags = _flags (player :: Player)
        inAir = not $ _touchingGround flags
        velX  = vecX $ _vel (player :: Player)

updateLegsState :: Player -> Direction -> LegsState -> LegsState
updateLegsState player dir legsState = legsState
    { _status = status
    , _sprite = spr'
    }
    where
        status  = legsStatus player dir
        sprites = _sprites (legsState :: LegsState)
        newSpr  = case status of
            LegsStandingGround    -> _stand sprites
            LegsInAir             -> _air sprites
            LegsWalkingGround     -> _walk sprites
            LegsBackWalkingGround -> _backWalk sprites

        spr                 = _sprite (legsState :: LegsState)
        spr'
            | newSpr == spr = updateSprite spr
            | otherwise     = newSpr

resetLegsState :: LegsSprites -> Player -> Direction -> LegsState -> LegsState
resetLegsState legsSprs player dir legsState = legsState
    { _status  = status
    , _sprite  = spr
    , _sprites = legsSprs
    }
    where
        status = legsStatus player dir
        spr    = case status of
            LegsStandingGround -> _stand legsSprs
            LegsInAir          -> _air legsSprs

            LegsWalkingGround ->
                let
                    currentSpr = _sprite (legsState :: LegsState)
                    walkSpr    = _walk legsSprs
                in if
                    | currentSpr == walkSpr -> currentSpr
                    | otherwise             -> walkSpr

            LegsBackWalkingGround ->
                let
                    currentSpr  = _sprite (legsState :: LegsState)
                    backWalkSpr = _backWalk legsSprs
                in if
                    | currentSpr == backWalkSpr -> currentSpr
                    | otherwise                 -> backWalkSpr
