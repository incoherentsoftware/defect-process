module Level.Room.Util
    ( defaultRoomMinCameraY
    , startingShopRoomType
    , endRoomType
    , endHallwayRoomType
    , isArenaRoomType
    , isTransitionRoomType
    , isToTransitionRoomType
    , isChallengeRoomType
    , roomTypeToFilePath
    , roomFilePathToType
    , roomTypeToRoomName
    , roomPortalHitboxes
    , isRoomPortalBarrierPlayerClose
    ) where

import Data.Maybe      (fromMaybe)
import System.FilePath ((<.>), (</>), takeBaseName)
import qualified Data.List as L
import qualified Data.Text as T

import Collision.Hitbox.Types
import Level.Room.Portal.Manager
import Level.Room.Types
import Util

defaultRoomMinCameraY = -9999                           :: PosY
startingShopRoomType  = SpecialRoomType "starting-shop" :: RoomType
endRoomType           = SpecialRoomType "end"           :: RoomType
endHallwayRoomType    = SpecialRoomType "end-hallway"   :: RoomType

isArenaRoomType :: RoomType -> Bool
isArenaRoomType = \case
    ArenaRoomType _ -> True
    _               -> False

isTransitionRoomType :: RoomType -> Bool
isTransitionRoomType = \case
    FromTransitionRoomType _ -> True
    ToTransitionRoomType _   -> True
    _                      -> False

isToTransitionRoomType :: RoomType -> Bool
isToTransitionRoomType = \case
    ToTransitionRoomType _ -> True
    _                      -> False

isChallengeRoomType :: RoomType -> Bool
isChallengeRoomType = \case
    ChallengeRoomType _ -> True
    _                   -> False

roomTypeToFilePath :: RoomType -> FilePath
roomTypeToFilePath = \case
    EmptyRoomType               -> "empty"
    NextRoomType                -> "next"
    ArenaRoomType name          ->
        let name' = T.unpack name
        in "data/levels/arenas" </> name' </> name' <.> "room"
    FromTransitionRoomType name ->
        let name' = T.unpack name
        in "data/levels/from-transitions" </> name' </> "from-" ++ name' <.> "room"
    ToTransitionRoomType name   ->
        let name' = T.unpack name
        in "data/levels/to-transitions" </> name' </> "to-" ++ name' <.> "room"
    ChallengeRoomType name      ->
        let name' = T.unpack name
        in "data/levels/challenges" </> name' </> name' <.> "room"
    SpecialRoomType name        ->
        let name' = T.unpack name
        in "data/levels/special" </> name' </> name' <.> "room"

roomFilePathToType :: FilePath -> RoomType
roomFilePathToType = \case
    "empty"  -> EmptyRoomType
    "next"   -> NextRoomType
    filePath ->
        let
            isPrefixOf'  = \s -> s `L.isPrefixOf` filePath
            name         = T.pack $ takeBaseName filePath
            stripPrefix' = \s -> fromMaybe s (T.stripPrefix s name)
        in if
            | isPrefixOf' "data/levels/to-transitions"   -> ToTransitionRoomType $ stripPrefix' "to-"
            | isPrefixOf' "data/levels/from-transitions" -> FromTransitionRoomType $ stripPrefix' "from-"
            | isPrefixOf' "data/levels/challenges"       -> ChallengeRoomType name
            | isPrefixOf' "data/levels/special"          -> SpecialRoomType name
            | otherwise                                  -> ArenaRoomType name

roomTypeToRoomName :: RoomType -> RoomName
roomTypeToRoomName = \case
    EmptyRoomType               -> "empty"
    NextRoomType                -> "next"
    ArenaRoomType name          -> name
    FromTransitionRoomType name -> name
    ToTransitionRoomType name   -> name
    ChallengeRoomType name      -> name
    SpecialRoomType name        -> name

roomPortalHitboxes :: Room -> [Hitbox]
roomPortalHitboxes room = maybe [] roomPortalManagerHitboxes (_portalManager room)

isRoomPortalBarrierPlayerClose :: Room -> Bool
isRoomPortalBarrierPlayerClose room = maybe False isRoomPortalManagerBarrierPlayerClose (_portalManager room)
