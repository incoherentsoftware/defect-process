module Level.Room.Chooser
    ( module Level.Room.Chooser.Types
    , mkRoomChooser
    , nextRoomChooserRoomType
    , nextRoomChooserRoomTypeSkipTransition
    , setRoomChooserRoom
    , roomChooserRoomContentType
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Random   (MonadRandom)
import Control.Monad.State    (MonadState, evalStateT, execState, get, modify)
import Data.Bifunctor         (bimap)
import Data.Maybe             (fromMaybe, listToMaybe)
import System.Directory       (listDirectory)
import System.FilePath        (hasExtension, takeBaseName)
import System.Random.Shuffle  (shuffleM)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

import Configs
import Configs.All.Level
import Level.Room
import Level.Room.Chooser.Types
import Level.Room.Event.Types
import Util

arenasDirectoryPath     = "data/levels/arenas"     :: FilePath
challengesDirectoryPath = "data/levels/challenges" :: FilePath

firstToTransitionRoomChoices = NE.fromList
    [ GoldChunkContentType
    , GoldChunkContentType
    , GoldChunkContentType
    , GoldChunkContentType
    , NoContentType
    ]

transitionRoomChoices = NE.fromList $
    [ HealthContentType
    , HealthContentType
    , GoldChunkContentType
    , GoldChunkContentType
    , EventContentType LightningStrikeEvent
    , EventContentType BouncingBallEvent
    , EventContentType SlotMachineEvent
    , ShopContentType
    , ShopContentType
    , NoContentType
    ] :: NE.NonEmpty RoomContentType

healthOrShopRoomChoices = NE.fromList $
    [ HealthContentType
    , ShopContentType
    ] :: NE.NonEmpty RoomContentType

filterOutRoomChoices :: [RoomContentType] -> NE.NonEmpty RoomContentType -> NE.NonEmpty RoomContentType
filterOutRoomChoices removeTypes choices = NoContentType NE.:| (NE.filter (`notElem` removeTypes) choices)

isToTransitionRequiredShop :: ([RoomContentType], [RoomContentType]) -> Bool
isToTransitionRequiredShop = \case
    ((to0:_), (from0:from1:_))
        | ShopContentType `notElem` [to0, from0, from1] -> True
    _                                                   -> False

isHealthOrShop :: RoomContentType -> Bool
isHealthOrShop = \case
    HealthContentType -> True
    ShopContentType   -> True
    _                 -> False

prevRepeatedRoomEventType :: ([RoomContentType], [RoomContentType]) -> Maybe RoomEventType
prevRepeatedRoomEventType (toTrans, fromTrans) = case [eventType | EventContentType eventType <- choices] of
    (t0:t1:_)
        | t0 == t1 -> Just t0
    _              -> Nothing
    where choices = concat $ zipWith (\x y -> [x, y]) toTrans fromTrans

generateTransitionRoomContentTypes :: MonadIO m => Int -> m (V.Vector RoomContentType, V.Vector RoomContentType)
generateTransitionRoomContentTypes numArenas = bimap convert convert <$> generateNext 0 ([], [])
    where
        convert = \xs -> V.fromList $ reverse xs

        removeRepeatedEventChoice
            :: MonadState (NE.NonEmpty RoomContentType) m1
            => ([RoomContentType], [RoomContentType])
            -> m1 ()
        removeRepeatedEventChoice (toTrans, fromTrans) = case prevRepeatedRoomEventType (toTrans, fromTrans) of
            Nothing        -> return ()
            Just eventType -> modify $ filterOutRoomChoices [EventContentType eventType]

        generateNext
            :: MonadIO m1
            => Int
            -> ([RoomContentType], [RoomContentType])
            -> m1 ([RoomContentType], [RoomContentType])
        generateNext idx trans@(toTrans, fromTrans)
            | idx >= numArenas = return (toTrans, fromTrans)

            -- first
            | idx == 0 = do
                toContentType   <- randomChoice firstToTransitionRoomChoices
                fromContentType <- randomChoice $ filterOutRoomChoices [ShopContentType] transitionRoomChoices
                generateNext (idx + 1) (toContentType:toTrans, fromContentType:fromTrans)

            -- last
            | idx == numArenas - 1 = do
                toTrans' <- (:toTrans) <$> chooseMiddleToContentType

                fromContentType <- flip evalStateT transitionRoomChoices $ do
                    modify $ filterOutRoomChoices [HealthContentType]
                    removeRepeatedEventChoice (toTrans', fromTrans)
                    randomChoice =<< get

                generateNext (idx + 1) (toTrans', fromContentType:fromTrans)

            -- middle
            | otherwise = do
                toContentType <- chooseMiddleToContentType
                let toTrans'   = toContentType:toTrans

                fromContentType <- flip evalStateT transitionRoomChoices $ do
                    when (isHealthOrShop prevFromContentType && isHealthOrShop toContentType) $
                        modify $ filterOutRoomChoices [HealthContentType]
                    removeRepeatedEventChoice (toTrans', fromTrans)
                    randomChoice =<< get

                generateNext (idx + 1) (toTrans', fromContentType:fromTrans)

            where
                prevToContentType   = fromMaybe NoContentType (listToMaybe toTrans)
                prevFromContentType = fromMaybe NoContentType (listToMaybe fromTrans)

                chooseMiddleToContentType = if
                    | isToTransitionRequiredShop trans         -> return ShopContentType
                    | not (isHealthOrShop prevFromContentType) -> randomChoice healthOrShopRoomChoices
                    | otherwise                                -> flip evalStateT transitionRoomChoices $ do
                        when (isHealthOrShop prevToContentType && isHealthOrShop prevFromContentType) $
                            modify $ filterOutRoomChoices [HealthContentType, ShopContentType]
                        removeRepeatedEventChoice trans
                        randomChoice =<< get

mkRoomChooser :: (ConfigsRead m, MonadIO m, MonadRandom m) => m RoomChooser
mkRoomChooser =
    let
        listRoomNames :: MonadIO m1 => FilePath -> m1 [FilePath]
        listRoomNames dirPath = do
            dirPath' <- translateResourcePath dirPath
            subDirs  <- filter (not . hasExtension) <$> liftIO (listDirectory dirPath')
            return
                [ name
                | subDir <- subDirs
                , let name = takeBaseName subDir
                , not $ "_" `L.isPrefixOf` name
                ]
    in do
        arenaBaseFileNames     <- shuffleM =<< listRoomNames arenasDirectoryPath
        challengeBaseFileNames <- shuffleM =<< listRoomNames challengesDirectoryPath

        let
            arenaFileIds     = map (ArenaRoomType . T.pack) arenaBaseFileNames
            challengeFileIds = map (ChallengeRoomType . T.pack) challengeBaseFileNames

        maxNumArenas <- readConfig _level _maxNumArenas
        let
            numArenas     = min maxNumArenas (length arenaFileIds)
            arenaFileIds' = take numArenas arenaFileIds

        (toTransitionRoomContentTypes, fromTransitionRoomContentTypes) <- generateTransitionRoomContentTypes numArenas

        return $ RoomChooser
            { _currentRoomType                = EmptyRoomType
            , _arenaRoomTypes                 = arenaFileIds'
            , _challengeRoomTypes             = challengeFileIds
            , _numArenas                      = numArenas
            , _toTransitionRoomContentTypes   = toTransitionRoomContentTypes
            , _fromTransitionRoomContentTypes = fromTransitionRoomContentTypes
            , _visitedArenaRoomNames          = S.empty
            , _visitedToTransitionRoomNames   = S.empty
            , _visitedFromTransitionRoomNames = S.empty
            , _visitedChallengeRoomNames      = S.empty
            , _roomEventTypeHistory           = []
            }

nextRoomChooserRoomType :: RoomChooser -> RoomType
nextRoomChooserRoomType roomChooser = case _currentRoomType roomChooser of
    roomType
        | roomType == endHallwayRoomType -> endRoomType
    EmptyRoomType                        -> nextArenaRoomType
    NextRoomType                         -> nextArenaRoomType
    ArenaRoomType name                   -> FromTransitionRoomType name
    ChallengeRoomType _                  -> nextToTransitionRoomType
    ToTransitionRoomType name            -> ArenaRoomType name
    FromTransitionRoomType _             -> nextChallengeRoomType
    SpecialRoomType _                    -> nextToTransitionRoomType
    where
        takeNextRoomType = \ns -> fromMaybe endHallwayRoomType (listToMaybe ns)

        nextChallengeRoomType = takeNextRoomType $ _challengeRoomTypes roomChooser
        nextArenaRoomType     = takeNextRoomType $ _arenaRoomTypes roomChooser

        nextToTransitionRoomType
            | S.size (_visitedArenaRoomNames roomChooser) >= _numArenas roomChooser = endHallwayRoomType
            | otherwise                                                             =
                ToTransitionRoomType $ roomTypeToRoomName nextArenaRoomType

nextRoomChooserRoomTypeSkipTransition :: RoomChooser -> RoomType
nextRoomChooserRoomTypeSkipTransition roomChooser = case _currentRoomType roomChooser of
    roomType
        | roomType == endHallwayRoomType -> endRoomType
    EmptyRoomType                        -> nextArenaRoomType
    NextRoomType                         -> nextArenaRoomType
    ArenaRoomType _                      -> nextChallengeRoomType
    ChallengeRoomType _                  -> nextArenaRoomType
    ToTransitionRoomType name            -> ArenaRoomType name
    FromTransitionRoomType _             -> nextChallengeRoomType
    SpecialRoomType _                    -> nextArenaRoomType
    where
        takeNextRoomType      = \ns -> fromMaybe endHallwayRoomType (listToMaybe ns)
        nextChallengeRoomType = takeNextRoomType $ _challengeRoomTypes roomChooser
        nextArenaRoomType     = takeNextRoomType $ _arenaRoomTypes roomChooser

setRoomChooserRoom :: MonadIO m => Room -> RoomChooser -> m RoomChooser
setRoomChooserRoom currentRoom roomChooser =
    let
        currentRoomType        = _type currentRoom
        currentRoomContentType = roomChooserRoomContentType currentRoomType roomChooser
    in return . flip execState roomChooser $ do
        modify $ \rc -> case currentRoomType of
            ArenaRoomType name -> rc {_visitedArenaRoomNames = S.insert name (_visitedArenaRoomNames rc)}

            FromTransitionRoomType name -> rc
                { _visitedFromTransitionRoomNames = S.insert name (_visitedFromTransitionRoomNames rc)
                , _roomEventTypeHistory           = case currentRoomContentType of
                    EventContentType eventType -> eventType:_roomEventTypeHistory rc
                    _                          -> _roomEventTypeHistory rc
                }

            ToTransitionRoomType name -> rc
                { _visitedToTransitionRoomNames = S.insert name (_visitedToTransitionRoomNames rc)
                , _roomEventTypeHistory         = case currentRoomContentType of
                    EventContentType eventType -> eventType:_roomEventTypeHistory rc
                    _                          -> _roomEventTypeHistory rc
                }

            ChallengeRoomType name -> rc {_visitedChallengeRoomNames = S.insert name (_visitedChallengeRoomNames rc)}

            _ -> rc

        modify $ \rc -> rc
            { _currentRoomType    = currentRoomType
            , _arenaRoomTypes     = filter (/= currentRoomType) (_arenaRoomTypes roomChooser)
            , _challengeRoomTypes = filter (/= currentRoomType) (_challengeRoomTypes roomChooser)
            }

roomChooserRoomContentType :: RoomType -> RoomChooser -> RoomContentType
roomChooserRoomContentType roomType roomChooser = case roomType of
    ToTransitionRoomType _ ->
        let idx = S.size $ _visitedToTransitionRoomNames roomChooser
        in fromMaybe NoContentType (_toTransitionRoomContentTypes roomChooser V.!? idx)

    FromTransitionRoomType _ ->
        let idx = S.size $ _visitedFromTransitionRoomNames roomChooser
        in fromMaybe NoContentType (_fromTransitionRoomContentTypes roomChooser V.!? idx)

    _
        | roomType == startingShopRoomType -> ShopContentType
        | roomType == endHallwayRoomType   -> ShopContentType
        | otherwise                        -> NoContentType
