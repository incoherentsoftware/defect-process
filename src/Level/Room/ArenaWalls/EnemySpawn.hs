module Level.Room.ArenaWalls.EnemySpawn
    ( chooseRoomArenaWallsEnemySpawnWaves
    , thinkRoomArenaWallsEnemySpawn
    , roomArenaWallsEnemySpawnOffset
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe             (catMaybes, fromMaybe)
import System.Random.Shuffle  (shuffleM)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import Collision
import Configs
import Configs.All.Level
import Constants
import Enemy
import Enemy.All
import InfoMsg.Util
import Level.DangerValue
import Level.Room.ArenaWalls.EnemySpawn.Types
import Level.Room.ArenaWalls.Types
import Level.Room.ArenaWalls.Util
import Msg
import Util

timeBetweenEnemyWavesSecs              = 0.5               :: Secs
playerEnemySpawnOverlapThresholdDistSq = 100 ** 2          :: Float
airSpawnOffset                         = Pos2 0.0 (-300.0) :: Pos2

anyTurretEnemyTypes = [BubbleTurretEnemy, TurretEnemy] :: [EnemyType]

parseTextToEnemyTypes :: T.Text -> [EnemyType]
parseTextToEnemyTypes txt = catMaybes $ map enemyTypeFromName (T.splitOn "|" txt)

mkEnemySpawnWave :: forall m. MonadIO m => DangerValue -> EnemySpawnWaveChoice -> m EnemySpawnWave
mkEnemySpawnWave dangerVal enWaveChoice = EnemySpawnWave dangerVal <$> chooseEnemyTypes [] (_parseTexts enWaveChoice)
    where
        byRules :: [EnemyType] -> EnemyType -> Bool
        byRules chosenEnTypes enType = and $ map byRule (fromMaybe [] (_rules enWaveChoice))
            where
                byRule :: EnemySpawnWaveChoiceRule -> Bool
                byRule = \case
                    MaxOneAnyTurretRule
                        | enType `elem` anyTurretEnemyTypes -> countBy (`elem` anyTurretEnemyTypes) < 1
                        | otherwise                         -> True
                    MaxOneBombRule
                        | enType == BombEnemy               -> countBy (== BombEnemy) < 1
                        | otherwise                         -> True
                    MaxOneBubbleTurretRule
                        | enType == BubbleTurretEnemy       -> countBy (== BubbleTurretEnemy) < 1
                        | otherwise                         -> True
                    MaxOneFlyingRule
                        | enType == FlyingEnemy             -> countBy (== FlyingEnemy) < 1
                        | otherwise                         -> True
                    MaxTwoFlyingRule
                        | enType == FlyingEnemy             -> countBy (== FlyingEnemy) < 2
                        | otherwise                         -> True
                    MaxOneHopRule
                        | enType == HopEnemy                -> countBy (== HopEnemy) < 1
                        | otherwise                         -> True
                    MaxOneSpearRule
                        | enType == SpearEnemy              -> countBy (== SpearEnemy) < 1
                        | otherwise                         -> True
                    MaxOneTurretRule
                        | enType == TurretEnemy             -> countBy (== TurretEnemy) < 1
                        | otherwise                         -> True
                    MaxOneWallRule
                        | enType == WallEnemy               -> countBy (== WallEnemy) < 1
                        | otherwise                         -> True
                    where countBy = \p -> length $ filter p chosenEnTypes

        chooseEnemyTypes :: [EnemyType] -> [T.Text] -> m [EnemyType]
        chooseEnemyTypes chosenEnTypes []                       = return chosenEnTypes
        chooseEnemyTypes chosenEnTypes (enTypesTxt:enTypesTxts) = do
            let enTypeChoices = filter (byRules chosenEnTypes) (parseTextToEnemyTypes enTypesTxt)

            chosenEnTypes' <- case NE.nonEmpty enTypeChoices of
                Nothing      -> return chosenEnTypes
                Just choices -> (:chosenEnTypes) <$> randomChoice choices
            chooseEnemyTypes chosenEnTypes' enTypesTxts

chooseRoomArenaWallsEnemySpawnWaves :: (ConfigsRead m, MonadIO m) => DangerValue -> m [EnemySpawnWave]
chooseRoomArenaWallsEnemySpawnWaves currentDangerVal =
    let
        takeEnemySpawnWaveJSON :: ConfigsRead m1 => m1 EnemySpawnWaveJSON
        takeEnemySpawnWaveJSON = do
            jsons <- readConfig _level (_enemySpawnWaves :: LevelConfig -> NE.NonEmpty EnemySpawnWaveJSON)
            return $ case NE.filter (\j -> (_dangerValue (j :: EnemySpawnWaveJSON) >= currentDangerVal)) jsons of
                (json:_) -> json
                []       ->
                    let firstDangerVal = _dangerValue (NE.head jsons :: EnemySpawnWaveJSON)
                    in if
                        | currentDangerVal < firstDangerVal -> NE.head jsons
                        | otherwise                         -> NE.last jsons

        applyChanceMultiplier :: EnemySpawnWaveChoice -> [EnemySpawnWaveChoice]
        applyChanceMultiplier choice = maybe [choice] (`replicate` choice) (_chanceMultiplier choice)

        chooseSpawnWave :: MonadIO m1 => EnemySpawnWaveJSON -> m1 (Maybe EnemySpawnWave)
        chooseSpawnWave spawnWaveJSON =
            let
                dangerVal = _dangerValue (spawnWaveJSON :: EnemySpawnWaveJSON)
                enChoices = concatMap applyChanceMultiplier $ _enemyChoices (spawnWaveJSON :: EnemySpawnWaveJSON)
            in case enChoices of
                []     -> return Nothing
                (c:cs) -> do
                    enChoice <- randomChoice $ c NE.:| cs
                    Just <$> mkEnemySpawnWave dangerVal enChoice
    in do
        spawnWaveJSON <- takeEnemySpawnWaveJSON
        catMaybes <$> sequenceA (replicate (_numWaves spawnWaveJSON) (chooseSpawnWave spawnWaveJSON))

processEnemyCount :: MsgsRead ThinkLevelMsgsPhase m => RoomArenaWalls -> m Int
processEnemyCount arenaWalls = L.foldl' processMsg 0 <$> readMsgs
    where
        processMsg :: Int -> InfoMsgPayload -> Int
        processMsg !enemyCount msgData = case msgData of
            InfoMsgEnemyPos enHbx _ ->
                let
                    enCenterX  = vecX $ hitboxCenter enHbx
                    wallsLeft  = vecX $ roomArenaWallsLeftWallPos arenaWalls
                    wallsRight = vecX $ roomArenaWallsRightWallPos arenaWalls
                in if
                    | enCenterX >= wallsLeft && enCenterX <= wallsRight -> enemyCount + 1
                    | otherwise                                         -> enemyCount

            _ -> enemyCount

readPlayerPos :: MsgsRead ThinkLevelMsgsPhase m => RoomArenaWalls -> m Pos2
readPlayerPos arenaWalls = L.foldl' processMsg wallsBotCenter <$> readMsgs
    where
        processMsg :: Pos2 -> InfoMsgPayload -> Pos2
        processMsg playerPos msgData = case msgData of
            InfoMsgPlayer playerInfo -> playerInfoPos playerInfo
            _                        -> playerPos

        wallsBotCenter = hitboxBotCenter $ roomArenaWallsBoundingHitbox arenaWalls

thinkRoomArenaWallsEnemySpawn :: MsgsRead ThinkLevelMsgsPhase m => RoomArenaWalls -> m [Msg ThinkLevelMsgsPhase]
thinkRoomArenaWallsEnemySpawn arenaWalls = case _status (arenaWalls :: RoomArenaWalls) of
    WallsReadyStatus -> return []
    WallsDoneStatus  -> return []

    WallsActiveStatus noEnemiesSecs ->
        let
            mkUpdateStatusMsg = \status ->
                mkMsg $ RoomMsgUpdateArenaWalls (\a -> a {_status = status})
            enemySpawnWaves   = _enemySpawnWaves (arenaWalls :: RoomArenaWalls)
        in do
            enemyCount <- processEnemyCount arenaWalls
            playerPos  <- readPlayerPos arenaWalls

            return $ if
                | enemyCount <= 0 -> if
                    | noEnemiesSecs < timeBetweenEnemyWavesSecs ->
                        [mkUpdateStatusMsg $ WallsActiveStatus (noEnemiesSecs + timeStep)]

                    | not (null enemySpawnWaves) -> enemySpawnWaveMsgs playerPos arenaWalls

                    | otherwise ->
                        [ mkUpdateStatusMsg WallsDoneStatus
                        , mkMsg AudioMsgPlayPostBattleExplorationMusic
                        ] ++ roomArenaWallsDisappearMsgs arenaWalls

                | otherwise -> []

roomArenaWallsEnemySpawnOffset :: Pos2 -> EnemyType -> Pos2
roomArenaWallsEnemySpawnOffset pos enType = case enType of
    FlyingEnemy -> pos `vecAdd` airSpawnOffset
    HammerEnemy -> pos `vecAdd` airSpawnOffset
    BatEnemy    -> pos `vecAdd` airSpawnOffset
    _           -> pos

enemySpawnWaveMsgs :: Pos2 -> RoomArenaWalls -> [Msg ThinkLevelMsgsPhase]
enemySpawnWaveMsgs playerPos arenaWalls = case _enemySpawnWaves (arenaWalls :: RoomArenaWalls) of
    []                     -> []
    (spawnWave:spawnWaves) ->
        let
            mkEnemies = do
                let farEnough   = \spawnPos -> vecDistSq playerPos spawnPos >= playerEnemySpawnOverlapThresholdDistSq
                spawnPositions <- shuffleM $ filter farEnough (_enemySpawnPositions arenaWalls)
                sequenceA
                    [ mkEnemyFromType enType pos' dir
                    | (pos, enType) <- zip spawnPositions (_enemyTypes spawnWave)
                    , let dir  = if vecX pos < vecX playerPos then RightDir else LeftDir
                    , let pos' = roomArenaWallsEnemySpawnOffset pos enType
                    ]

            updateWalls = \aw -> aw
                { _status          = WallsActiveStatus 0.0
                , _enemySpawnWaves = spawnWaves
                }
        in
            [ mkMsg $ EnemyMsgAddsM mkEnemies
            , mkMsg $ RoomMsgUpdateArenaWalls updateWalls
            ]
