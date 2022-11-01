module World.Audio.LayeredMusic
    ( module World.Audio.LayeredMusic.Types
    , isBattleMusicType
    , isExploreMusicType
    , mkLayeredMusic
    , playLayeredMusic
    , fadeInLayeredMusic
    , fadeInLayeredMusicEx
    , fadeInLayeredMusicJukebox
    , canProgressLayeredMusic
    , progressLayeredMusic
    , cycleLayeredMusicType
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable          (sequenceA_)
import Data.Ord               (comparing)
import System.FilePath        (takeFileName)
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Vector as V

import Audio.Fmod
import Configs
import Configs.All.Progress
import World.Audio.LayeredMusic.Types

isBattleMusicType :: LayeredMusicType -> Bool
isBattleMusicType = \case
    BattleAMusic  -> True
    ExploreAMusic -> False
    BattleBMusic  -> True
    ExploreBMusic -> False
    BattleCMusic  -> True
    ExploreCMusic -> False

isExploreMusicType :: LayeredMusicType -> Bool
isExploreMusicType = not . isBattleMusicType

layeredMusicFilePathSubstrs :: LayeredMusicType -> FilePath
layeredMusicFilePathSubstrs = \case
    BattleAMusic  -> "battle-a"
    ExploreAMusic -> "exploration-a"
    BattleBMusic  -> "battle-b"
    ExploreBMusic -> "exploration-b"
    BattleCMusic  -> "battle-c"
    ExploreCMusic -> "exploration-c"

mkLayeredMusic :: MonadIO m => LayeredMusicType -> m LayeredMusic
mkLayeredMusic layeredMusicType = do
    musics <- L.sortBy (comparing fst) <$> getFmodMusics

    let
        filePathSubstr = layeredMusicFilePathSubstrs layeredMusicType
        layers         =
            [ musicIndex
            | (filePath, musicIndex) <- musics
            , filePathSubstr `L.isInfixOf` takeFileName filePath
            ]

    return $ LayeredMusic
        { _type       = layeredMusicType
        , _layers     = V.fromList layers
        , _layerIndex = 0
        }

currentMusicIndex :: LayeredMusic -> Maybe FmodMusicIndex
currentMusicIndex layeredMusic = _layers layeredMusic V.!? _int (_layerIndex layeredMusic)

playLayeredMusic :: MonadIO m => LayeredMusic -> m ()
playLayeredMusic layeredMusic = sequenceA_ $ do
    musicIndex <- currentMusicIndex layeredMusic
    Just $ playFmodMusicWorld musicIndex

fadeInLayeredMusic :: MonadIO m => LayeredMusic -> m ()
fadeInLayeredMusic layeredMusic = fadeInLayeredMusicEx 1.0 layeredMusic

fadeInLayeredMusicEx :: MonadIO m => Float -> LayeredMusic -> m ()
fadeInLayeredMusicEx volumeMultiplier layeredMusic = sequenceA_ $ do
    musicIndex <- currentMusicIndex layeredMusic
    Just $ fadeInFmodMusicWorld musicIndex volumeMultiplier

fadeInLayeredMusicJukebox :: MonadIO m => LayeredMusic -> m ()
fadeInLayeredMusicJukebox layeredMusic = sequenceA_ $ do
    musicIndex <- currentMusicIndex layeredMusic
    Just $ fadeInFmodMusicWorldJukebox musicIndex

canProgressLayeredMusic :: LayeredMusic -> Bool
canProgressLayeredMusic layeredMusic = _int layerIndex + 1 < V.length (_layers layeredMusic)
    where layerIndex = _layerIndex layeredMusic

progressLayeredMusic :: LayeredMusic -> LayeredMusic
progressLayeredMusic layeredMusic = layeredMusic {_layerIndex = layerIndex'}
    where
        layerIndex                                 = _layerIndex layeredMusic
        layerIndex'
            | canProgressLayeredMusic layeredMusic = layerIndex + 1
            | otherwise                            = layerIndex

cycleLayeredMusicType :: ConfigsRead m => LayeredMusicType -> m LayeredMusicType
cycleLayeredMusicType layeredMusicType = do
    unlockedMusic <- readConfig _progress _unlockedMusic
    let isUnlocked = \musicType -> musicType `S.member` unlockedMusic

    return $ case layeredMusicType of
        BattleAMusic
            | isUnlocked BattleBMusic -> BattleBMusic
            | isUnlocked BattleCMusic -> BattleCMusic
            | otherwise               -> BattleAMusic
        BattleBMusic
            | isUnlocked BattleCMusic -> BattleCMusic
            | isUnlocked BattleAMusic -> BattleAMusic
            | otherwise               -> BattleBMusic
        BattleCMusic
            | isUnlocked BattleAMusic -> BattleAMusic
            | isUnlocked BattleBMusic -> BattleBMusic
            | otherwise               -> BattleCMusic
        ExploreAMusic
            | isUnlocked ExploreBMusic -> ExploreBMusic
            | isUnlocked ExploreCMusic -> ExploreCMusic
            | otherwise                -> ExploreAMusic
        ExploreBMusic
            | isUnlocked ExploreCMusic -> ExploreCMusic
            | isUnlocked ExploreAMusic -> ExploreAMusic
            | otherwise                -> ExploreBMusic
        ExploreCMusic
            | isUnlocked ExploreAMusic -> ExploreAMusic
            | isUnlocked ExploreBMusic -> ExploreBMusic
            | otherwise                -> ExploreCMusic
