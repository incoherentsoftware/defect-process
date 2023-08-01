module Audio.Util
    ( loadMusicDirectoryFilePaths
    , translateFmodStudioPath
    ) where

import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe             (fromMaybe)
import System.Directory       (listDirectory)
import System.FilePath        ((</>), normalise)
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text as T

import Util

musicDirectoryPath     = "data/music" :: FilePath
musicFileNameExtension = ".ogg"       :: FileName

-- all other FMOD Studio event:/ path folder names besides below should be in Title Case
expectedFolderNames = S.fromList
    [ "SFX Events"
    , "UI"
    ] :: S.Set T.Text

loadMusicDirectoryFilePaths :: MonadIO m => m [FilePath]
loadMusicDirectoryFilePaths = do
    dirContents <- liftIO $ listDirectory =<< translateResourcePath musicDirectoryPath
    let
        musicFileNames = filter (L.isSuffixOf musicFileNameExtension) dirContents
        musicFilePaths = map (musicDirectoryPath </>) musicFileNames
    return $ map normalise musicFilePaths

-- workaround FMOD Studio tool exporting these particular paths with altered case for whatever reason
translateFmodStudioPath :: MonadIO m => FilePath -> m FilePath
translateFmodStudioPath = \case
    "event:/SFX Events/Player/Spawn"                      -> return "event:/SFX Events/Player/spawn"
    "event:/SFX Events/Player/Grind-c"                    -> return "event:/SFX Events/Player/grind-c"
    "event:/SFX Events/Player/Warp-out"                   -> return "event:/SFX Events/Player/warp-out"
    "event:/SFX Events/Enemy/Spawn"                       -> return "event:/SFX Events/Enemy/spawn"
    "event:/SFX Events/Enemy/Death"                       -> return "event:/SFX Events/Enemy/death"
    "event:/SFX Events/Enemy/giant/attack-smash"          -> return "event:/SFX Events/Enemy/Giant/attack-smash"
    "event:/SFX Events/Enemy/giant/attack-punch"          -> return "event:/SFX Events/Enemy/Giant/attack-punch"
    "event:/SFX Events/Enemy/giant/hurt"                  -> return "event:/SFX Events/Enemy/Giant/hurt"
    "event:/SFX Events/Enemy/Super-armor"                 -> return "event:/SFX Events/Enemy/super-armor"
    "event:/SFX Events/Enemy/spear/hurt"                  -> return "event:/SFX Events/Enemy/Spear/hurt"
    "event:/SFX Events/Enemy/spear/attack-projectile-hit" ->
        return "event:/SFX Events/Enemy/Spear/attack-projectile-hit"
    "event:/SFX Events/Enemy/spear/attack-throw-release"  ->
        return "event:/SFX Events/Enemy/Spear/attack-throw-release"
    "event:/SFX Events/Enemy/spear/attack-shove"          -> return "event:/SFX Events/Enemy/Spear/attack-shove"
    "event:/SFX Events/Enemy/spear/attack-throw-charge"   -> return "event:/SFX Events/Enemy/Spear/attack-throw-charge"
    "event:/SFX Events/Enemy/Stun"                        -> return "event:/SFX Events/Enemy/stun"
    "event:/SFX Events/Enemy/turret/hurt"                 -> return "event:/SFX Events/Enemy/Turret/hurt"
    "event:/SFX Events/Enemy/turret/attack"               -> return "event:/SFX Events/Enemy/Turret/attack"

    path ->
        let
            tokens                = T.splitOn "/" (T.pack path)
            folderNames           = safeInit $ safeTail tokens
            isFolderNameWrongCase = or [T.toTitle n /= n | n <- folderNames, n `S.notMember` expectedFolderNames]
            soundName             = fromMaybe "" (maybeLast tokens)
            isSoundNameWrongCase  = T.toLower soundName /= soundName
        in do
            when (isFolderNameWrongCase || isSoundNameWrongCase) $
                traceIO' $ "sfx unexpected path case: " <> path
            return path
