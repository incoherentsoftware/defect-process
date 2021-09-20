module Console.Types
    ( ConsoleCommandResult(..)
    , ConsoleCommand
    , ConsoleUpdateResult(..)
    , Console(..)
    , ConsoleCommandHistoryIndex
    , ConsoleCommandHistoryOffset(..)
    ) where

import qualified Data.Map as M
import qualified Data.Text as T

import AppEnv
import Configs
import Msg.Phase
import Window.Graphics
import Window.InputState
import World.Types

data ConsoleCommandResult
    = NoUpdateResult T.Text
    | UpdateWorldResult T.Text World
    | UpdateConfigsResult T.Text Configs
    | RunFileResult FilePath
    | BindCmdResult InputRawData T.Text
    | PrintCmdBindsResult

type ConsoleCommand m = [T.Text] -> World -> m ConsoleCommandResult

type ConsoleCommandHistoryIndex = Maybe Int

data ConsoleCommandHistoryOffset
    = PrevHistory
    | NextHistory

data ConsoleUpdateResult = ConsoleUpdateResult
    { _console :: Console
    , _world   :: World
    , _configs :: Configs
    , _output  :: [T.Text]
    }

data Console = Console
    { _active               :: Bool
    , _displayText          :: DisplayText
    , _displayTextLines     :: [DisplayText]
    , _commands             :: M.Map T.Text (ConsoleCommand (AppEnv ConsoleMsgsPhase))
    , _commandsHistoryIndex :: ConsoleCommandHistoryIndex
    , _commandBinds         :: M.Map InputRawData T.Text
    }
