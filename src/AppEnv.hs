module AppEnv
    ( AppEnvReadData(_configs)
    , AppEnvData
    , AppEnv
    , mkAppEnvData
    , appEnvDataGraphics
    , appEnvDataFileCacheData
    , appEnvDataAsyncRequestQueue
    , appEnvDataAsyncDataQueue
    , runAppEnv
    , withAppEnvReadData
    , updateAppEnvReadData
    , clearAppEnvMsgs
    , catchAppEnv
    , withMsgsPhase
    ) where

import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO)
import Control.Monad.Catch           (SomeException, catch)
import Control.Monad.Reader          (local, runReaderT)
import Control.Monad.State           (evalStateT, modify)
import qualified Data.Map as M

import AppEnv.Types
import Async.Data
import Async.Request
import Configs
import FileCache
import Msg.Types
import Window.Graphics.Types
import Window.InputState.Types
import Window.Types

mkAppEnvReadData :: Window -> Configs -> IO AppEnvReadData
mkAppEnvReadData window cfgs = do
    fileCacheData     <- mkFileCacheData cfgs
    asyncRequestQueue <- newTQueueIO
    asyncDataQueue    <- newTQueueIO
    asyncSignalQueue  <- newTQueueIO

    return $ AppEnvReadData
        { _graphics          = _graphics (window :: Window)
        , _inputState        = _inputState (window :: Window)
        , _configs           = cfgs
        , _fileCacheData     = fileCacheData
        , _asyncRequestQueue = asyncRequestQueue
        , _asyncDataQueue    = asyncDataQueue
        , _asyncSignalQueue  = asyncSignalQueue
        }

mkAppEnvWriteData :: IO AppEnvWriteData
mkAppEnvWriteData = return $ AppEnvWriteData M.empty

mkAppEnvData :: Window -> Configs -> IO AppEnvData
mkAppEnvData window cfgs =
    AppEnvData <$>
    mkAppEnvReadData window cfgs <*>
    mkAppEnvWriteData

appEnvDataGraphics :: AppEnvData -> Graphics
appEnvDataGraphics = (_graphics :: AppEnvReadData -> Graphics) . _readData

appEnvDataFileCacheData :: AppEnvData -> FileCacheData
appEnvDataFileCacheData = _fileCacheData . _readData

appEnvDataAsyncRequestQueue :: AppEnvData -> TQueue AsyncRequest
appEnvDataAsyncRequestQueue = _asyncRequestQueue . _readData

appEnvDataAsyncDataQueue :: AppEnvData -> TQueue AsyncData
appEnvDataAsyncDataQueue = _asyncDataQueue . _readData

runAppEnv :: AppEnvData -> AppEnv BaseMsgsPhase a -> IO a
runAppEnv appEnvData (AppEnv appEnv) = evalStateT (runReaderT appEnv envReadData) envWriteData
    where
        envReadData  = _readData appEnvData
        envWriteData = _writeData appEnvData

withAppEnvReadData :: InputState -> Configs -> AppEnv p a -> AppEnv p a
withAppEnvReadData inputState cfgs appEnv = local updateReadData appEnv
    where
        updateReadData :: AppEnvReadData -> AppEnvReadData
        updateReadData readData = readData
            { _inputState = inputState
            , _configs    = cfgs
            } :: AppEnvReadData

updateAppEnvReadData :: InputState -> Configs -> AppEnvData -> AppEnvData
updateAppEnvReadData inputState cfgs appEnvData = appEnvData
    { _readData = (_readData appEnvData)
        { _inputState = inputState
        , _configs    = cfgs
        } :: AppEnvReadData
    }

clearAppEnvMsgs :: AppEnv BaseMsgsPhase ()
clearAppEnvMsgs = modify $ \appEnvData -> appEnvData {_messages = M.empty}

catchAppEnv :: AppEnv p m -> (SomeException -> AppEnv p m) -> AppEnv p m
catchAppEnv = catch

withMsgsPhase :: AppEnv p a -> AppEnv BaseMsgsPhase a
withMsgsPhase (AppEnv appEnv) = AppEnv appEnv
