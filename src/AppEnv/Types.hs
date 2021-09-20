module AppEnv.Types
    ( AppEnvReadData(..)
    , AppEnvWriteData(..)
    , AppEnvData(..)
    , AppEnv(..)
    ) where

import Control.Concurrent.STM        (atomically)
import Control.Concurrent.STM.TQueue (TQueue, flushTQueue, isEmptyTQueue, tryReadTQueue, writeTQueue)
import Control.Monad.Catch           (MonadCatch, MonadThrow)
import Control.Monad.IO.Class        (liftIO)
import Control.Monad.Random          (MonadRandom)
import Control.Monad.Reader          (MonadReader, ReaderT, asks)
import Control.Monad.State           (MonadState, StateT, MonadIO, gets, modify)
import Data.Foldable                 (for_)
import Data.Maybe                    (catMaybes)
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Map as M

import Async.Data
import Async.Request
import Async.Signal
import Configs
import FileCache
import Msg.Types
import Window.Graphics.Types
import Window.InputState.Types
import {-# SOURCE #-} Msg.Payload

data AppEnvReadData = AppEnvReadData
    { _graphics          :: Graphics
    , _inputState        :: InputState
    , _configs           :: Configs
    , _fileCacheData     :: FileCacheData
    , _asyncRequestQueue :: TQueue AsyncRequest
    , _asyncDataQueue    :: TQueue AsyncData
    , _asyncSignalQueue  :: TQueue AsyncSignal
    }

data AppEnvWriteData = AppEnvWriteData
    { _messages :: M.Map MsgId [MsgInternal]
    }

data AppEnvData = AppEnvData
    { _readData  :: AppEnvReadData
    , _writeData :: AppEnvWriteData
    }

newtype AppEnv p a = AppEnv (ReaderT AppEnvReadData (StateT AppEnvWriteData IO) a)
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadReader AppEnvReadData
        , MonadState AppEnvWriteData
        , MonadIO
        , MonadCatch
        , MonadThrow
        , MonadRandom
        )

instance ConfigsRead (AppEnv p) where
    readConfig :: (Configs -> a) -> (a -> b) -> AppEnv p b
    readConfig cfg f = (f . cfg) <$> readConfigs

    readConfigs :: AppEnv p Configs
    readConfigs = asks _configs

instance MsgsRead p (AppEnv p) where
    readMsgsTo :: AllowMsgRead p a => MsgId -> AppEnv p [a]
    readMsgsTo toId = do
        msgs <- M.findWithDefault [] toId <$> gets _messages
        return . catMaybes $ map (fromMsgPayload . _payload) msgs

instance MsgsWrite p (AppEnv p) where
    writeMsgs :: [Msg p] -> AppEnv p ()
    writeMsgs msgs = for_ msgs $ \(Msg msg) ->
        let
            msgToId     = _to msg
            msgOrderCmp = \m1 m2 -> compare (_order m1) (_order m2)
        in do
            msgsList     <- M.findWithDefault [] msgToId <$> gets _messages
            let msgsList' = L.insertBy msgOrderCmp msg msgsList
            modify $ \appEnvData ->
                appEnvData {_messages = M.insert msgToId msgsList' (_messages appEnvData)}

instance AsyncRequestRead (AppEnv p) where
    isAsyncRequestsEmpty :: AppEnv p Bool
    isAsyncRequestsEmpty = liftIO . atomically . isEmptyTQueue =<< asks _asyncRequestQueue

instance AsyncRequestWrite (AppEnv p) where
    writeAsyncRequest :: AsyncRequest -> AppEnv p ()
    writeAsyncRequest asyncRequest = do
        queue <- asks _asyncRequestQueue
        liftIO . atomically $ writeTQueue queue asyncRequest

instance AsyncDataRead (AppEnv p) where
    readAsyncData :: AppEnv p (Maybe AsyncData)
    readAsyncData = liftIO . atomically . tryReadTQueue =<< asks _asyncDataQueue

    readAsyncDataAll :: AppEnv p [AsyncData]
    readAsyncDataAll = liftIO . atomically . flushTQueue =<< asks _asyncDataQueue

instance AsyncSignalRead (AppEnv p) where
    readAsyncSignal :: AppEnv p (Maybe AsyncSignal)
    readAsyncSignal = liftIO . atomically . tryReadTQueue =<< asks _asyncSignalQueue

instance AsyncSignalWrite (AppEnv p) where
    writeAsyncSignal :: AsyncSignal -> AppEnv p ()
    writeAsyncSignal asyncSignal = do
        queue <- asks _asyncSignalQueue
        liftIO . atomically $ writeTQueue queue asyncSignal

instance FileCache (AppEnv p) where
    readFileCachePackResource :: PackResourceFilePath -> AppEnv p BS.ByteString
    readFileCachePackResource packResourceFilePath =
        readFileCacheDataPackResource packResourceFilePath =<< asks _fileCacheData

    readFileCache :: FilePath -> AppEnv p BS.ByteString
    readFileCache filePath = readFileCacheData filePath =<< asks _fileCacheData

instance GraphicsRead (AppEnv p) where
    getGraphics :: AppEnv p Graphics
    getGraphics = asks _graphics

instance GraphicsReadWrite (AppEnv p)

instance InputRead (AppEnv p) where
    readInputState :: AppEnv p InputState
    readInputState = asks _inputState
