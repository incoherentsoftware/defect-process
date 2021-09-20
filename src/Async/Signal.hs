module Async.Signal
    ( AsyncSignal(..)
    , AsyncSignalRead(..)
    , AsyncSignalWrite(..)
    ) where

data AsyncSignal
    = DoMainThreadLoadsSignal

class Monad m => AsyncSignalRead m where
    readAsyncSignal :: m (Maybe AsyncSignal)

class Monad m => AsyncSignalWrite m where
    writeAsyncSignal :: AsyncSignal -> m ()
