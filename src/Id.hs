module Id
    ( HashedId(..)
    , Id(NullId)
    , newId
    , hashId
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Unique            (Unique, hashUnique, newUnique)

newtype HashedId = HashedId {_int :: Int}
    deriving (Eq, Ord, Show)

data Id a
    = Id Unique
    | NullId
    deriving (Eq, Ord)

instance Show (Id a) where
    show :: Id a -> String
    show (Id u) = show $ hashUnique u
    show NullId = "null"

newId :: MonadIO m => m (Id a)
newId = Id <$> liftIO newUnique

-- implementation specific for Data.Unique, assumes newUnique starts at 1
hashId :: Id a -> HashedId
hashId (Id uniq) = HashedId $ hashUnique uniq
hashId NullId    = HashedId 0
