{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-| 
Module      : FutureBack.Common.Types
Copyright   : (c) Martin Stücklschwaiger, Michael Hartl, 2014
Maintainer  : mikehartl17@gmail.com
Stability   : experimental
Portability : portable

Most of our internal types are defined here.
-}

module SongBooks.Common.Types (
       module System.Posix.Types,
       module Control.Monad.Trans.Except,
       module SongBooks.Common.Types.Database,       
       FileInfo,
       FileDate,
       FileMeta(..),
       Backup(..),
       FileType(..),
       ErrorMsg(..),
       BackupCondition(..),
       BackupRestart(..),
       RestartCondition(..),
       RestartHandler,
       FBT,
       FBIO,
       runFBT,
       UniqueId,
       BackupId,
       -- re-exports from directory-bytestring
       RawFilePath,
       FileStatus,
       ) where

import System.Posix.Files.ByteString
import System.Posix.ByteString.FilePath
import System.Posix.Types
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Database.HDBC (SqlError)
import qualified System.IO.Error as E
import qualified Data.Text as T

import SongBooks.Common.Types.Database

type FileInfo = (RawFilePath, FileStatus)

type FileDate = EpochTime

data FileType = RegularFile | Directory | Symlink | BlockDevice | CharDevice | UnknownFileType
  deriving (Read, Show, Eq)

type UniqueId = T.Text
type BackupId = UniqueId

-- | All known restarts which may become available during a backup or restore operation.
data BackupRestart
    = Overwrite
    | OverwriteAll
    | Abort
    | Retry
    | Merge
    | MergeAll
    deriving (Show, Read, Eq)

-- | All known conditions which may occur during a backup or restore operation.
data BackupCondition
    = FileExists FileType RawFilePath
    | WrongFileType RawFilePath FileType FileType
    | WrongFileMode RawFilePath FileMode FileMode
    deriving (Show, Eq)

-- | A concrete condition which occurred, and the available restarts.
data RestartCondition = RestartCondition [BackupCondition] [BackupRestart]
    deriving (Show, Eq)

-- | A function which selects one of the available restarts. If the handler 
-- returns 'Nothing', then the next handler is tried. If no handler handles
-- the restart, an error is thrown.
type RestartHandler m = RestartCondition -> m (Maybe BackupRestart)

-- | An error which occurred in the program.
data ErrorMsg
    = StringError String -- ^ A simple string error.
    | DBError SqlError   -- ^ An error thrown by Sqlite.
    | IOError E.IOError  -- ^ A disk I/O error
    | BackupError RestartCondition -- ^ A restartable condition
    deriving (Show, Eq)

-- | This is most of the file metadata grouped together and stored as a single node. 
data FileMeta = FileMeta
    { metaPermissions :: FileMode
    , metaOwner       :: (UserID, GroupID)
    , metaFilesize    :: FileOffset
    , metaModified    :: EpochTime
    , metaFileType    :: FileType
    }
    deriving (Read, Show, Eq)

-- | All the metadata belonging to a single backup run, stored together as a single node.
data Backup = Backup 
    { backupId          :: BackupId
    , backupStartTime   :: EpochTime
    , backupSource      :: RawFilePath
    , backupDestination :: RawFilePath
    , mBackupEndTime    :: Maybe EpochTime
    } deriving (Eq, Show)

{-instance Error ErrorMsg where
         noMsg = StringError ""
         strMsg = StringError
-}
-- | Mainly for testing purposes, a monad which throws 'ErrorMsg's.
newtype FBT m r = FBT { getFBT :: ExceptT ErrorMsg m r }
     deriving (Applicative, Monad, MonadIO, Functor) -- MonadError ErrorMsg, Functor)

type FBIO = FBT IO

-- | Run an action which may throw 'ErrorMsg's.
runFBT :: (MonadIO m) => FBT m a -> m (Either ErrorMsg a)
runFBT action = runExceptT . getFBT $ action
