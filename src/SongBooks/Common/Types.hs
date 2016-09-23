{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SongBooks.Common.Types (
       module System.Posix.Types,
       module Control.Monad.Trans.Except,
       ErrorMsg(..),
       FBT,
       FBIO,
       runFBT,
       -- re-exports from directory-bytestring
       RawFilePath,
       FileStatus
       ) where

import System.Posix.Files.ByteString
import System.Posix.ByteString.FilePath
import System.Posix.Types
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Database.HDBC (SqlError)
import qualified System.IO.Error as E
import qualified Data.Text as T

type Id  = Integer
type Idx = Int

-- | An error which occurred in the program.
data ErrorMsg
    = StringError String -- ^ A simple string error.
    | DBError SqlError   -- ^ An error thrown by Sqlite.
    | IOError E.IOError  -- ^ A disk I/O error
    deriving (Show, Eq)

-- | Mainly for testing purposes, a monad which throws 'ErrorMsg's.
newtype FBT m r = FBT { getFBT :: ExceptT ErrorMsg m r }
     deriving (Applicative, Monad, MonadIO, Functor)

type FBIO = FBT IO

-- | Run an action which may throw 'ErrorMsg's.
runFBT :: (MonadIO m) => FBT m a -> m (Either ErrorMsg a)
runFBT action = runExceptT . getFBT $ action
