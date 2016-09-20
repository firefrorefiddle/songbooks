{-# LANGUAGE NoMonomorphismRestriction,
             FlexibleContexts,
             GeneralizedNewtypeDeriving
 #-}

module SongBooks.Database.DBT 
    ( DBT
    , runDBT
    , run
    , run'
    , commit
    , quickQuery
    , prepare
    , execute
    , executeMany
    -- re-exports from HDBC
    , fetchRow
    , Statement
    , SqlValue
    , fromSql
    , toSql
    ) where

import Database.HDBC.Sqlite3 (Connection)
import Database.HDBC (Statement, SqlValue, toSql, fromSql)
import Database.HDBC (SqlError)
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.Sqlite3 as HDBC
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Exception
import SongBooks.Common.Types

-- | A monad containing an Sqlite connection and reporting errors
newtype DBT m r = DBT { getDBT :: ExceptT ErrorMsg (ReaderT Connection m) r } 
     deriving (Applicative, Monad, MonadIO, MonadReader Connection, Functor)

dbErr :: SqlError -> ErrorMsg
dbErr = DBError

throwDB :: (Monad m) => ErrorMsg -> DBT m r
throwDB = DBT . throwE

catchSql :: SqlError -> IO (Either ErrorMsg b)
catchSql e = return . Left $ dbErr e

-- | Try something but catch SqlErrors and report them in the error monad
trySql :: (MonadIO m) => IO a -> m (Either ErrorMsg a)
trySql f = liftIO $ catch (f >>= return . Right) catchSql

-- | Lift some IO action into 'DBT', throwing 'SqlError's as 'DBError'
liftDB :: (MonadIO m) => IO a -> DBT m a
liftDB f = do res <- trySql f
              case res of 
                   Left err -> throwDB err
                   Right res' -> return res'

-- | Run a 'DBT' program
runDBT :: (MonadIO m) 
       => FilePath -- ^Path to the SQLite database file
       -> DBT m a  -- ^action to run
       -> m (Either ErrorMsg a)
runDBT fp action = do
       c <- trySql $ HDBC.connectSqlite3 fp
       case c of 
            Left err -> return . Left $ err
            Right c' -> do
                  ret <- flip runReaderT c' . runExceptT . getDBT $ action
                  case ret of
                       Left _ -> liftIO $ HDBC.rollback c'
                       Right res -> res `seq` liftIO $ HDBC.commit c'
                  liftIO $ HDBC.disconnect c'
                  return ret

-- | 'Database.HDBC.run' lifted into DBT
run :: (MonadIO m) => String -> [SqlValue] -> DBT m Integer
run stmt vals = ask >>= \conn -> liftDB $ HDBC.run conn stmt vals

-- | Run a query without variable parameters
run' :: (MonadIO m) => String -> DBT m Integer
run' stmt = ask >>= \conn -> liftDB $ HDBC.run conn stmt []

-- | 'Database.HDBC.commit' lifted into DBT
commit :: (MonadIO m) => DBT m ()
commit = ask >>= \conn -> liftDB $ HDBC.commit conn

-- | 'Database.HDBC.quickQuery' lifted into DBT
quickQuery :: (MonadIO m) => String -> [SqlValue] -> DBT m [[SqlValue]]
quickQuery q vals = ask >>= \conn -> liftDB $ HDBC.quickQuery' conn q vals

-- | 'Database.HDBC.prepare' lifted into DBT
prepare :: (MonadIO m) => String -> DBT m Statement
prepare q = ask >>= \conn -> liftDB $ HDBC.prepare conn q

-- | 'Database.HDBC.execute' lifted into DBT
execute :: (MonadIO m) => Statement -> [SqlValue] -> DBT m Integer
execute q vals = liftDB $ HDBC.execute q vals

-- | 'Database.HDBC.executeMany' lifted into DBT
executeMany :: (MonadIO m) => Statement -> [[SqlValue]] -> DBT m ()
executeMany q vals = liftDB $ HDBC.executeMany q vals

-- | 'Database.HDBC.fetchRow' lifted into DBT
fetchRow :: (MonadIO m) => Statement -> DBT m (Maybe [SqlValue])
fetchRow = liftDB . HDBC.fetchRow
