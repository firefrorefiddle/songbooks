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
    , execute
    , executeMany
    , select
    , selectExact
    , selectExact'      
    -- re-exports from HDBC      
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
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Exception
import SongBooks.Common.Types
import qualified Data.Map as M
import Crypto.Hash.MD5 (hash)
import Data.ByteString (ByteString, pack)
import Data.Char (ord)

type Hash = ByteString
type StmtMap = M.Map Hash Statement

-- | A monad containing an Sqlite connection and reporting errors
newtype DBT m r = DBT { getDBT :: ExceptT ErrorMsg (StateT StmtMap (ReaderT Connection m)) r } 
     deriving (Applicative, Monad, MonadIO, MonadReader Connection, MonadState StmtMap, Functor)

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
                  ret <- flip runReaderT c' . flip evalStateT M.empty . runExceptT . getDBT $ action
                  case ret of
                       Left _ -> liftIO $ HDBC.rollback c'
                       Right res -> res `seq` liftIO $ HDBC.commit c'
                  liftIO $ HDBC.disconnect c'
                  return ret

-- | 'Database.HDBC.prepare' lifted into DBT
-- Prepared statements are cached in the State Monad.
-- Should not be exported, so the Statement doesn't
-- leak outside this module, where it could be used
-- outside of a DBT context and raise a runtime error.
prepare :: (MonadIO m) => String -> DBT m Statement
prepare q = do
  let h = hash . pack . map (fromIntegral.ord) $ q
  prepMap <- get
  let prepStmt = M.lookup h prepMap
  case prepStmt of
    Nothing -> do conn <- ask
                  stmt <- liftDB $ HDBC.prepare conn q
                  put (M.insert h stmt prepMap)
                  return stmt
    Just stmt -> return stmt

-- | Run a query. Question marks are replaced by the parameters.
--   This is issued directly to the database without caching of the
--   statement.
run :: (MonadIO m) => String -> [SqlValue] -> DBT m Integer
run stmt vals = ask >>= \conn -> liftDB $ HDBC.run conn stmt vals

-- | Run a query without variable parameters. This is issued directly to
--   the database without caching of the statement.
run' :: (MonadIO m) => String -> DBT m Integer
run' stmt = ask >>= \conn -> liftDB $ HDBC.run conn stmt []

-- | Issue a commit on the connection. You can continue working afterwards.
commit :: (MonadIO m) => DBT m ()
commit = ask >>= \conn -> liftDB $ HDBC.commit conn

-- | Execute a query and ignore the results. Question marks are
--   replaced by the bind parameters by HDBC. If the same query
--   has been prepared before, the statement is reused.
execute :: (MonadIO m) => String -> [SqlValue] -> DBT m Integer
execute q vals = do stmt <- prepare q
                    liftDB $ HDBC.execute stmt vals

-- | Execute a query with multiple sets of bind parameters and ignore the
--   results. Question marks are replaced by the bind parameters by HDBC.
--   If the same query has been prepared before, the statement is reused.
executeMany :: (MonadIO m) => String -> [[SqlValue]] -> DBT m ()
executeMany q vals = do stmt <- prepare q
                        liftDB $ HDBC.executeMany stmt vals

-- | Execute a query and fetch all rows, strictly. Question marks are
--   replaced by the bind parameters by HDBC. If the same query
--   has been prepared before, the statement is reused.
select :: (MonadIO m) => String -> [SqlValue] -> DBT m ([[SqlValue]])
select q params = do stmt <- prepare q
                     liftDB $ HDBC.execute stmt params
                     liftDB $ HDBC.fetchAllRows' stmt

selectExact :: (MonadIO m) => String -> [SqlValue] -> DBT m (Maybe [SqlValue])
selectExact q params = do res <- select q params
                          case res of
                            []    -> return Nothing
                            [row] -> return $ Just row
                            _     -> throwDB (StringError $ "selectExact: Query returns more than one row.\n" ++ q)

selectExact' :: (MonadIO m) => String -> [SqlValue] -> DBT m [SqlValue]
selectExact' q params = do res <- select q params
                           case res of
                             []    -> throwDB (StringError $ "selectExact': Query returns nothing.\n" ++ q)
                             [row] -> return row
                             _     -> throwDB (StringError $ "selectExact': Query returns more than one row.\n" ++ q)
