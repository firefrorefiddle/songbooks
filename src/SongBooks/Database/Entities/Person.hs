module SongBooks.Database.Entities.Person
       ( Person
       , Publisher
       , queryPerson
       , savePerson
       , deletePerson
       , queryPublisher
       , savePublisher
       , deletePublisher
       ) where

import SongBooks.Common.Types
import SongBooks.Database.DBT

data Person = Person { personId        :: Maybe Id
                     , personLastName  :: Text
                     , personFirstName :: Maybe Text
                     } deriving (Read, Show, Eq)

data Publisher = Publisher { publishedId   :: Maybe Id
                           , publisherName :: Text
                           } deriving (Read, Show, Eq)

getPersonById :: (MonadIO m) => Id -> DBT m (Maybe Person)
getPersonById pid = do
  names <- selectExact "SELECT last_name, first_name \
                          \ FROM person \
                          \ WHERE id = ?" [toSql pid]
  case names of
    Nothing -> return Nothing
    Just [ln, fn] -> return $ Just $ Person (Just pid) (fromSql ln) (Just $ fromSql fn)
  
queryPerson :: (MonadIO m) => Maybe Text -> Maybe Text -> DBT m [Person]
queryPerson qln qfn = undefined -- map toPerson <$> select ("SELECT id, last_name, first_name \
--                                                \ FROM person" ++ whereLastName qln ++ whereFirstName qfn)
--   where toPerson pid ln fn = Person (fromSql pid) (fromSql ln) (fromSql fn)
--        whereLastName Nothing = ""
--        whereLastName (Just ln) = " WHERE last_name = ?"
--        whereFirstName = undefined

savePerson :: (MonadIO m) => Person -> DBT m Bool
savePerson = undefined

deletePerson :: (MonadIO m) => Person -> DBT m Int
deletePerson = undefined

queryPublisher :: (MonadIO m) => Publisher -> DBT m [Publisher]  
queryPublisher = undefined

savePublisher :: (MonadIO m) => Publisher -> DBT m Bool
savePublisher = undefined

deletePublisher :: (MonadIO m) => Publisher -> DBT m Int
deletePublisher = undefined
