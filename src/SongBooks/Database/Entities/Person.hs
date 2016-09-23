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

import SongBooks.Common.Types.Internal
import SongBooks.Database.DBT

data Person = Person { personId        :: Maybe Id
                     , personLastName  :: Text
                     , personFirstName :: Maybe Text
                     } deriving (Read, Show, Eq)

data Publisher = Publisher { publishedId   :: Maybe Id
                           , publisherName :: Text
                           } deriving (Read, Show, Eq)

queryPerson :: (Monad m) => Person -> DBT m [Person]
queryPerson = undefined

savePerson :: (Monad m) => Person -> DBT m Bool
savePerson = undefined

deletePerson :: (Monad m) => Person -> DBT m Int
deletePerson = undefined

queryPublisher :: (Monad m) => Publisher -> DBT m [Publisher]  
queryPublisher = undefined

savePublisher :: (Monad m) => Publisher -> DBT m Bool
savePublisher = undefined

deletePublisher :: (Monad m) => Publisher -> DBT m Int
deletePublisher = undefined
