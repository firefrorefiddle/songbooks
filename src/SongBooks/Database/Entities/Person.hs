module SongBooks.Database.Entities.Person
       ( queryPerson
       , savePerson
       , deletePerson) where

import SongBooks.Database.DBT

queryPerson :: (Monad m) => Person -> DBT m [Person]
queryPerson = undefined

savePerson :: (Monad m) => Person -> DBT m Bool
savePerson = undefined

deletePerson :: (Monad m) => Person -> DBT m Int
deletePerson = undefined
 
-- queryPersonStmt :: Statement
