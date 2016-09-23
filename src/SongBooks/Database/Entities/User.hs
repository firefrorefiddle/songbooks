module SongBooks.Database.Entities.User
       ( User
       , queryUser
       , saveUser
       , deleteUser
       ) where

import SongBooks.Common.Types.Internal
import SongBooks.Database.DBT

data User = User { userId     :: Maybe Id
                 , userLogon  :: Text
                 , userPerson :: Maybe Person
                 , userPwHash :: Maybe Text
                 , userPwSalt :: Maybe Text
                 } deriving (Read, Show, Eq)

queryUser :: (Monad m) => Song -> DBT m [Song]
queryUser = undefined

saveUser :: (Monad m) => Song -> DBT m Bool
saveUser = undefined

deleteUser :: (Monad m) => Song -> DBT m Int
deleteUser = undefined

