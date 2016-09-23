module SongBooks.Database.Entities.SongBook
       ( SongBook
       , Style
       , SBPart
       , querySongBook
       , saveSongbook
       , deleteSongBook
       , queryStyle
       , saveStyle
       , deleteStyle
       , querySBPart
       , saveSBPart
       , deleteSBPart) where

import SongBooks.Common.Types.Internal
import SongBooks.Database.DBT

data Style = TeXStyle { texId :: Maybe Id
                      , texHeader :: Text } deriving (Read, Show, Eq)

data SBPart = SBPart { sbpTitle :: Maybe Text
                     , sbpSongs :: [Song]
                     } deriving (Read, Show, Eq)

data SongBook = SongBook { sbId          :: Maybe Id
                         , sbName        :: Text
                         , sbTitle       :: Maybe Text
                         , sbSubtitle    :: Maybe Text
                         , sbDescription :: Maybe Text
                         , sbLicense     :: Maybe Text
                         , sbCrHolder    :: Maybe Publisher
                         , sbCrYear      :: Maybe Integer
                         , sbAuthors     :: [Person]
                         , sbParts       :: [(Idx, SBPart)]
                         } deriving (Read, Show, Eq)

querySongBook :: (Monad m) => SongBook -> DBT m [SongBook]
querySongBook = undefined

saveSongBook :: (Monad m) => SongBook -> DBT m Bool
saveSongBook = undefined

deleteSongBook :: (Monad m) => SongBook -> DBT m Int
deleteSongBook = undefined

queryStyle :: (Monad m) => Style -> DBT m [Style]
queryStyle = undefined

saveStyle :: (Monad m) => Style -> DBT m Bool
saveStyle = undefined

deleteStyle :: (Monad m) => Style -> DBT m Int
deleteStyle = undefined

querySBPart :: (Monad m) => SBPart -> DBT m [SBPart]
querySBPart = undefined

saveSBPart :: (Monad m) => SBPart -> DBT m Bool
saveSBPart = undefined

deleteSBPart :: (Monad m) => SBPart -> DBT m Int
deleteSBPart = undefined
