module SongBooks.Database.Entities.Song
       ( Song
       , SongInst
       , SIData
       , DataType
       , InstType
       , VerseType
       , Verse
       , querySong
       , saveSong
       , deleteSong
       , querySongInst
       , saveSongInst
       , deleteSongInst
       , queryVerse
       , saveVerse
       , deleteVerse
       , queryDataType
       , saveDataType
       , deleteDataType
       , querySIData
       , saveSIData
       , deleteSIData
       ) where

import SongBooks.Common.Types.Internal
import SongBooks.Database.DBT

data InstType = TeX | Sng | Ly | Pdf deriving (Read, Show, Eq)

data DataType = DataType { dtId :: Maybe Id
                         , dtApplicable :: [InstType]
                         , dtName :: Text
                         } deriving (Read, Show, Eq)
  
type SIData = (DataType, Text)

data SongInst = SongInst { siId     :: Maybe Id
                         , siUser   :: User
                         , siKey    :: Maybe Text
                         , siType   :: InstType
                         , siData   :: [SIData]
                         , siVerses :: [Verse]
                         } deriving (Read, Show, Eq)

data VerseType = NormalVerse | Chorus | Bridge
  deriving (Read, Show, Eq)

data Verse = Verse { verseSongId  :: Maybe Id
                   , verseIdx     :: Integer
                   , verseType    :: VerseType
                   , verseContent :: Text
                   } deriving (Read, Show, Eq)

data Song = Song { songId :: Maybe Id
                 , songTitle :: Text
                 , songOriginal :: Maybe Song
                 , songLyricsWriter :: Maybe Person
                 , songMusicWriter :: Maybe Person
                 , songTranslator :: Maybe Person
                 , songCrHolder :: Maybe Publisher
                 , songCrYear :: Maybe Int
                 , songLicense :: Maybe Text
                 , songInsts :: [SongInst]
                 } deriving (Read, Show, Eq)

querySong :: (Monad m) => Song -> DBT m [Song]
querySong = undefined

saveSong :: (Monad m) => Song -> DBT m Bool
saveSong = undefined

deleteSong :: (Monad m) => Song -> DBT m Int
deleteSong = undefined

querySongInst :: (Monad m) => SongInst -> DBT m [SongInst]
querySongInst = undefined

saveSongInst :: (Monad m) => SongInst -> DBT m Bool
saveSongInst = undefined

deleteSongInst :: (Monad m) => SongInst -> DBT m Int
deleteSongInst = undefined

queryVerse :: (Monad m) => Verse -> DBT m [Verse]
queryVerse = undefined

saveVerse :: (Monad m) => Verse -> DBT m Bool
saveVerse = undefined

deleteVerse :: (Monad m) => Verse -> DBT m Int
deleteVerse = undefined

queryDataType :: (Monad m) => DataType -> DBT m [DataType]
queryDataType = undefined

saveDataType :: (Monad m) => DataType -> DBT m Bool
saveDataType = undefined

deleteDataType :: (Monad m) => DataType -> DBT m Int
deleteDataType = undefined

querySIData :: (Monad m) => SIData -> DBT m [SIData]
querySIData = undefined

saveSIData :: (Monad m) => SIData -> DBT m Bool
saveSIData = undefined

deleteSIData :: (Monad m) => SIData -> DBT m Int
deleteSIData = undefined
