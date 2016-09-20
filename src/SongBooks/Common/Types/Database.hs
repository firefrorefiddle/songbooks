module SongBooks.Common.Types.Database where

import Data.Text

type Id = Integer

data Person = Person { personId        :: Maybe Id
                     , personLastName  :: Text
                     , personFirstName :: Maybe Text
                     } deriving (Read, Show, Eq)
 
data Publisher = Publisher { publishedId   :: Maybe Id
                           , publisherName :: Text
                           } deriving (Read, Show, Eq)

data User = User { userId     :: Maybe Id
                 , userLogon  :: Text
                 , userPerson :: Maybe Person
                 , userPwHash :: Maybe Text
                 , userPwSalt :: Maybe Text
                 } deriving (Read, Show, Eq)

data InstType = Tex | Sng | Ly | Pdf deriving (Read, Show, Eq)

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

