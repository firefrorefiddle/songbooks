{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module SongBooks.Entities where

import Data.Acid
import Data.SafeCopy
import SongBooks.Common.Types
import Control.Monad.Reader
import Control.Monad.State

data Person = Person { personId        :: Maybe Id
                     , personLastName  :: Text
                     , personFirstName :: Maybe Text
                     } deriving (Read, Show, Eq)

data Publisher = Publisher { publishedId   :: Maybe Id
                           , publisherName :: Text
                           } deriving (Read, Show, Eq)


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

data DataType = DataType { dtId :: Maybe Id
                         , dtApplicable :: [InstType]
                         , dtName :: Text
                         } deriving (Read, Show, Eq)

data SongInst = SongInst { siId     :: Maybe Id
                         , siUser   :: User
                         , siKey    :: Maybe Text
                         , siType   :: InstType
                         , siData   :: [(DataType, Text)]
                         , siVerses :: [Verse]
                         } deriving (Read, Show, Eq)

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


data User = User { userId     :: Maybe Id
                 , userLogon  :: Text
                 , userPerson :: Maybe Person
                 , userPwHash :: Maybe Text
                 , userPwSalt :: Maybe Text
                 } deriving (Read, Show, Eq)

$(deriveSafeCopy 0 'base ''Person)
$(deriveSafeCopy 0 'base ''Publisher)
$(deriveSafeCopy 0 'base ''Style)
$(deriveSafeCopy 0 'base ''SBPart)
$(deriveSafeCopy 0 'base ''SongBook)
$(deriveSafeCopy 0 'base ''InstType)
$(deriveSafeCopy 0 'base ''DataType)
$(deriveSafeCopy 0 'base ''SongInst)
$(deriveSafeCopy 0 'base ''VerseType)
$(deriveSafeCopy 0 'base ''Verse)
$(deriveSafeCopy 0 'base ''Song)
$(deriveSafeCopy 0 'base ''User)

data DB = SongBooksDB {
  getPeople :: [Person],
  getUsers :: [User],
  getPublishers :: [Publisher],
  getSongs :: [Song],
  getStyles :: [Style],
  getSongBooks :: [SongBook]
}

$(deriveSafeCopy 0 'base ''DB)

-- Transactions are defined to run in either the 'Update' monad
-- or the 'Query' monad.
addPerson :: Person -> Update DB ()
addPerson p
    = do db <- get
         put $ db { getPeople = p : getPeople db }

viewPeople :: Int -> Query DB [Person]
viewPeople limit
    = do db <- ask
         return $ take limit . getPeople $ db

-- This will define @ViewMessage@ and @AddMessage@ for us.
$(makeAcidic ''DB ['addPerson, 'viewPeople])
