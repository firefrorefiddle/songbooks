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
import Control.Lens

data Person = Person { _personId        :: Maybe Id
                     , _personLastName  :: Text
                     , _personFirstName :: Maybe Text
                     } deriving (Read, Show, Eq)

data Publisher = Publisher { _publishedId   :: Maybe Id
                           , _publisherName :: Text
                           } deriving (Read, Show, Eq)

data Style = TeXStyle { _texId :: Maybe Id
                      , _texHeader :: Text } deriving (Read, Show, Eq)

data User = User { _userId     :: Maybe Id
                 , _userLogon  :: Text
                 , _userPerson :: Maybe Person
                 , _userPwHash :: Maybe Text
                 , _userPwSalt :: Maybe Text
                 } deriving (Read, Show, Eq)

data DataType = DataType { _dtId :: Maybe Id
                         , _dtApplicable :: [InstType]
                         , _dtName :: Text
                         } deriving (Read, Show, Eq)

data Verse = Verse { _verseSongId  :: Maybe Id
                   , _verseIdx     :: Integer
                   , _verseType    :: VerseType
                   , _verseContent :: Text
                   } deriving (Read, Show, Eq)

data SongInst = SongInst { _siId     :: Maybe Id
                         , _siUser   :: User
                         , _siKey    :: Maybe Text
                         , _siType   :: InstType
                         , _siData   :: [(DataType, Text)]
                         , _siVerses :: [Verse]
                         } deriving (Read, Show, Eq)

data Song = Song { _songId :: Maybe Id
                 , _songTitle :: Text
                 , _songOriginal :: Maybe Song
                 , _songLyricsWriter :: Maybe Person
                 , _songMusicWriter :: Maybe Person
                 , _songTranslator :: Maybe Person
                 , _songCrHolder :: Maybe Publisher
                 , _songCrYear :: Maybe Int
                 , _songLicense :: Maybe Text
                 , _songInsts :: [SongInst]
                 } deriving (Read, Show, Eq)

data SBPart = SBPart { _sbpTitle :: Maybe Text
                     , _sbpSongs :: [Song]
                     } deriving (Read, Show, Eq)

data SongBook = SongBook { _sbId          :: Maybe Id
                         , _sbName        :: Text
                         , _sbTitle       :: Maybe Text
                         , _sbSubtitle    :: Maybe Text
                         , _sbDescription :: Maybe Text
                         , _sbLicense     :: Maybe Text
                         , _sbCrHolder    :: Maybe Publisher
                         , _sbCrYear      :: Maybe Integer
                         , _sbAuthors     :: [Person]
                         , _sbParts       :: [(Idx, SBPart)]
                         } deriving (Read, Show, Eq)

data DB = SongBooksDB {
  _people :: [Person],
  _users :: [User],
  _publishers :: [Publisher],
  _songs :: [Song],
  _styles :: [Style],
  _songBooks :: [SongBook]
}


makeLenses ''Person
$(deriveSafeCopy 0 'base ''Person)
makeLenses ''Publisher
$(deriveSafeCopy 0 'base ''Publisher)
makeLenses ''Style
$(deriveSafeCopy 0 'base ''Style)
makeLenses ''User
$(deriveSafeCopy 0 'base ''User)
makeLenses ''DataType
$(deriveSafeCopy 0 'base ''InstType)
$(deriveSafeCopy 0 'base ''DataType)
makeLenses ''Verse
$(deriveSafeCopy 0 'base ''VerseType)
$(deriveSafeCopy 0 'base ''Verse)
makeLenses ''SongInst
$(deriveSafeCopy 0 'base ''SongInst)
makeLenses ''Song
$(deriveSafeCopy 0 'base ''Song)
makeLenses ''SBPart
$(deriveSafeCopy 0 'base ''SBPart)
makeLenses ''SongBook
$(deriveSafeCopy 0 'base ''SongBook)
makeLenses ''DB
$(deriveSafeCopy 0 'base ''DB)

-- Transactions are defined to run in either the 'Update' monad
-- or the 'Query' monad.
addPerson :: Person -> Update DB ()
addPerson p
    = do db <- get
         put $ over people (p:) db

viewPeople :: Int -> Query DB [Person]
viewPeople limit
    = do db <- ask
         return $ take limit . _people $ db

-- This will define @ViewMessage@ and @AddMessage@ for us.
$(makeAcidic ''DB
  [ 'addPerson
  , 'viewPeople
  ])
