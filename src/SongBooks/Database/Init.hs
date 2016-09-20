module SongBooks.Database.Init where

import SongBooks.Database.DBT
import Control.Monad.IO.Class

createPersonStmt =
  "CREATE TABLE person (                               \
  \ id          INTEGER     PRIMARY KEY AUTOINCREMENT, \
  \ last_name   VARCHAR(30) NOT NULL,                  \
  \ first_name  VARCHAR(30))"

createPublisherStmt =
  "CREATE TABLE publisher (                            \
  \ id          INTEGER     PRIMARY KEY AUTOINCREMENT, \
  \ name        VARCHAR(30) NOT NULL)"

createUserStmt =
  "CREATE TABLE user (                                 \
   \ id         INTEGER     PRIMARY KEY AUTOINCREMENT, \
   \ logon      VARCHAR(15) UNIQUE NOT NULL,           \
   \ person_id  INTEGER     REFERENCES person(id),     \
   \ pw_hash    VARCHAR(30),                           \
   \ pw_salt    VARCHAR(30))"

createSongStmt =
  "CREATE TABLE song (                                         \
  \  id            INTEGER        PRIMARY KEY AUTOINCREMENT,    \
  \  title         VARCHAR (100)  NOT NULL,                     \
  \  original_id   INTEGER        REFERENCES song(id),          \
  \  lyrics_writer INTEGER        REFERENCES person(id),        \
  \  music_writer  INTEGER        REFERENCES person(id),        \
  \  translator    INTEGER        REFERENCES person(id),        \
  \  cr_holder     INTEGER        REFERENCES publisher(id),     \
  \  cr_year       INTEGER        CHECK (cr_year BETWEEN 1      \
  \                                              AND 2100),     \
  \  license       VARCHAR(20));"
   
createSongInstStmt =
  "CREATE TABLE song_inst (                                              \
  \  id        INTEGER       PRIMARY KEY AUTOINCREMENT,                  \
  \  song_id   INTEGER       NOT NULL                                    \
  \                          REFERENCES song(id),                        \
  \  user_id   INTEGER       NOT NULL                                    \
  \                          REFERENCES user(id),                        \
  \  key       VARCHAR(4),                                               \
  \  type      CHAR(10)      CHECK(type IN ('tex', 'sng', 'ly', 'pdf'))  \
  \  );"

createDataTypeStmt =
  "CREATE TABLE data_type (                                              \  
  \  id            VARCHAR(40)   PRIMARY KEY,                            \
  \  name          VARCHAR(20)   NOT NULL);"

createDataTypeApplicStmt =
  "CREATE TABLE data_type_applic ( \
  \  id            VARCHAR(40) NOT NULL REFERENCES data_type(id), \
  \  applicable    CHAR(10) NOT NULL CHECK(applicable IN('tex', 'sng', 'ly', 'pdf')), \
  \  UNIQUE(id, applicable)"

createInstDataStmt =
  "CREATE TABLE inst_data (                                              \
  \  song_inst_id  INTEGER       NOT NULL REFERENCES song_inst(id),      \
  \  type          VARCHAR(40)   NOT NULL REFERENCES data_type(id)       \
  \  data          VARCHAR(2000) NOT NULL)"

createVerseStmt =
  "CREATE TABLE verse (                                          \
  \  song_id   INTEGER       NOT NULL REFERENCES song_inst (id), \
  \  idx       INTEGER       NOT NULL,                           \
  \  type      VARCHAR(10)   NOT NULL                            \
  \                CHECK(type IN ('verse', 'chorus', 'bridge')), \
  \  content   VARCHAR(2000) NOT NULL,                           \
  \  UNIQUE (song_id, idx))"


doCreateTables :: (MonadIO m) => DBT m ()
doCreateTables = do
  quickQuery createPersonStmt []
  quickQuery createPublisherStmt []  
  quickQuery createUserStmt []
  quickQuery createSongStmt []
  quickQuery createSongInstStmt []
  quickQuery createVerseStmt []  
  return ()
