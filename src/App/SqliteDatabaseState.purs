module App.SqliteDatabaseState
  ( SqliteDatabaseState
  , SqliteDatabaseStateHook
  , UpdateSqliteDatabaseState
  , closeSqliteDatabase
  , initialSqliteDatabaseState
  , openSqliteDatabase
  ) where

import Prelude
import Data.Either (either)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (uncurry)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (Error)
import React.Basic.Hooks (type (/\), (/\))
import Sqlite3Wasm.Sqlite3Wasm as Sq3

type SqliteDatabaseState
  = { maybePromiser :: Maybe Sq3.SqliteWorker1Promiser
    , maybeDbId :: Maybe Sq3.DbId
    }

initialSqliteDatabaseState :: SqliteDatabaseState
initialSqliteDatabaseState =
  { maybePromiser: Nothing
  , maybeDbId: Nothing
  }

type UpdateSqliteDatabaseState
  = (SqliteDatabaseState -> SqliteDatabaseState) -> Effect Unit

type SqliteDatabaseStateHook
  = (SqliteDatabaseState /\ UpdateSqliteDatabaseState)

-- OPFS上のデーターベースファイルを開く
openSqliteDatabase :: SqliteDatabaseStateHook -> Sq3.OpfsDatabaseFilePath -> Effect Unit
openSqliteDatabase (_state /\ setState) filepath =
  Aff.runAff_ (either fail success) do
    promiser <- Sq3.createWorker1Promiser -- SQLite3 WASM Worker1 promiser を得る
    liftEffect $ setState _ { maybePromiser = Just promiser }
    Sq3.open promiser filepath
  where
  fail :: Error -> Effect Unit
  fail err = Console.error $ "database open fail: " <> Aff.message err

  success :: Sq3.OpenResult -> Effect Unit
  success result = do
    Console.log $ "database '" <> result.filename <> "' opened"
    setState _ { maybeDbId = Just result.dbId }

-- データーベースを閉じる
closeSqliteDatabase :: SqliteDatabaseStateHook -> Effect Unit
closeSqliteDatabase (state /\ setState) =
  maybe mempty (uncurry close) do
    promiser <- state.maybePromiser
    dbId <- state.maybeDbId
    pure (promiser /\ dbId)
  where
  close :: Sq3.SqliteWorker1Promiser -> Sq3.DbId -> Effect Unit
  close promiser dbId = Aff.runAff_ (either fail $ const success) $ Sq3.close promiser dbId

  fail :: Error -> Effect Unit
  fail err = Console.error $ "database close fail: " <> Aff.message err

  success :: Effect Unit
  success = do
    Console.log "database closed"
    setState _ { maybePromiser = Nothing, maybeDbId = Nothing }
