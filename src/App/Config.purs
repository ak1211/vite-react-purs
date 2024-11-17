module App.Config
  ( Config
  , devConfig
  ) where

import Sqlite3Wasm.Sqlite3Wasm (OpfsDatabaseFilePath(..))

type Config
  = { sqliteDatabaseFile :: OpfsDatabaseFilePath -- OPFS上のデータベースファイルパス
    }

devConfig :: Config
devConfig =
  { sqliteDatabaseFile: OpfsDatabaseFilePath "env_database.sqlite3"
  }
