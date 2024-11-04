module Sqlite3Wasm.Sqlite3Wasm
  ( CloseResult
  , ConfigGetResult
  , DbId
  , ExecResult
  , OpenResult
  , OpfsDatabaseFilePath(..)
  , SqliteResponse
  , SqliteVersion
  , SqliteWorker1Promiser
  , close
  , configGet
  , createWorker1Promiser
  , exec
  , open
  , overwriteOpfsFileWithSpecifiedArrayBuffer
  ) where

import Prelude
import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either, either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)

-- SQLite Version numbers
type SqliteVersion
  = { libVersion :: String
    , libVersionNumber :: Number
    , sourceId :: String
    , downloadVersion :: Number
    }

{-
  https://sqlite.org/wasm/doc/trunk/api-worker1.md
  SQLite WASM Worker1 API
-}
foreign import data SqliteWorker1Promiser :: Type

type DbId
  = Json

foreign import configGetImpl :: EffectFn1 SqliteWorker1Promiser (Promise Json)

foreign import openImpl :: EffectFn2 SqliteWorker1Promiser String (Promise Json)

foreign import closeImpl :: EffectFn2 SqliteWorker1Promiser DbId (Promise Json)

foreign import execImpl :: EffectFn3 SqliteWorker1Promiser DbId String (Promise Json)

foreign import createWorker1PromiserImpl :: Effect (Promise SqliteWorker1Promiser)

foreign import overwriteOpfsFileWithSpecifiedArrayBufferImpl :: EffectFn2 String ArrayBuffer (Promise Unit)

-- SQLiteWorker1Promiserを得る
createWorker1Promiser :: Aff SqliteWorker1Promiser
createWorker1Promiser = createWorker1PromiserImpl # toAffE

-- SQLiteメソッドの応答
type SqliteResponse
  = { type :: String -- メソッド名または"error"
    , messageId :: String -- 何らかの値が返却されてくる
    , result :: Json -- 処理結果
    }

-- config-getメソッドの結果
type ConfigGetResult
  = { version :: SqliteVersion
    , bigIntEnabled :: Boolean
    , vfsList :: Array String
    }

-- config-getメソッド呼び出し
configGet :: SqliteWorker1Promiser -> Aff ConfigGetResult
configGet promiser = do
  (respondJson :: Json) <- runEffectFn1 configGetImpl promiser # toAffE
  (respond :: SqliteResponse) <- throwErrorOnFailure $ decodeJson respondJson
  -- TODO: type "error"のことは今は気にしないことにする
  throwErrorOnFailure $ decodeJson respond.result

-- openメソッドの結果
type OpenResult
  = { filename :: String
    , dbId :: DbId
    , persistent :: Boolean
    , vfs :: String
    }

-- openメソッド呼び出し
open :: SqliteWorker1Promiser -> OpfsDatabaseFilePath -> Aff OpenResult
open promiser opfsFilePath = do
  (respondJson :: Json) <- runEffectFn2 openImpl promiser (unwrap opfsFilePath) # toAffE
  (respond :: SqliteResponse) <- throwErrorOnFailure $ decodeJson respondJson
  -- TODO: type "error"のことは今は気にしないことにする
  throwErrorOnFailure $ decodeJson respond.result

-- closeメソッドの結果
type CloseResult
  = { filename :: Maybe String
    }

-- closeメソッド呼び出し
close :: SqliteWorker1Promiser -> DbId -> Aff CloseResult
close promiser dbId = do
  (respondJson :: Json) <- runEffectFn2 closeImpl promiser dbId # toAffE
  (respond :: SqliteResponse) <- throwErrorOnFailure $ decodeJson respondJson
  -- TODO: type "error"のことは今は気にしないことにする
  throwErrorOnFailure $ decodeJson respond.result

-- execメソッドの結果
type ExecResult
  = { resultRows :: Array Json }

-- execメソッド呼び出し
exec :: SqliteWorker1Promiser -> DbId -> String -> Aff ExecResult
exec promiser dbId sql = do
  (respondJson :: Json) <- runEffectFn3 execImpl promiser dbId sql # toAffE
  (respond :: SqliteResponse) <- throwErrorOnFailure $ decodeJson respondJson
  -- TODO: type "error"のことは今は気にしないことにする
  throwErrorOnFailure $ decodeJson respond.result

--
throwErrorOnFailure :: forall a. Either JsonDecodeError a -> Aff a
throwErrorOnFailure = either (Aff.throwError <<< Aff.error <<< printJsonDecodeError) pure

-- OPFS上のSQLite3データーベースファイルパスを指定する型
newtype OpfsDatabaseFilePath
  = OpfsDatabaseFilePath String

derive instance genericOpfsDatabaseFilePath :: Generic OpfsDatabaseFilePath _

derive instance newtypeOpfsDatabaseFilePath :: Newtype OpfsDatabaseFilePath _

instance showBaseURL :: Show OpfsDatabaseFilePath where
  show = genericShow

-- OPFS上のSQLite3データーベースファイルを指定のバッファで上書きする。
overwriteOpfsFileWithSpecifiedArrayBuffer :: OpfsDatabaseFilePath -> ArrayBuffer -> Aff Unit
overwriteOpfsFileWithSpecifiedArrayBuffer opfsDb buffer = runEffectFn2 overwriteOpfsFileWithSpecifiedArrayBufferImpl (unwrap opfsDb) buffer # toAffE
