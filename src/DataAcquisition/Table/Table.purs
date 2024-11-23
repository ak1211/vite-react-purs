module DataAcquisition.Table.Table
  ( getSensorIdStoredInTable
  ) where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Data.Traversable (traverse)
import DataAcquisition.EnvSensorId (EnvSensorId)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Sqlite3Wasm.Sqlite3Wasm as Sq3
import Unsafe.Coerce (unsafeCoerce)

-- テーブルに格納されているセンサーＩＤを取得する
getSensorIdStoredInTable :: Sq3.SqliteWorker1Promiser -> Sq3.DbId -> String -> Aff (Array EnvSensorId)
getSensorIdStoredInTable promiser dbId targetTable = do
  result <- Sq3.exec promiser dbId $ "SELECT DISTINCT `sensor_id` FROM `" <> targetTable <> "` ORDER BY `sensor_id`;"
  let
    decoded = map decodeJson_ result.resultRows
  traverse (either fail success) decoded
  where
  fail = Aff.throwError <<< Aff.error

  decodeJson_ :: Json -> Either String { sensor_id :: EnvSensorId }
  decodeJson_ json = lmap (\e -> printJsonDecodeError e <> " " <> (unsafeCoerce json :: String)) $ decodeJson json

  success r = pure r.sensor_id
