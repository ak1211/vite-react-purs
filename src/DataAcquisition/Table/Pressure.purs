-- 気圧テーブル
module DataAcquisition.Table.Pressure
  ( Pressure
  , getPressure
  , module DataAcquisition.Table.Table
  , tableName
  ) where

import Prelude
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Either (Either, either)
import Data.Int (decimal, toNumber, toStringAs)
import Data.Maybe (Maybe, maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import DataAcquisition.DataAcquisition (OrderBy, toString)
import DataAcquisition.EnvSensorId (EnvSensorId(..))
import DataAcquisition.Table.Table (getSensorIdStoredInTable)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import JS.BigInt as BigInt
import Sqlite3Wasm.Sqlite3Wasm as Sq3

tableName :: String
tableName = "pressure"

-- データーベースにある気圧測定値
type DbRowPressure
  = { at :: Int, pascal :: Int }

-- 気圧測定値
type Pressure
  = { at :: Milliseconds, hpa :: Number }

-- 気圧テーブルから気圧を取得する
getPressure :: Sq3.SqliteWorker1Promiser -> Sq3.DbId -> EnvSensorId -> OrderBy -> Maybe Int -> Aff (EnvSensorId /\ Array Pressure)
getPressure promiser dbId (EnvSensorId sensorId) orderBy maybeLimits = do
  Console.log query
  result <- Sq3.exec promiser dbId query
  let
    (decoded :: Array (Either JsonDecodeError DbRowPressure)) = map decodeJson result.resultRows
  xs <- traverse (either fail (pure <<< fromDbRowPressure)) decoded
  pure (EnvSensorId sensorId /\ xs)
  where
  query =
    "SELECT `at`,`pascal` FROM `"
      <> tableName
      <> "` WHERE `sensor_id`="
      <> BigInt.toString sensorId
      <> " ORDER BY `at` "
      <> toString orderBy
      <> maybe "" (\n -> " LIMIT " <> toStringAs decimal n) maybeLimits
      <> ";"

  fail = Aff.throwError <<< Aff.error <<< printJsonDecodeError

  fromDbRowPressure :: DbRowPressure -> Pressure
  fromDbRowPressure x = { at: Milliseconds (1000.0 * toNumber x.at), hpa: (toNumber x.pascal) / 100.0 }
