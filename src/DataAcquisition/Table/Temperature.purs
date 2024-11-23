-- 温度テーブル
module DataAcquisition.Table.Temperature
  ( Temperature
  , getTemperature
  , tableName
  , module DataAcquisition.Table.Table
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
tableName = "temperature"

-- データーベースにある気温測定値
type DbRowTemperature
  = { at :: Int, milli_degc :: Int }

-- 気温測定値
type Temperature
  = { at :: Milliseconds, degc :: Number }

-- 温度テーブルから温度を取得する
getTemperature :: Sq3.SqliteWorker1Promiser -> Sq3.DbId -> EnvSensorId -> OrderBy -> Maybe Int -> Aff (EnvSensorId /\ Array Temperature)
getTemperature promiser dbId (EnvSensorId sensorId) orderBy maybeLimits = do
  Console.log query
  result <- Sq3.exec promiser dbId query
  let
    (decoded :: Array (Either JsonDecodeError DbRowTemperature)) = map decodeJson result.resultRows
  xs <- traverse (either fail (pure <<< fromDbRowTemperature)) decoded
  pure (EnvSensorId sensorId /\ xs)
  where
  query =
    "SELECT `at`,`milli_degc` FROM `"
      <> tableName
      <> "` WHERE `sensor_id`="
      <> BigInt.toString sensorId
      <> " ORDER BY `at` "
      <> toString orderBy
      <> maybe "" (\n -> " LIMIT " <> toStringAs decimal n) maybeLimits
      <> ";"

  fail = Aff.throwError <<< Aff.error <<< printJsonDecodeError

  fromDbRowTemperature :: DbRowTemperature -> Temperature
  fromDbRowTemperature x = { at: Milliseconds (1000.0 * toNumber x.at), degc: (toNumber x.milli_degc) / 1000.0 }
