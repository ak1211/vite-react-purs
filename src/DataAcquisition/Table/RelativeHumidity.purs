-- 相対湿度テーブル
module DataAcquisition.Table.RelativeHumidity
  ( DbRowRelativeHumidity
  , RelativeHumidity
  , getRelativeHumidity
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
tableName = "relative_humidity"

-- データーベースにある相対湿度測定値
type DbRowRelativeHumidity
  = { at :: Int, ppm_rh :: Int }

-- 相対湿度測定値
type RelativeHumidity
  = { at :: Milliseconds, percent :: Number }

-- 湿度テーブルから温度を取得する
getRelativeHumidity :: Sq3.SqliteWorker1Promiser -> Sq3.DbId -> EnvSensorId -> OrderBy -> Maybe Int -> Aff (EnvSensorId /\ Array RelativeHumidity)
getRelativeHumidity promiser dbId (EnvSensorId sensorId) orderBy maybeLimits = do
  Console.log query
  result <- Sq3.exec promiser dbId query
  let
    (decoded :: Array (Either JsonDecodeError DbRowRelativeHumidity)) = map decodeJson result.resultRows
  xs <- traverse (either fail (pure <<< fromDbRowRelativeHumidity)) decoded
  pure (EnvSensorId sensorId /\ xs)
  where
  query =
    "SELECT `at`,`ppm_rh` FROM `"
      <> tableName
      <> "` WHERE `sensor_id`="
      <> BigInt.toString sensorId
      <> " ORDER BY `at` "
      <> toString orderBy
      <> maybe "" (\n -> " LIMIT " <> toStringAs decimal n) maybeLimits
      <> ";"

  fail = Aff.throwError <<< Aff.error <<< printJsonDecodeError

  fromDbRowRelativeHumidity :: DbRowRelativeHumidity -> RelativeHumidity
  fromDbRowRelativeHumidity x = { at: Milliseconds (1000.0 * toNumber x.at), percent: (toNumber x.ppm_rh) / 10000.0 }
