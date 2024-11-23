module App.Pages.Charts
  ( ChartsProps
  , mkCharts
  ) where

import Prelude
import App.Config (Config)
import App.PressureChart as PressureChart
import App.RelativeHumidityChart as RelativeHumidityChart
import App.SqliteDatabaseState (SqliteDatabaseState)
import App.TemperatureChart as TemperatureChart
import Data.Either (either)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse, traverse_)
import Data.Tuple.Nested (type (/\), tuple3, uncurry3, (/\))
import DataAcquisition.DataAcquisition (OrderBy(..))
import DataAcquisition.EnvSensorId (EnvSensorId)
import DataAcquisition.Table.Pressure (Pressure, getPressure)
import DataAcquisition.Table.RelativeHumidity (RelativeHumidity, getRelativeHumidity)
import DataAcquisition.Table.Table (getSensorIdStoredInTable)
import DataAcquisition.Table.Temperature (Temperature, getTemperature)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Effect.Exception (Error)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (Component, component, useState)
import React.Basic.Hooks as React
import Shadcn.Components as Shadcn
import Sqlite3Wasm.Sqlite3Wasm as Sq3

type ChartsProps
  = { config :: Config
    , sqliteDatabaseState :: SqliteDatabaseState
    }

type State
  = { counter :: Int
    , temperatureSeries :: Array (EnvSensorId /\ Array Temperature)
    , relativeHumiditySeries :: Array (EnvSensorId /\ Array RelativeHumidity)
    , pressureSeries :: Array (EnvSensorId /\ Array Pressure)
    }

initialState :: State
initialState =
  { counter: 0
  , temperatureSeries: []
  , relativeHumiditySeries: []
  , pressureSeries: []
  }

mkCharts :: Component ChartsProps
mkCharts = do
  temperatureChart <- TemperatureChart.mkComponent
  relativeHumidityChart <- RelativeHumidityChart.mkComponent
  pressureChart <- PressureChart.mkComponent
  component "Charts" \(props :: ChartsProps) -> React.do
    -- useStateフックを使って、stateとsetStateを得る
    stateHook@(state /\ setState) <- useState initialState
    --
    pure
      $ DOM.div
          { children:
              [ DOM.h1_ [ DOM.text "Charts" ]
              , DOM.p_ [ DOM.text "Try clicking the button!" ]
              , Shadcn.button
                  { onClick:
                      capture_ do
                        setState _ { counter = state.counter + 1 }
                  , className: "m-1"
                  , variant: "destructive"
                  }
                  [ DOM.text "Clicks: "
                  , DOM.text (show state.counter)
                  ]
              , Shadcn.button
                  { onClick:
                      capture_ $ sensorIdButtonOnClickHandler "temperature" props.sqliteDatabaseState
                  , className: "m-1"
                  , variant: "secondary"
                  }
                  [ DOM.text "temperatureテーブルに入ってるsensor_id" ]
              , Shadcn.button
                  { onClick:
                      capture_ $ getTemperatureButtonOnClickHandler stateHook props.sqliteDatabaseState
                  , className: "m-1"
                  , variant: ""
                  }
                  [ DOM.text "temperature" ]
              , Shadcn.button
                  { onClick:
                      capture_ $ sensorIdButtonOnClickHandler "relative_humidity" props.sqliteDatabaseState
                  , className: "m-1"
                  , variant: "secondary"
                  }
                  [ DOM.text "relative_humidityテーブルに入ってるsensor_id" ]
              , Shadcn.button
                  { onClick:
                      capture_ $ getRelativeHumidityButtonOnClickHandler stateHook props.sqliteDatabaseState
                  , className: "m-1"
                  , variant: ""
                  }
                  [ DOM.text "relative_humidity" ]
              , Shadcn.button
                  { onClick:
                      capture_ $ sensorIdButtonOnClickHandler "pressure" props.sqliteDatabaseState
                  , className: "m-1"
                  , variant: "secondary"
                  }
                  [ DOM.text "pressureテーブルに入ってるsensor_id" ]
              , Shadcn.button
                  { onClick:
                      capture_ $ getPressureButtonOnClickHandler stateHook props.sqliteDatabaseState
                  , className: "m-1"
                  , variant: ""
                  }
                  [ DOM.text "pressure" ]
              , temperatureChart state.temperatureSeries
              , relativeHumidityChart state.relativeHumiditySeries
              , pressureChart state.pressureSeries
              ]
          }

sensorIdButtonOnClickHandler :: String -> SqliteDatabaseState -> Effect Unit
sensorIdButtonOnClickHandler dbTable sqlite =
  Aff.runAff_ (either fail success)
    $ maybe (Aff.throwError $ Aff.error "database file not opened") (uncurry3 getSensorIdStoredInTable) do
        promiser <- sqlite.maybePromiser
        dbId <- sqlite.maybeDbId
        pure $ tuple3 promiser dbId dbTable
  where
  fail :: Error -> Effect Unit
  fail err = Console.error $ "getSensorIdStoredInTable failed: " <> Aff.message err

  success :: Array EnvSensorId -> Effect Unit
  success = traverse_ Console.logShow

getTemperatureButtonOnClickHandler :: StateHook -> SqliteDatabaseState -> Effect Unit
getTemperatureButtonOnClickHandler (_state /\ setState) sqlite =
  Aff.runAff_ (either fail success)
    $ maybe (Aff.throwError $ Aff.error "database file not opened") (uncurry3 go) do
        promiser <- sqlite.maybePromiser
        dbId <- sqlite.maybeDbId
        pure $ tuple3 promiser dbId "temperature"
  where
  fail :: Error -> Effect Unit
  fail err = Console.error $ Aff.message err

  -- 取得したデーターでstate変数を上書きする
  success :: Array (EnvSensorId /\ Array Temperature) -> Effect Unit
  success xs = setState _ { temperatureSeries = xs }

  go :: Sq3.SqliteWorker1Promiser -> Sq3.DbId -> String -> Aff (Array (EnvSensorId /\ Array Temperature))
  go promiser dbId dbTable = do
    sensorIds <- getSensorIdStoredInTable promiser dbId dbTable
    traverse (\targetId -> getTemperature promiser dbId targetId ASC $ Just 10000) sensorIds

getRelativeHumidityButtonOnClickHandler :: StateHook -> SqliteDatabaseState -> Effect Unit
getRelativeHumidityButtonOnClickHandler (_state /\ setState) sqlite =
  Aff.runAff_ (either fail success)
    $ maybe (Aff.throwError $ Aff.error "database file not opened") (uncurry3 go) do
        promiser <- sqlite.maybePromiser
        dbId <- sqlite.maybeDbId
        pure $ tuple3 promiser dbId "relative_humidity"
  where
  fail :: Error -> Effect Unit
  fail err = Console.error $ Aff.message err

  -- 取得したデーターでstate変数を上書きする
  success :: Array (EnvSensorId /\ Array RelativeHumidity) -> Effect Unit
  success xs = setState _ { relativeHumiditySeries = xs }

  go :: Sq3.SqliteWorker1Promiser -> Sq3.DbId -> String -> Aff (Array (EnvSensorId /\ Array RelativeHumidity))
  go promiser dbId dbTable = do
    sensorIds <- getSensorIdStoredInTable promiser dbId dbTable
    traverse (\targetId -> getRelativeHumidity promiser dbId targetId ASC $ Just 10000) sensorIds

getPressureButtonOnClickHandler :: StateHook -> SqliteDatabaseState -> Effect Unit
getPressureButtonOnClickHandler (_state /\ setState) sqlite =
  Aff.runAff_ (either fail success)
    $ maybe (Aff.throwError $ Aff.error "database file not opened") (uncurry3 go) do
        promiser <- sqlite.maybePromiser
        dbId <- sqlite.maybeDbId
        pure $ tuple3 promiser dbId "pressure"
  where
  fail :: Error -> Effect Unit
  fail err = Console.error $ Aff.message err

  success :: Array (EnvSensorId /\ Array Pressure) -> Effect Unit
  success xs = setState _ { pressureSeries = xs }

  go :: Sq3.SqliteWorker1Promiser -> Sq3.DbId -> String -> Aff (Array (EnvSensorId /\ Array Pressure))
  go promiser dbId dbTable = do
    sensorIds <- getSensorIdStoredInTable promiser dbId dbTable
    traverse (\targetId -> getPressure promiser dbId targetId ASC $ Just 10000) sensorIds

type StateHook
  = (State /\ ((State -> State) -> Effect Unit))

{-}
 データーベースにクエリを発行する
 -}
{-}
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
-- 温度テーブルから温度を取得する
getTemperature :: Sq3.SqliteWorker1Promiser -> Sq3.DbId -> EnvSensorId -> Aff (Array DbRowTemperature)
getTemperature promiser dbId sensorId = do
  let
    query =
      "SELECT `at`,`milli_degc` FROM `temperature` WHERE `sensor_id`="
        <> BigInt.toString (unwrap sensorId)
        <> " ORDER BY `at` ASC LIMIT 10000;"
  Console.log query
  result <- Sq3.exec promiser dbId query
  let
    decoded = map decodeJson_ result.resultRows
  traverse (either fail pure) decoded
  where
  fail = Aff.throwError <<< Aff.error <<< printJsonDecodeError

  decodeJson_ :: Json -> Either JsonDecodeError DbRowTemperature
  decodeJson_ = decodeJson
-- 湿度テーブルから温度を取得する
getRelativeHumidity :: Sq3.SqliteWorker1Promiser -> Sq3.DbId -> EnvSensorId -> Aff (Array DbRowRelativeHumidity)
getRelativeHumidity promiser dbId sensorId = do
  let
    query =
      "SELECT `at`,`ppm_rh` FROM `relative_humidity` WHERE `sensor_id`="
        <> BigInt.toString (unwrap sensorId)
        <> " ORDER BY `at` ASC LIMIT 10000;"
  Console.log query
  result <- Sq3.exec promiser dbId query
  let
    decoded = map decodeJson_ result.resultRows
  traverse (either fail pure) decoded
  where
  fail = Aff.throwError <<< Aff.error <<< printJsonDecodeError

  decodeJson_ :: Json -> Either JsonDecodeError DbRowRelativeHumidity
  decodeJson_ = decodeJson
-- 気圧テーブルから気圧を取得する
getPressure :: Sq3.SqliteWorker1Promiser -> Sq3.DbId -> EnvSensorId -> Aff (Array DbRowPressure)
getPressure promiser dbId sensorId = do
  let
    query =
      "SELECT `at`,`pascal` FROM `pressure` WHERE `sensor_id`="
        <> BigInt.toString (unwrap sensorId)
        <> " ORDER BY `at` ASC LIMIT 10000;"
  Console.log query
  result <- Sq3.exec promiser dbId query
  let
    decoded = map decodeJson_ result.resultRows
  traverse (either fail pure) decoded
  where
  fail = Aff.throwError <<< Aff.error

  decodeJson_ :: Json -> Either String DbRowPressure
  decodeJson_ json = lmap (\e -> printJsonDecodeError e <> " " <> (unsafeCoerce json :: String)) $ decodeJson json

  -}
