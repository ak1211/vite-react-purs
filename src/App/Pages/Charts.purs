module App.Pages.Charts
  ( ChartsProps
  , mkCharts
  ) where

import Prelude
import App.Config (Config)
import App.DataAcquisition.Types (EnvSensorId, PressureChartSeries, RelativeHumidityChartSeries, TemperatureChartSeries)
import App.PressureChart as PressureChart
import App.RelativeHumidityChart as RelativeHumidityChart
import App.Shadcn.Shadcn as Shadcn
import App.SqliteDatabaseState (SqliteDatabaseState)
import App.TemperatureChart as TemperatureChart
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Data.Int (toNumber)
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse, traverse_)
import Data.Tuple.Nested (type (/\), tuple3, uncurry3, (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Effect.Exception (Error)
import JS.BigInt as BigInt
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (Component, component, useState)
import React.Basic.Hooks as React
import Sqlite3Wasm.Sqlite3Wasm as Sq3
import Unsafe.Coerce (unsafeCoerce)

type ChartsProps
  = { config :: Config
    , sqliteDatabaseState :: SqliteDatabaseState
    }

type State
  = { counter :: Int
    , temperatureSeries :: Array TemperatureChartSeries
    , relativeHumiditySeries :: Array RelativeHumidityChartSeries
    , pressureSeries :: Array PressureChartSeries
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
                  { onClick: capture_ versionButtonOnClickHandler
                  , className: "m-1"
                  , variant: "secondary"
                  }
                  [ DOM.text "SQLite version"
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

versionButtonOnClickHandler :: Effect Unit
versionButtonOnClickHandler = Aff.runAff_ (either fail success) $ configGet
  where
  configGet :: Aff Sq3.ConfigGetResult
  configGet = Sq3.configGet =<< Sq3.createWorker1Promiser

  fail :: Error -> Effect Unit
  fail err = Console.error $ "config-get failed: " <> Aff.message err

  success :: Sq3.ConfigGetResult -> Effect Unit
  success r = Console.logShow r

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
  success :: Array TemperatureChartSeries -> Effect Unit
  success xs = setState _ { temperatureSeries = xs }

  mkRecord :: EnvSensorId -> Array DbRowTemperature -> TemperatureChartSeries
  mkRecord sensorId rowTemperature =
    let
      record x = { at: x.at, degc: (toNumber x.milli_degc) / 1000.0 }
    in
      sensorId /\ map record rowTemperature

  go :: Sq3.SqliteWorker1Promiser -> Sq3.DbId -> String -> Aff (Array TemperatureChartSeries)
  go promiser dbId dbTable = do
    sensors <- getSensorIdStoredInTable promiser dbId dbTable
    traverse (\s -> mkRecord s <$> getTemperature promiser dbId s) sensors

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

  success :: Array RelativeHumidityChartSeries -> Effect Unit
  success xs = setState _ { relativeHumiditySeries = xs }

  mkRecord :: EnvSensorId -> Array DbRowRelativeHumidity -> RelativeHumidityChartSeries
  mkRecord sensorId rowRelativeHumidity =
    let
      record x = { at: x.at, percent: (toNumber x.ppm_rh) / 10000.0 }
    in
      sensorId /\ map record rowRelativeHumidity

  go :: Sq3.SqliteWorker1Promiser -> Sq3.DbId -> String -> Aff (Array RelativeHumidityChartSeries)
  go promiser dbId dbTable = do
    sensors <- getSensorIdStoredInTable promiser dbId dbTable
    traverse (\s -> mkRecord s <$> getRelativeHumidity promiser dbId s) sensors

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

  success :: Array PressureChartSeries -> Effect Unit
  success xs = setState _ { pressureSeries = xs }

  mkRecord :: EnvSensorId -> Array DbRowPressure -> PressureChartSeries
  mkRecord sensorId rowPressure =
    let
      record x = { at: x.at, hpa: (toNumber x.pascal) / 100.0 }
    in
      sensorId /\ map record rowPressure

  go :: Sq3.SqliteWorker1Promiser -> Sq3.DbId -> String -> Aff (Array PressureChartSeries)
  go promiser dbId dbTable = do
    sensors <- getSensorIdStoredInTable promiser dbId dbTable
    traverse (\s -> mkRecord s <$> getPressure promiser dbId s) sensors

type StateHook
  = (State /\ ((State -> State) -> Effect Unit))

{-}
 データーベースにクエリを発行する
 -}
type DbRowTemperature
  = { at :: Int, milli_degc :: Int }

type DbRowRelativeHumidity
  = { at :: Int, ppm_rh :: Int }

type DbRowPressure
  = { at :: Int, pascal :: Int }

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
