module App.Pages.Home
  ( HomeProps
  , mkHome
  ) where

import Prelude
import App.DataAcquisition.Types (EnvSensorId, PressureChartSeries, RelativeHumidityChartSeries, TemperatureChartSeries)
import App.TemperatureChart as TemperatureChart
import App.PressureChart as PressureChart
import App.RelativeHumidityChart as RelativeHumidityChart
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Either (Either, either)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse, traverse_)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\), uncurry3, tuple3)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class.Console (log, logShow, error, errorShow)
import Effect.Exception (Error)
import JS.BigInt as BigInt
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_, targetFiles)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, component, useEffect, useState)
import React.Basic.Hooks as React
import Sqlite3Wasm.Sqlite3Wasm (ConfigGetResult, DbId, OpenResult, OpfsDatabaseFilePath(..), SqliteWorker1Promiser)
import Sqlite3Wasm.Sqlite3Wasm as Sq3
import Web.File.File (File)
import Web.File.File as File
import Web.File.FileList (FileList)
import Web.File.FileList as FileList
import Web.File.FileReader.Aff (readAsArrayBuffer)

type HomeProps
  = Unit

type State
  = { counter :: Int
    , promiser :: Maybe SqliteWorker1Promiser
    , dbId :: Maybe DbId
    , temperatureSeries :: Array TemperatureChartSeries
    , relativeHumiditySeries :: Array RelativeHumidityChartSeries
    , pressureSeries :: Array PressureChartSeries
    }

initialState :: State
initialState =
  { counter: 0
  , promiser: Nothing
  , dbId: Nothing
  , temperatureSeries: []
  , relativeHumiditySeries: []
  , pressureSeries: []
  }

mkHome :: Component HomeProps
mkHome = do
  temperatureChart <- TemperatureChart.mkComponent
  relativeHumidityChart <- RelativeHumidityChart.mkComponent
  pressureChart <- PressureChart.mkComponent
  component "Home" \_props -> React.do
    -- ステートフックを使って、stateとsetStateを得る
    stateHook@(state /\ setState) <- useState initialState
    -- 副作用フック(useEffect)
    useEffect unit do
      let
        fail :: Error -> Effect Unit
        fail = log <<< Aff.message

        update :: SqliteWorker1Promiser -> Effect Unit
        update newPromiser = setState _ { promiser = Just newPromiser }
      -- SQLite3 WASM Worker1 promiser を得る
      Aff.runAff_ (either fail update) Sq3.createWorker1Promiser
      -- 副作用フックのクリーンアップ関数を返却する
      pure $ void $ closeDatabaseHandler stateHook
    --
    pure
      $ DOM.div
          { children:
              [ DOM.h1_ [ DOM.text "Home" ]
              , DOM.p_ [ DOM.text "Try clicking the button!" ]
              , DOM.button
                  { onClick:
                      capture_ do
                        setState _ { counter = state.counter + 1 }
                  , children:
                      [ DOM.text "Clicks: "
                      , DOM.text (show state.counter)
                      ]
                  }
              , DOM.br {}
              , DOM.button
                  { onClick: capture_ versionButtonOnClickHandler
                  , children:
                      [ DOM.text "SQLite version"
                      ]
                  }
              , DOM.br {}
              , DOM.button
                  { onClick:
                      capture_ $ sensorIdButtonOnClickHandler stateHook "temperature"
                  , children:
                      [ DOM.text "temperatureテーブルに入ってるsensor_id" ]
                  }
              , DOM.br {}
              , DOM.button
                  { onClick:
                      capture_ $ getTemperatureButtonOnClickHandler stateHook
                  , children:
                      [ DOM.text "temperature" ]
                  }
              , DOM.br {}
              , DOM.button
                  { onClick:
                      capture_ $ sensorIdButtonOnClickHandler stateHook "relative_humidity"
                  , children:
                      [ DOM.text "relative_humidityテーブルに入ってるsensor_id" ]
                  }
              , DOM.br {}
              , DOM.button
                  { onClick:
                      capture_ $ getRelativeHumidityButtonOnClickHandler stateHook
                  , children:
                      [ DOM.text "relative_humidity" ]
                  }
              , DOM.br {}
              , DOM.button
                  { onClick:
                      capture_ $ sensorIdButtonOnClickHandler stateHook "pressure"
                  , children:
                      [ DOM.text "pressureテーブルに入ってるsensor_id" ]
                  }
              , DOM.br {}
              , DOM.button
                  { onClick:
                      capture_ $ getPressureButtonOnClickHandler stateHook
                  , children:
                      [ DOM.text "pressure" ]
                  }
              , DOM.br {}
              , DOM.button
                  { onClick:
                      capture_ $ openFileHandler stateHook opfsDatabaseFileToUse
                  , children:
                      [ DOM.text "Open database file on OPFS"
                      ]
                  }
              , DOM.p_
                  [ DOM.text "データーベースファイルをアップロードする"
                  , DOM.input
                      { type: "file"
                      , onChange:
                          handler targetFiles loadFileHandler
                      }
                  ]
              , temperatureChart state.temperatureSeries
              , relativeHumidityChart state.relativeHumiditySeries
              , pressureChart state.pressureSeries
              ]
          }

-- OPFS上のデータベースファイルパス
opfsDatabaseFileToUse :: OpfsDatabaseFilePath
opfsDatabaseFileToUse = OpfsDatabaseFilePath "env_database.sqlite3"

versionButtonOnClickHandler :: Effect Unit
versionButtonOnClickHandler = Aff.runAff_ (either fail success) $ configGet
  where
  configGet :: Aff ConfigGetResult
  configGet = Sq3.configGet =<< Sq3.createWorker1Promiser

  fail :: Error -> Effect Unit
  fail = error <<< Aff.message

  success :: ConfigGetResult -> Effect Unit
  success r = logShow r

sensorIdButtonOnClickHandler :: StateHook -> String -> Effect Unit
sensorIdButtonOnClickHandler (state /\ _) table =
  Aff.runAff_ (either fail success)
    $ maybe (Aff.throwError $ Aff.error "database file not opened") (uncurry3 getSensorIdStoredInTable) do
        promiser <- state.promiser
        dbId <- state.dbId
        pure $ tuple3 promiser dbId table
  where
  fail :: Error -> Effect Unit
  fail = error <<< Aff.message

  success :: Array EnvSensorId -> Effect Unit
  success = traverse_ logShow

getTemperatureButtonOnClickHandler :: StateHook -> Effect Unit
getTemperatureButtonOnClickHandler (state /\ setState) =
  Aff.runAff_ (either fail success)
    $ maybe (Aff.throwError $ Aff.error "database file not opened") (uncurry3 go) do
        promiser <- state.promiser
        dbId <- state.dbId
        pure $ tuple3 promiser dbId "temperature"
  where
  fail :: Error -> Effect Unit
  fail = error <<< Aff.message

  -- 取得したデーターでstate変数を上書きする
  success :: Array TemperatureChartSeries -> Effect Unit
  success xs = setState _ { temperatureSeries = xs }

  mkRecord :: EnvSensorId -> Array DbRowTemperature -> TemperatureChartSeries
  mkRecord sensorId rowTemperature =
    let
      record x = { at: x.at, degc: (toNumber x.milli_degc) / 1000.0 }
    in
      sensorId /\ map record rowTemperature

  go :: SqliteWorker1Promiser -> DbId -> String -> Aff (Array TemperatureChartSeries)
  go promiser dbId table = do
    sensors <- getSensorIdStoredInTable promiser dbId table
    traverse (\s -> mkRecord s <$> getTemperature promiser dbId s) sensors

getRelativeHumidityButtonOnClickHandler :: StateHook -> Effect Unit
getRelativeHumidityButtonOnClickHandler (state /\ setState) =
  Aff.runAff_ (either fail success)
    $ maybe (Aff.throwError $ Aff.error "database file not opened") (uncurry3 go) do
        promiser <- state.promiser
        dbId <- state.dbId
        pure $ tuple3 promiser dbId "relative_humidity"
  where
  fail :: Error -> Effect Unit
  fail = error <<< Aff.message

  success :: Array RelativeHumidityChartSeries -> Effect Unit
  success xs = setState _ { relativeHumiditySeries = xs }

  mkRecord :: EnvSensorId -> Array DbRowRelativeHumidity -> RelativeHumidityChartSeries
  mkRecord sensorId rowRelativeHumidity =
    let
      record x = { at: x.at, percent: (toNumber x.ppm_rh) / 10000.0 }
    in
      sensorId /\ map record rowRelativeHumidity

  go :: SqliteWorker1Promiser -> DbId -> String -> Aff (Array RelativeHumidityChartSeries)
  go promiser dbId table = do
    sensors <- getSensorIdStoredInTable promiser dbId table
    traverse (\s -> mkRecord s <$> getRelativeHumidity promiser dbId s) sensors

getPressureButtonOnClickHandler :: StateHook -> Effect Unit
getPressureButtonOnClickHandler (state /\ setState) =
  Aff.runAff_ (either fail success)
    $ maybe (Aff.throwError $ Aff.error "database file not opened") (uncurry3 go) do
        promiser <- state.promiser
        dbId <- state.dbId
        pure $ tuple3 promiser dbId "pressure"
  where
  fail :: Error -> Effect Unit
  fail = error <<< Aff.message

  success :: Array PressureChartSeries -> Effect Unit
  success xs = setState _ { pressureSeries = xs }

  mkRecord :: EnvSensorId -> Array DbRowPressure -> PressureChartSeries
  mkRecord sensorId rowPressure =
    let
      record x = { at: x.at, hpa: (toNumber x.pascal) / 100.0 }
    in
      sensorId /\ map record rowPressure

  go :: SqliteWorker1Promiser -> DbId -> String -> Aff (Array PressureChartSeries)
  go promiser dbId table = do
    sensors <- getSensorIdStoredInTable promiser dbId table
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

-- データーベースを閉じる
closeDatabaseHandler :: StateHook -> Effect Unit
closeDatabaseHandler (state /\ setState) =
  maybe mempty (uncurry close) do
    promiser <- state.promiser
    dbId <- state.dbId
    pure (promiser /\ dbId)
  where
  close :: SqliteWorker1Promiser -> DbId -> Effect Unit
  close promiser dbId = Aff.runAff_ (either fail $ const success) $ Sq3.close promiser dbId

  fail :: Error -> Effect Unit
  fail = error <<< Aff.message

  success :: Effect Unit
  success = do
    logShow "database closed"
    setState _ { dbId = Nothing }

-- OPFS上のデーターベースファイルを開く
openFileHandler :: StateHook -> OpfsDatabaseFilePath -> Effect Unit
openFileHandler (state /\ setState) filepath = maybe mempty go state.promiser
  where
  go promiser = Aff.runAff_ (either fail success) $ Sq3.open promiser filepath

  fail :: Error -> Effect Unit
  fail = error <<< Aff.message

  success :: OpenResult -> Effect Unit
  success result = do
    logShow $ "database '" <> result.filename <> "' opened"
    setState _ { dbId = Just result.dbId }

-- テーブルに格納されているセンサーＩＤを取得する
getSensorIdStoredInTable :: SqliteWorker1Promiser -> DbId -> String -> Aff (Array EnvSensorId)
getSensorIdStoredInTable promiser dbId targetTable = do
  result <- Sq3.exec promiser dbId $ "SELECT DISTINCT `sensor_id` FROM `" <> targetTable <> "` ORDER BY `sensor_id`;"
  let
    decoded = map decodeJson_ result.resultRows
  traverse (either fail success) decoded
  where
  fail = Aff.throwError <<< Aff.error <<< printJsonDecodeError

  decodeJson_ :: Json -> Either JsonDecodeError { sensor_id :: EnvSensorId }
  decodeJson_ = decodeJson

  success r = pure r.sensor_id

-- 温度テーブルから温度を取得する
getTemperature :: SqliteWorker1Promiser -> DbId -> EnvSensorId -> Aff (Array DbRowTemperature)
getTemperature promiser dbId sensorId = do
  let
    query =
      "SELECT `at`,`milli_degc` FROM `temperature` WHERE `sensor_id`="
        <> BigInt.toString (unwrap sensorId)
        <> " ORDER BY `at` ASC LIMIT 10000;"
  void $ log query
  result <- Sq3.exec promiser dbId query
  let
    decoded = map decodeJson_ result.resultRows
  traverse (either fail pure) decoded
  where
  fail = Aff.throwError <<< Aff.error <<< printJsonDecodeError

  decodeJson_ :: Json -> Either JsonDecodeError DbRowTemperature
  decodeJson_ = decodeJson

-- 湿度テーブルから温度を取得する
getRelativeHumidity :: SqliteWorker1Promiser -> DbId -> EnvSensorId -> Aff (Array DbRowRelativeHumidity)
getRelativeHumidity promiser dbId sensorId = do
  let
    query =
      "SELECT `at`,`ppm_rh` FROM `relative_humidity` WHERE `sensor_id`="
        <> BigInt.toString (unwrap sensorId)
        <> " ORDER BY `at` ASC LIMIT 10000;"
  void $ log query
  result <- Sq3.exec promiser dbId query
  let
    decoded = map decodeJson_ result.resultRows
  traverse (either fail pure) decoded
  where
  fail = Aff.throwError <<< Aff.error <<< printJsonDecodeError

  decodeJson_ :: Json -> Either JsonDecodeError DbRowRelativeHumidity
  decodeJson_ = decodeJson

-- 気圧テーブルから温度を取得する
getPressure :: SqliteWorker1Promiser -> DbId -> EnvSensorId -> Aff (Array DbRowPressure)
getPressure promiser dbId sensorId = do
  let
    query =
      "SELECT `at`,`pascal` FROM `pressure` WHERE `sensor_id`="
        <> BigInt.toString (unwrap sensorId)
        <> " ORDER BY `at` ASC LIMIT 10000;"
  void $ log query
  result <- Sq3.exec promiser dbId query
  let
    decoded = map decodeJson_ result.resultRows
  traverse (either fail pure) decoded
  where
  fail = Aff.throwError <<< Aff.error <<< printJsonDecodeError

  decodeJson_ :: Json -> Either JsonDecodeError DbRowPressure
  decodeJson_ = decodeJson

-- OPFS上のデーターベースファイルにローカルファイルを上書きする
loadFileHandler :: Maybe FileList -> Effect Unit
loadFileHandler Nothing = mempty

loadFileHandler (Just filelist) = maybe (errorShow "FileList is empty") go $ FileList.item 0 filelist
  where
  go :: File -> Effect Unit
  go file = Aff.runAff_ (either fail $ const success) $ overwriteOpfsFile file

  overwriteOpfsFile :: File -> Aff Unit
  overwriteOpfsFile file = do
    ab <- readAsArrayBuffer (File.toBlob file)
    Sq3.overwriteOpfsFileWithSpecifiedArrayBuffer opfsDatabaseFileToUse ab

  fail :: Error -> Effect Unit
  fail = error <<< Aff.message

  success :: Effect Unit
  success = do
    log "OPFS file overwrited"
