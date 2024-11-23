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
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), tuple3, uncurry3, (/\))
import DataAcquisition.DataAcquisition (OrderBy(..))
import DataAcquisition.EnvSensorId (EnvSensorId)
import DataAcquisition.Table.Pressure (Pressure)
import DataAcquisition.Table.Pressure as TP
import DataAcquisition.Table.RelativeHumidity (RelativeHumidity)
import DataAcquisition.Table.RelativeHumidity as TRH
import DataAcquisition.Table.Table (getSensorIdStoredInTable)
import DataAcquisition.Table.Temperature (Temperature)
import DataAcquisition.Table.Temperature as TT
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Exception (Error)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useEffect, useState)
import React.Basic.Hooks as React
import Result3 (Result3(..))
import Sqlite3Wasm.Sqlite3Wasm as Sq3

type ChartsProps
  = { config :: Config
    , sqliteDatabaseState :: SqliteDatabaseState
    }

type State
  = { temperatureSeriesResult :: Result3 String (Array (EnvSensorId /\ Array Temperature))
    , relativeHumiditySeriesResult :: Result3 String (Array (EnvSensorId /\ Array RelativeHumidity))
    , pressureSeriesResult :: Result3 String (Array (EnvSensorId /\ Array Pressure))
    }

type StateHook
  = (State /\ ((State -> State) -> Effect Unit))

initialState :: State
initialState =
  { temperatureSeriesResult: Initial
  , relativeHumiditySeriesResult: Initial
  , pressureSeriesResult: Initial
  }

mkCharts :: Component ChartsProps
mkCharts = do
  temperatureChart <- TemperatureChart.mkComponent
  relativeHumidityChart <- RelativeHumidityChart.mkComponent
  pressureChart <- PressureChart.mkComponent
  component "Charts" \(props :: ChartsProps) -> React.do
    -- useStateフックを使って、stateとsetStateを得る
    stateHook@(state /\ _setState) <- useState initialState
    useEffect props.sqliteDatabaseState.maybeDbId do
      if isJust props.sqliteDatabaseState.maybeDbId then do
        getTemperature stateHook props.sqliteDatabaseState
        getRelativeHumidity stateHook props.sqliteDatabaseState
        getPressure stateHook props.sqliteDatabaseState
      else
        mempty
      pure mempty
    --
    pure
      $ DOM.div
          { className: "container mx-auto p-4 min-h-appMainContents"
          , children:
              [ temperatureChart
                  $ case state.temperatureSeriesResult of
                      Initial -> []
                      Err _e -> []
                      Ok series -> series
              , relativeHumidityChart
                  $ case state.relativeHumiditySeriesResult of
                      Initial -> []
                      Err _e -> []
                      Ok series -> series
              , pressureChart
                  $ case state.pressureSeriesResult of
                      Initial -> []
                      Err _e -> []
                      Ok series -> series
              ]
          }

getTemperature :: StateHook -> SqliteDatabaseState -> Effect Unit
getTemperature (_state /\ setState) sqlite =
  Aff.runAff_ (either fail success)
    $ maybe (Aff.throwError $ Aff.error "データーベースが開かれていません。") (uncurry3 go) do
        promiser <- sqlite.maybePromiser
        dbId <- sqlite.maybeDbId
        pure $ tuple3 promiser dbId TT.tableName
  where
  fail :: Error -> Effect Unit
  fail err = setState _ { temperatureSeriesResult = Err $ Aff.message err }

  -- 取得したデーターでstate変数を上書きする
  success :: Array (EnvSensorId /\ Array Temperature) -> Effect Unit
  success xs = setState _ { temperatureSeriesResult = Ok xs }

  go :: Sq3.SqliteWorker1Promiser -> Sq3.DbId -> String -> Aff (Array (EnvSensorId /\ Array Temperature))
  go promiser dbId dbTable = do
    sensorIds <- getSensorIdStoredInTable promiser dbId dbTable
    traverse (\sensorId -> TT.getTemperature promiser dbId sensorId ASC Nothing) sensorIds

getRelativeHumidity :: StateHook -> SqliteDatabaseState -> Effect Unit
getRelativeHumidity (_state /\ setState) sqlite =
  Aff.runAff_ (either fail success)
    $ maybe (Aff.throwError $ Aff.error "データーベースが開かれていません。") (uncurry3 go) do
        promiser <- sqlite.maybePromiser
        dbId <- sqlite.maybeDbId
        pure $ tuple3 promiser dbId TRH.tableName
  where
  fail :: Error -> Effect Unit
  fail err = setState _ { relativeHumiditySeriesResult = Err $ Aff.message err }

  -- 取得したデーターでstate変数を上書きする
  success :: Array (EnvSensorId /\ Array RelativeHumidity) -> Effect Unit
  success xs = setState _ { relativeHumiditySeriesResult = Ok xs }

  go :: Sq3.SqliteWorker1Promiser -> Sq3.DbId -> String -> Aff (Array (EnvSensorId /\ Array RelativeHumidity))
  go promiser dbId dbTable = do
    sensorIds <- getSensorIdStoredInTable promiser dbId dbTable
    traverse (\sensorId -> TRH.getRelativeHumidity promiser dbId sensorId ASC Nothing) sensorIds

getPressure :: StateHook -> SqliteDatabaseState -> Effect Unit
getPressure (_state /\ setState) sqlite =
  Aff.runAff_ (either fail success)
    $ maybe (Aff.throwError $ Aff.error "データーベースが開かれていません。") (uncurry3 go) do
        promiser <- sqlite.maybePromiser
        dbId <- sqlite.maybeDbId
        pure $ tuple3 promiser dbId TP.tableName
  where
  fail :: Error -> Effect Unit
  fail err = setState _ { temperatureSeriesResult = Err $ Aff.message err }

  -- 取得したデーターでstate変数を上書きする
  success :: Array (EnvSensorId /\ Array Pressure) -> Effect Unit
  success xs = setState _ { pressureSeriesResult = Ok xs }

  go :: Sq3.SqliteWorker1Promiser -> Sq3.DbId -> String -> Aff (Array (EnvSensorId /\ Array Pressure))
  go promiser dbId dbTable = do
    sensorIds <- getSensorIdStoredInTable promiser dbId dbTable
    traverse (\sensorId -> TP.getPressure promiser dbId sensorId ASC Nothing) sensorIds
