module App.Pages.Home
  ( Props
  , mkHome
  ) where

import Prelude
import App.Card.Card (card)
import App.Card.RelativeHumidity (relativeHumidityOverview)
import App.Card.SqliteWasm (sqliteWasmOverview)
import App.Card.Temperature (temperatureOverview)
import App.Card.Pressure (pressureOverview)
import App.Config (Config)
import App.SqliteDatabaseState (SqliteDatabaseState)
import Data.Either (either)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Traversable (traverse)
import Data.Tuple.Nested (tuple3, uncurry3)
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
import React.Basic.Hooks (type (/\), (/\), Component, JSX, component, useState, useEffect)
import React.Basic.Hooks as React
import Result3 (Result3(..))
import Sqlite3Wasm.Sqlite3Wasm as Sq3

type Props
  = { config :: Config
    , sqliteDatabaseState :: SqliteDatabaseState
    }

type State
  = { temperatureSeriesResult :: Result3 String (Array (EnvSensorId /\ Array Temperature))
    , relativeHumiditySeriesResult :: Result3 String (Array (EnvSensorId /\ Array RelativeHumidity))
    , pressureSeriesResult :: Result3 String (Array (EnvSensorId /\ Array Pressure))
    }

type SetState
  = (State -> State) -> Effect Unit

type StateHook
  = (State /\ SetState)

initialState :: State
initialState =
  { temperatureSeriesResult: Initial
  , relativeHumiditySeriesResult: Initial
  , pressureSeriesResult: Initial
  }

mkHome :: Component Props
mkHome = do
  sqliteWasmOverview <- sqliteWasmOverview
  component "Home" \(props :: Props) -> React.do
    stateHook@(state /\ _) <- useState initialState
    -- 副作用フック(useEffect)
    useEffect props.sqliteDatabaseState.maybeDbId do
      if isJust props.sqliteDatabaseState.maybeDbId then do
        getTemperatureTableSummary stateHook props.sqliteDatabaseState
        getRelativeHumidityTableSummary stateHook props.sqliteDatabaseState
        getPressureTableSummary stateHook props.sqliteDatabaseState
      else
        mempty
      pure mempty
    pure
      $ DOM.section
          { className: "p-4 grid grid-flow-row-dense grid-cols-4 gap-3"
          , children:
              [ tutorialCard props.sqliteDatabaseState
              ]
                <> case state.temperatureSeriesResult of
                    Initial -> []
                    Err _ -> []
                    Ok series -> map temperatureOverview series
                <> case state.relativeHumiditySeriesResult of
                    Initial -> []
                    Err _ -> []
                    Ok series -> map relativeHumidityOverview series
                <> case state.pressureSeriesResult of
                    Initial -> []
                    Err _ -> []
                    Ok series -> map pressureOverview series
                <> [ sqliteWasmOverview unit
                  ]
          }

tutorialCard :: SqliteDatabaseState -> JSX
tutorialCard sqlite =
  card
    { className: "col-span-4"
    , title: "Welcome"
    , description: "まいど！計測データ分析アプリです。"
    , children:
        [ DOM.text "初めて使うときは計測データーベースファイルをアップロードしてください。"
        , DOM.p
            { className: "mt-10 font-bold"
            , children:
                [ if isJust sqlite.maybeDbId then
                    DOM.text "計測データ分析を始めましょう。"
                  else
                    DOM.text "Open Databaseをクリックしてデーターベースをオープンしてください。"
                ]
            }
        ]
    , footer: ""
    }

-- 温度テーブルの要約を得る
getTemperatureTableSummary :: StateHook -> SqliteDatabaseState -> Effect Unit
getTemperatureTableSummary (_state /\ setState) sqlite =
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
    traverse (\sensorId -> TT.getTemperature promiser dbId sensorId DESC $ Just 3) sensorIds

-- 湿度テーブルの要約を得る
getRelativeHumidityTableSummary :: StateHook -> SqliteDatabaseState -> Effect Unit
getRelativeHumidityTableSummary (_state /\ setState) sqlite =
  Aff.runAff_ (either fail success)
    $ maybe (Aff.throwError $ Aff.error "データーベースが開かれていません。") (uncurry3 go) do
        promiser <- sqlite.maybePromiser
        dbId <- sqlite.maybeDbId
        pure $ tuple3 promiser dbId TRH.tableName
  where
  fail :: Error -> Effect Unit
  fail err = setState _ { temperatureSeriesResult = Err $ Aff.message err }

  -- 取得したデーターでstate変数を上書きする
  success :: Array (EnvSensorId /\ Array RelativeHumidity) -> Effect Unit
  success xs = setState _ { relativeHumiditySeriesResult = Ok xs }

  go :: Sq3.SqliteWorker1Promiser -> Sq3.DbId -> String -> Aff (Array (EnvSensorId /\ Array RelativeHumidity))
  go promiser dbId dbTable = do
    sensorIds <- getSensorIdStoredInTable promiser dbId dbTable
    traverse (\sensorId -> TRH.getRelativeHumidity promiser dbId sensorId DESC $ Just 3) sensorIds

-- 気圧テーブルの要約を得る
getPressureTableSummary :: StateHook -> SqliteDatabaseState -> Effect Unit
getPressureTableSummary (_state /\ setState) sqlite =
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
    traverse (\sensorId -> TP.getPressure promiser dbId sensorId DESC $ Just 3) sensorIds
