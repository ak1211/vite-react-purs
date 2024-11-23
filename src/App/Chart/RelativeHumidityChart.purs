module App.RelativeHumidityChart
  ( Props
  , mkComponent
  ) where

import Prelude
import Apexcharts (Apexchart, Apexoptions, createChart, render, updateOptions)
import Apexcharts.Chart as C
import Apexcharts.Chart.Zoom as Z
import Apexcharts.Common as CC
import Apexcharts.DataLabels as DL
import Apexcharts.Legend as L
import Apexcharts.Series as SE
import Apexcharts.Stroke (Curve(..))
import Apexcharts.Stroke as S
import Apexcharts.Title (Align(..))
import Apexcharts.Title as TI
import Apexcharts.Tooltip as TT
import Apexcharts.Tooltip.X as TTX
import Apexcharts.Xaxis as X
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Options (Options, (:=))
import Data.Tuple.Nested (type (/\))
import DataAcquisition.EnvSensorId (EnvSensorId, printEnvSensorId)
import DataAcquisition.Table.RelativeHumidity (RelativeHumidity)
import Effect (Effect)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useEffect, useLayoutEffect, useState, (/\))
import React.Basic.Hooks as React

type Props
  = Array (EnvSensorId /\ Array RelativeHumidity)

type State
  = { apexchart :: Maybe Apexchart
    }

initialState :: State
initialState =
  { apexchart: Nothing
  }

mkComponent :: Component Props
mkComponent = do
  component "Chart" \props -> React.do
    -- ステートフックを使って、stateとsetStateを得る
    (state /\ setState) <- useState initialState
    -- 副作用フック(useEffect)
    useEffect unit do
      let
        option = chartOption props
      chart <- createChart "#relative-humidity-chart" option
      render chart
      setState (\_ -> { apexchart: Just chart })
      pure mempty
    -- アップデートは同期的にして欲しいのでuseLayoutEffectにした。
    useLayoutEffect props do
      maybe mempty (update props) state.apexchart
      pure mempty
    --
    pure $ DOM.div { id: "relative-humidity-chart" }
  where
  update :: Props -> Apexchart -> Effect Unit
  update props chart = updateOptions (chartOption props) chart

  dataset :: EnvSensorId /\ Array RelativeHumidity -> { sensorName :: String, timelines :: Array { at :: Number, percent :: Number } }
  dataset (sensor_id /\ values) =
    let
      name = printEnvSensorId sensor_id

      timelines = map (\v -> { at: unwrap v.at, percent: v.percent }) values
    in
      { sensorName: name, timelines: timelines }

  chartOption :: Props -> Options Apexoptions
  chartOption props =
    let
      datasets = map dataset props
    in
      C.chart := (C.type' := CC.Line <> C.height := 300.0 <> Z.zoom := (Z.enabled := true))
        <> SE.series
        := map
            ( \ds ->
                SE.name := ds.sensorName
                  <> SE.data'
                  := map (\val -> [ val.at, val.percent ]) ds.timelines
            )
            datasets
        <> S.stroke
        := (S.curve := Straight)
        <> L.legend
        := (L.showForSingleSeries := true)
        <> DL.dataLabels
        := (DL.enabled := false)
        <> TI.title
        := (TI.text := "M5Unit-ENV2で測定した相対湿度 (%RH)" <> TI.align := Left)
        <> X.xaxis
        := (X.type' := X.Datetime)
        <> X.xaxis
        := (X.type' := X.Datetime)
        <> TT.tooltip
        := TTX.x
        := (TTX.format := "yyyy-MM-dd hh:mm:ss")
