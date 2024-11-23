module App.TemperatureChart
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
import Data.Int (toStringAs, decimal)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Options (Options, (:=))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unique as Unique
import DataAcquisition.EnvSensorId (EnvSensorId, printEnvSensorId)
import DataAcquisition.Table.Temperature (Temperature)
import Effect (Effect)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useEffect, useLayoutEffect, useState)
import React.Basic.Hooks as React
import Shadcn.Components as Shadcn

type Props
  = Array (EnvSensorId /\ Array Temperature)

type State
  = { chartId :: Maybe String
    , apexchart :: Maybe Apexchart
    }

initialState :: State
initialState =
  { chartId: Nothing
  , apexchart: Nothing
  }

mkComponent :: Component Props
mkComponent = do
  component "Chart" \props -> React.do
    -- ステートフックを使って、stateとsetStateを得る
    (state /\ setState) <- useState initialState
    -- 副作用フック(useEffect)
    useEffect unit do
      hash <- Unique.hashUnique <$> Unique.newUnique
      let
        uniqueId = "chart-" <> toStringAs decimal hash
      setState _ { chartId = Just uniqueId }
      pure mempty
    useEffect state.chartId do
      maybe mempty (create props setState) state.chartId
      pure mempty
    -- アップデートは同期的にして欲しいのでuseLayoutEffectにした。
    useLayoutEffect props do
      maybe mempty (update props) state.apexchart
      pure mempty
    --
    pure
      $ Shadcn.card { className: "m-1 p-3 h-fit drop-shadow-md" }
      $ maybe [] (\chartId -> [ DOM.div { id: chartId } ]) state.chartId
  where
  create :: Props -> ((State -> State) -> Effect Unit) -> String -> Effect Unit
  create props setState chartId = do
    let
      option = chartOption props
    chart <- createChart ("#" <> chartId) option
    render chart
    setState _ { apexchart = Just chart }

  update :: Props -> Apexchart -> Effect Unit
  update props chart = updateOptions (chartOption props) chart

  dataset :: EnvSensorId /\ Array Temperature -> { sensorName :: String, timelines :: Array { at :: Number, degc :: Number } }
  dataset (envSensorId /\ values) =
    { sensorName: printEnvSensorId envSensorId
    , timelines: map (\v -> { at: unwrap v.at, degc: v.degc }) values
    }

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
                  := map (\val -> [ val.at, val.degc ]) ds.timelines
            )
            datasets
        <> S.stroke
        := (S.curve := Straight)
        <> L.legend
        := (L.showForSingleSeries := true)
        <> DL.dataLabels
        := (DL.enabled := false)
        <> TI.title
        := (TI.text := "M5Unit-ENV2で測定した温度 (℃)" <> TI.align := Left)
        <> X.xaxis
        := (X.type' := X.Datetime)
        <> X.xaxis
        := (X.type' := X.Datetime)
        <> TT.tooltip
        := TTX.x
        := (TTX.format := "yyyy-MM-dd hh:mm:ss")
