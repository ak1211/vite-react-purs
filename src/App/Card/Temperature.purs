module App.Card.Temperature
  ( temperatureOverview
  , module App.Card.Card
  ) where

import Prelude
import App.Card.Card (CardContents, card)
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.DateTime.Instant as D
import Data.Either (note, either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (Maybe, maybe)
import Data.Tuple.Nested (type (/\), (/\))
import DataAcquisition.EnvSensorId (EnvSensorId, printEnvSensorId)
import DataAcquisition.Table.Temperature (Temperature)
import React.Basic.DOM as DOM
import React.Basic.Hooks (JSX)

temperatureOverview :: (EnvSensorId /\ Array Temperature) -> JSX
temperatureOverview (sensorId /\ temperatures) =
  card
    { className: "col-span-1"
    , title: "気温 ℃"
    , description: printEnvSensorId sensorId
    , children: maybe [] contents latest
    , footer:
        either show identity
          $ formatDateTime "YYYY-MM-DD HH:mm:ss UTC"
          =<< note "DateTime parse error" lastUpdatedAt
    }
  where
  latest :: Maybe Temperature
  latest = Array.last $ Array.sortWith (_.at) temperatures

  lastUpdatedAt :: Maybe DateTime
  lastUpdatedAt = pure <<< D.toDateTime =<< D.instant <<< _.at =<< latest

contents :: Temperature -> Array JSX
contents t =
  [ DOM.p { className: "text-center text-5xl", children: [ DOM.text $ show t.degc ] }
  ]
