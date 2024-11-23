module App.Card.Pressure
  ( pressureOverview
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
import DataAcquisition.Table.Pressure (Pressure)
import React.Basic.DOM as DOM
import React.Basic.Hooks (JSX)

pressureOverview :: (EnvSensorId /\ Array Pressure) -> JSX
pressureOverview (sensorId /\ pressures) =
  card
    { className: "col-span-1"
    , title: "気圧 hPa"
    , description: printEnvSensorId sensorId
    , children: maybe [] contents latest
    , footer:
        either show identity
          $ formatDateTime "YYYY-MM-DD HH:mm:ss UTC"
          =<< note "DateTime parse error" lastUpdatedAt
    }
  where
  latest :: Maybe Pressure
  latest = Array.last $ Array.sortWith (_.at) pressures

  lastUpdatedAt :: Maybe DateTime
  lastUpdatedAt = pure <<< D.toDateTime =<< D.instant <<< _.at =<< latest

contents :: Pressure -> Array JSX
contents pressure =
  [ DOM.p { className: "text-center text-5xl", children: [ DOM.text $ show pressure.hpa ] }
  ]
