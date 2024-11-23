module DataAcquisition.DataAcquisition
  ( ChartSeries
  , OrderBy(..)
  , toString
  ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import DataAcquisition.EnvSensorId (EnvSensorId)

type ChartSeries a
  = Tuple EnvSensorId (Array a)

data OrderBy
  = ASC
  | DESC

derive instance genericOrderBy :: Generic OrderBy _

derive instance eqOrderBy :: Eq OrderBy

instance showOrderBy :: Show OrderBy where
  show = genericShow

toString :: OrderBy -> String
toString ASC = "ASC"

toString DESC = "DESC"
