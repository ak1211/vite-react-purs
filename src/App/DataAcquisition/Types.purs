module App.DataAcquisition.Types
  ( ChartSeries
  , EnvSensorId(..)
  , PressureChartSeries
  , RelativeHumidityChartSeries
  , TemperatureChartSeries
  , printEnvSensorId
  ) where

import Prelude
import Data.Argonaut.Decode (JsonDecodeError(..))
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Either as Either
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Unsafe.Coerce (unsafeCoerce)

newtype EnvSensorId
  = EnvSensorId BigInt

derive instance genericEnvSensorId :: Generic EnvSensorId _

derive instance newtypeEnvSensorId :: Newtype EnvSensorId _

derive instance eqEnvSensorId :: Eq EnvSensorId

derive instance ordEnvSensorId :: Ord EnvSensorId

instance showEnvSensorId :: Show EnvSensorId where
  show = genericShow

foreign import printEnvSensorIdImpl :: EffectFn1 BigInt String

-- センサーＩＤを文字列に変換する
printEnvSensorId :: EnvSensorId -> Effect String
printEnvSensorId (EnvSensorId bigint) = runEffectFn1 printEnvSensorIdImpl bigint

-- BigIntはDecodeJsonのインスタンスでなかったので、BigIntを文字列で解釈してからBigIntに変換した。
-- unsafeCoerceを使っているからなんか問題があるかも
instance decodeJsonEnvSensorId :: DecodeJson EnvSensorId where
  decodeJson json = Either.note (UnexpectedValue json) maybeSensorId
    where
    maybeSensorId = EnvSensorId <$> (BigInt.fromString $ unsafeCoerce json)

type ChartSeries a
  = Tuple EnvSensorId (Array a)

type TemperatureChartSeries
  = ChartSeries { at :: Int, degc :: Number }

type RelativeHumidityChartSeries
  = ChartSeries { at :: Int, percent :: Number }

type PressureChartSeries
  = ChartSeries { at :: Int, hpa :: Number }
