module DataAcquisition.EnvSensorId
  ( EnvSensorId(..)
  , printEnvSensorId
  ) where

import Prelude
import Data.Argonaut.Decode (JsonDecodeError(..))
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Either as Either
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
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
printEnvSensorId :: EnvSensorId -> String
printEnvSensorId (EnvSensorId bigint) = unsafePerformEffect (runEffectFn1 printEnvSensorIdImpl bigint)

-- BigIntはDecodeJsonのインスタンスでなかったので、BigIntを文字列で解釈してからBigIntに変換した。
-- unsafeCoerceを使っているからなんか問題があるかも
instance decodeJsonEnvSensorId :: DecodeJson EnvSensorId where
  decodeJson json = Either.note (UnexpectedValue json) maybeSensorId
    where
    maybeSensorId = EnvSensorId <$> (BigInt.fromString $ unsafeCoerce json)
