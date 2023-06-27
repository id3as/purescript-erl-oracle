module Erl.Oracle.ImageTypes where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Foreign (unsafeFromForeign)
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON (class ReadForeign, class WriteForeign)

data ImageLifecycleState
  = Available
  | Deleted
  | Disabled
  | Exporting
  | Importing
  | Provisioning

derive instance Eq ImageLifecycleState
derive instance Generic ImageLifecycleState _
instance ReadForeign ImageLifecycleState where
  readImpl f =
    case unsafeFromForeign f of
      "DELETED" -> pure Deleted
      "PROVISIONING" -> pure Provisioning
      "IMPORTING" -> pure Importing
      "AVAILABLE" -> pure Available
      "EXPORTING" -> pure Exporting
      "DISABLED" -> pure Disabled
      somethingElse -> unsafeCrashWith $ "Unexpected LifecycleState " <> somethingElse

instance Show ImageLifecycleState where
  show = genericShow