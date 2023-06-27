module Erl.Oracle.CompartmentTypes where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Foreign (unsafeFromForeign)
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON (class ReadForeign)

data CompartmentLifecycleState
  = Active
  | Creating
  | Inactive
  | Deleted
  | Deleting

derive instance Eq CompartmentLifecycleState
derive instance Generic CompartmentLifecycleState _
instance ReadForeign CompartmentLifecycleState where
  readImpl f =
    case unsafeFromForeign f of
      "ACTIVE" -> pure Active
      "CREATING" -> pure Creating
      "INACTIVE" -> pure Inactive
      "DELETED" -> pure Deleted
      "DELETING" -> pure Deleting
      somethingElse -> unsafeCrashWith $ "Unexpected CompartmentLifecycleState " <> somethingElse

instance Show CompartmentLifecycleState where
  show = genericShow