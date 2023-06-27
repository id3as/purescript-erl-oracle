module Erl.Oracle.CapactityReservationLifecycleState where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Debug (spy)
import Foreign (unsafeFromForeign)
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON (class ReadForeign)

data CapactityReservationLifecycleState
  = Active
  | Creating
  | Updating
  | Moving
  | Deleted
  | Deleting

derive instance Eq CapactityReservationLifecycleState
derive instance Generic CapactityReservationLifecycleState _
instance ReadForeign CapactityReservationLifecycleState where
  readImpl f =
    case spy "capacityReservationLifecycleState" $ unsafeFromForeign f of
      "ACTIVE" -> pure Active
      "CREATING" -> pure Creating
      "UPDATING" -> pure Updating
      "MOVING" -> pure Moving
      "DELETED" -> pure Deleted
      "DELETING" -> pure Deleting
      somethingElse -> unsafeCrashWith $ "Unexpected CapactityReservationLifecycleState " <> somethingElse

instance Show CapactityReservationLifecycleState where
  show = genericShow