module Erl.Oracle.InstanceLifecycleTypes where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Erl.Json (genericEnumWriteForeignImpl)
import Foreign (unsafeFromForeign)
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON (class ReadForeign, class WriteForeign, write)

data InstanceLifecycleState
  = Moving
  | Provisioning
  | Running
  | Starting
  | Stopping
  | Stopped
  | CreatingImage
  | Terminating
  | Terminated

derive instance Eq InstanceLifecycleState
derive instance Generic InstanceLifecycleState _
instance ReadForeign InstanceLifecycleState where
  readImpl f =
    case unsafeFromForeign f of
      "MOVING" -> pure Moving
      "PROVISIONING" -> pure Provisioning
      "RUNNING" -> pure Running
      "STARTING" -> pure Starting
      "STOPPING" -> pure Stopping
      "STOPPED" -> pure Stopped
      "CREATING_IMAGE" -> pure CreatingImage
      "TERMINATING" -> pure Terminating
      "TERMINATED" -> pure Terminated
      somethingElse -> unsafeCrashWith $ "Unexpected InstanceLifecycleState " <> somethingElse

instance WriteForeign InstanceLifecycleState where
  writeImpl f =
    case f of
      Moving -> write "MOVING"
      Provisioning -> write "PROVISIONING"
      Running -> write "RUNNING"
      Starting -> write "STARTING"
      Stopping -> write "STOPPING"
      Stopped -> write "STOPPED"
      CreatingImage -> write "CREATING_IMAGE"
      Terminating -> write "TERMINATING"
      Terminated -> write "TERMINATED"

instance Show InstanceLifecycleState where
  show = genericShow