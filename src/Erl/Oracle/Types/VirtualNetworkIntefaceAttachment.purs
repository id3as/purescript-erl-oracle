module Erl.Oracle.Types.VirtualNetworkIntefaceAttachment where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Erl.Oracle.Types.Common (AvailabilityDomainId, CompartmentId, InstanceId, SubnetId, VlanId, VnicAttachmentId, VnicId)
import Foreign (unsafeFromForeign)
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON (class ReadForeign)

data VnicAttachmentLifecycleState
  = Attaching
  | Attached
  | Detaching
  | Detached

derive instance Eq VnicAttachmentLifecycleState
derive instance Generic VnicAttachmentLifecycleState _
instance ReadForeign VnicAttachmentLifecycleState where
  readImpl f =
    case unsafeFromForeign f of
      "ATTACHING" -> pure Attaching
      "ATTACHED" -> pure Attached
      "DETACHING" -> pure Detaching
      "DETACHED" -> pure Detached
      somethingElse -> unsafeCrashWith $ "Unexpected VnicAttachmentLifecycleState " <> somethingElse

type VnicAttachment =
  { availabilityDomain :: AvailabilityDomainId
  , compartment :: CompartmentId
  , displayName :: Maybe String
  , id :: VnicAttachmentId
  , instanceId :: InstanceId
  , lifecycleState :: VnicAttachmentLifecycleState
  , nicIndex :: Maybe Int
  , subnet :: Maybe SubnetId
  , timeCreated :: String
  , vlanId :: Maybe VlanId
  , vlanTag :: Maybe Int
  , vnicId :: Maybe VnicId
  }