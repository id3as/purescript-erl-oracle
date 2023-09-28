module Erl.Oracle.Types.VirtualNetworkInterface where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Erl.Data.List (List)
import Erl.Kernel.Inet (Ip4Address, Ip6Address)
import Erl.Oracle.Types.Common (AvailabilityDomainId, CompartmentId, DefinedTags, FreeformTags, SubnetId, VlanId, VnicId)
import Foreign (unsafeFromForeign)
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON (class ReadForeign)

data VnicLifecycleState
  = Provisioning
  | Available
  | Terminating
  | Terminated

derive instance Eq VnicLifecycleState
derive instance Generic VnicLifecycleState _
instance ReadForeign VnicLifecycleState where
  readImpl f =
    case unsafeFromForeign f of
      "PROVISIONING" -> pure Provisioning
      "AVAILABLE" -> pure Available
      "TERMINATING" -> pure Terminating
      "TERMINATED" -> pure Terminated
      somethingElse -> unsafeCrashWith $ "Unexpected VnicLifecycleState " <> somethingElse

type Vnic =
  { availabilityDomain :: AvailabilityDomainId
  , compartment :: CompartmentId
  , definedTags :: Maybe DefinedTags
  , displayName :: Maybe String
  , freeformTags :: Maybe FreeformTags
  , hostnameLabel :: Maybe String
  , id :: VnicId
  , ipv6Address :: Maybe Ip6Address
  , isPrimary :: Maybe Boolean
  , lifecycleState :: VnicLifecycleState
  , macAddress :: Maybe String
  , nsgIds :: Maybe (List String)
  , privateIp :: Maybe Ip4Address
  , publicIp :: Maybe Ip4Address
  , skipSourceDestCheck :: Maybe Boolean
  , subnetId :: Maybe SubnetId
  , timeCreated :: String
  , vlanId :: Maybe VlanId
  }