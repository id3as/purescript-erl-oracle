module Erl.Oracle.Types.Subnet where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Erl.Data.List (List)
import Erl.Oracle.Types.Common (AvailabilityDomainId, CompartmentId, DefinedTags, DhcpOptionsId, FreeformTags, RouteTableId, SecurityListId, SubnetId, VcnId)
import Foreign (unsafeFromForeign)
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON (class ReadForeign)

data SubnetLifecycleState
  = Provisioning
  | Available
  | Terminating
  | Terminated

derive instance Eq SubnetLifecycleState
derive instance Generic SubnetLifecycleState _
instance ReadForeign SubnetLifecycleState where
  readImpl f =
    case unsafeFromForeign f of
      "AVAILABLE" -> pure Available
      "PROVISIONING" -> pure Provisioning
      "TERMINATING" -> pure Terminating
      "TERMINATED" -> pure Terminated
      somethingElse -> unsafeCrashWith $ "Unexpected InstanceLifecycleState " <> somethingElse

type SubnetDetails =
  { availabilityDomain :: Maybe AvailabilityDomainId
  , cidrBlock :: String
  , compartment :: CompartmentId
  , definedTags :: Maybe DefinedTags
  , dhcpOptions :: Maybe DhcpOptionsId
  , displayName :: Maybe String
  , dnsLabel :: Maybe String
  , freeformTags :: Maybe FreeformTags
  , id :: SubnetId
  , ipv6CidrBlock :: Maybe String
  , ipv6CidrBlocks :: Maybe (List String)
  , ipv6PublicCidrBlock :: Maybe String
  , ipv6VirtualRouterIp :: Maybe String
  , lifecycleState :: SubnetLifecycleState
  , prohibitPublicIpOnVnic :: Maybe Boolean
  , routeTableId :: RouteTableId
  , securityListIds :: Maybe (List SecurityListId)
  , subnetDomainName :: Maybe String
  , timeCreated :: String
  , vcnId :: VcnId
  , virtualRouterIp :: String
  , virtualRouterMac :: String
  }

