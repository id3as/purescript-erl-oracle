module Erl.Oracle.Types.VirtualCloudNetwork
  ( VcnDetails
  , VcnLifecycleState(..)
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Erl.Data.List (List)
import Erl.Oracle.Types.Common (CompartmentId, DefinedTags, DhcpOptionsId, FreeformTags, RouteTableId, SecurityListId, VcnId)
import Foreign (unsafeFromForeign)
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON (class ReadForeign)

data VcnLifecycleState
  = Provisioning
  | Available
  | Terminating
  | Terminated
  | Updating

derive instance Eq VcnLifecycleState
derive instance Generic VcnLifecycleState _
instance ReadForeign VcnLifecycleState where
  readImpl f =
    case unsafeFromForeign f of
      "AVAILABLE" -> pure Available
      "PROVISIONING" -> pure Provisioning
      "TERMINATING" -> pure Terminating
      "TERMINATED" -> pure Terminated
      "UPDATING" -> pure Updating
      somethingElse -> unsafeCrashWith $ "Unexpected VcnLifecycleState " <> somethingElse

type VcnDetails =
  { byiop6CidrBlocks :: Maybe (List String)
  , cidrBlock :: Maybe String
  , cidrBlocks :: List String
  , compartmentId :: CompartmentId
  , defaultDhcpOptionsId :: Maybe DhcpOptionsId
  , defaultRouteTableId :: Maybe RouteTableId
  , defaultSecurityListId :: Maybe SecurityListId
  , definedTags :: DefinedTags
  , displayName :: Maybe String
  , dnsLabel :: Maybe String
  , freeformTags :: FreeformTags
  , id :: VcnId
  , ipv6CidrBlock :: Maybe String
  , ipv6PrivateCidrBlocks :: Maybe (List String)
  , ipv6PublicCidrBlock :: Maybe String
  , lifecycleState :: VcnLifecycleState
  , timeCreated :: String
  , vcnDomainName :: Maybe String
  }
