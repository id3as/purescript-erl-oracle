module Erl.Oracle.Subnet
  ( CreateSubnetRequest
  , DeleteSubnetRequest
  , ListSubnetsRequest
  , SubnetDetails
  , SubnetLifecycleState(..)
  , createSubnet
  , defaultCreateSubnetRequest
  , defaultDeleteSubnetRequest
  , defaultListSubnetsRequest
  , deleteSubnet
  , listSubnets
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect (Effect)
import Erl.Data.List (List)
import Erl.Oracle.Shared (BaseRequest, ociCliBase, runOciCli)
import Erl.Oracle.Types (AvailabilityDomainId, CompartmentId, DefinedTags, DhcpOptionsId, FreeformTags, RouteTableId, SecurityListId, SubnetId, VcnId)
import Foreign (F, MultipleErrors, unsafeFromForeign)
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON (class ReadForeign, readJSON')

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

type ListSubnetsRequest = BaseRequest
  ( compartment :: CompartmentId
  , vcn :: Maybe VcnId
  , displayName :: Maybe String
  )

defaultListSubnetsRequest :: CompartmentId -> ListSubnetsRequest
defaultListSubnetsRequest compartment =
  { compartment
  , vcn: Nothing
  , displayName: Nothing
  }

type ListSubnetResponse =
  { "data" :: List SubnetDetailsInt
  }

type SubnetDetailsInt =
  { "availability-domain" :: Maybe AvailabilityDomainId
  , "cidr-block" :: String
  , "compartment-id" :: CompartmentId
  , "defined-tags" :: Maybe DefinedTags
  , "dhcp-options-id" :: Maybe DhcpOptionsId
  , "display-name" :: Maybe String
  , "dns-label" :: Maybe String
  , "freeform-tags" :: Maybe FreeformTags
  , "id" :: SubnetId
  , "ipv6-cidr-block" :: Maybe String
  , "ipv6-cidr-blocks" :: Maybe (List String)
  , "ipv6-public-cidr-block" :: Maybe String
  , "ipv6-virtual-router-ip" :: Maybe String
  , "lifecycle-state" :: SubnetLifecycleState
  , "prohibit-public-ip-on-vnic" :: Maybe Boolean
  , "route-table-id" :: RouteTableId
  , "security-list-ids" :: Maybe (List SecurityListId)
  , "subnet-domain-name" :: Maybe String
  , "time-created" :: String
  , "vcn-id" :: VcnId
  , "virtual-router-ip" :: String
  , "virtual-router-mac" :: String
  }

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

fromSubnetDetailsInt :: SubnetDetailsInt -> F SubnetDetails
fromSubnetDetailsInt
  { "availability-domain": availabilityDomain
  , "cidr-block": cidrBlock
  , "compartment-id": compartment
  , "defined-tags": definedTags
  , "dhcp-options-id": dhcpOptions
  , "display-name": displayName
  , "dns-label": dnsLabel
  , "freeform-tags": freeformTags
  , "id": id
  , "ipv6-cidr-block": ipv6CidrBlock
  , "ipv6-cidr-blocks": ipv6CidrBlocks
  , "ipv6-public-cidr-block": ipv6PublicCidrBlock
  , "ipv6-virtual-router-ip": ipv6VirtualRouterIp
  , "lifecycle-state": lifecycleState
  , "prohibit-public-ip-on-vnic": prohibitPublicIpOnVnic
  , "route-table-id": routeTableId
  , "security-list-ids": securityListIds
  , "subnet-domain-name": subnetDomainName
  , "time-created": timeCreated
  , "vcn-id": vcnId
  , "virtual-router-ip": virtualRouterIp
  , "virtual-router-mac": virtualRouterMac
  } = do
  pure $
    { availabilityDomain
    , cidrBlock
    , compartment
    , definedTags
    , dhcpOptions
    , displayName
    , dnsLabel
    , freeformTags
    , id
    , ipv6CidrBlock
    , ipv6CidrBlocks
    , ipv6PublicCidrBlock
    , ipv6VirtualRouterIp
    , lifecycleState
    , prohibitPublicIpOnVnic
    , routeTableId
    , securityListIds
    , subnetDomainName
    , timeCreated
    , vcnId
    , virtualRouterIp
    , virtualRouterMac
    }

fromSubnetResponse :: ListSubnetResponse -> F (List SubnetDetails)
fromSubnetResponse { "data": entries } = ado
  instances <- traverse fromSubnetDetailsInt entries
  in instances

type CreateSubnetRequest = BaseRequest
  ( cidrBlock :: String
  , vcnId :: VcnId
  , displayName :: Maybe String
  )

defaultCreateSubnetRequest :: String -> VcnId -> CreateSubnetRequest
defaultCreateSubnetRequest cidrBlock vcnId =
  { cidrBlock
  , vcnId
  , displayName: Nothing
  }

type CreateSubnetResponseInt =
  { "data" :: SubnetDetailsInt
  }

fromCreateSubnetResponse :: CreateSubnetResponseInt -> F SubnetDetails
fromCreateSubnetResponse { "data": details } = ado
  resp <- fromSubnetDetailsInt details
  in resp

createSubnet :: CreateSubnetRequest -> Effect (Either MultipleErrors SubnetDetails)
createSubnet req@{ cidrBlock, vcnId, displayName } = do
  let
    cli = ociCliBase req $ " network subnet create"
      <> (" --cidr-block " <> cidrBlock)
      <> (" --vcn-id " <> unwrap vcnId)
      <> (fromMaybe "" $ (\r -> " --display-name '" <> r <> "'") <$> displayName)
  outputJson <- runOciCli cli
  pure $ runExcept $ fromCreateSubnetResponse =<< readJSON' =<< outputJson

listSubnets :: ListSubnetsRequest -> Effect (Either MultipleErrors (List SubnetDetails))
listSubnets req@{ compartment, displayName, vcn } = do
  let
    cli = ociCliBase req $ " network subnet list"
      <> (" --compartment-id " <> unwrap compartment)
      <> (fromMaybe "" $ (\r -> " --vcn-id " <> r) <$> unwrap <$> vcn)
      <> (fromMaybe "" $ (\r -> " --display-name '" <> r <> "'") <$> displayName)
  outputJson <- runOciCli cli
  pure $ runExcept $ fromSubnetResponse =<< readJSON' =<< outputJson

type DeleteSubnetRequest = BaseRequest
  ( subnetId :: SubnetId
  )

defaultDeleteSubnetRequest :: SubnetId -> DeleteSubnetRequest
defaultDeleteSubnetRequest subnetId =
  { subnetId
  }

type DeleteSubnetResponse =
  { "data" :: List String
  }

fromTerminateSubnetResponse :: DeleteSubnetResponse -> F Boolean
fromTerminateSubnetResponse { "data": _resp } = pure true

deleteSubnet :: DeleteSubnetRequest -> Effect (Either MultipleErrors Boolean)
deleteSubnet req@{ subnetId } = do
  let
    cli = ociCliBase req "network subnet delete --force " <>
      (" --subnet-id " <> unwrap subnetId)
  outputJson <- runOciCli cli
  pure $ runExcept $ fromTerminateSubnetResponse =<< readJSON' =<< outputJson