module Erl.Oracle.Subnet
  ( CreateSubnetRequest
  , DeleteSubnetRequest
  , ListSubnetsRequest
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
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect (Effect)
import Erl.Data.List (List)
import Erl.Oracle.Shared (BaseRequest, ociCliBase, ociCliBase', runOciCli)
import Erl.Oracle.Types.Common (AvailabilityDomainId, CompartmentId, DefinedTags, DhcpOptionsId, FreeformTags, RouteTableId, SecurityListId, SubnetId, VcnId, OciProfile)
import Erl.Oracle.Types.Subnet (SubnetLifecycleState, SubnetDetails)
import Foreign (F, MultipleErrors)
import Simple.JSON (readJSON')

type ListSubnetsRequest = BaseRequest
  ( vcn :: Maybe VcnId
  , displayName :: Maybe String
  )

defaultListSubnetsRequest :: OciProfile -> Maybe CompartmentId -> ListSubnetsRequest
defaultListSubnetsRequest profile@{ defaultCompartment } compartment =
  { compartment: fromMaybe defaultCompartment compartment
  , vcn: Nothing
  , displayName: Nothing
  , profile
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

defaultCreateSubnetRequest :: OciProfile -> Maybe CompartmentId -> String -> VcnId -> CreateSubnetRequest
defaultCreateSubnetRequest profile@{ defaultCompartment } compartment cidrBlock vcnId =
  { cidrBlock
  , vcnId
  , displayName: Nothing
  , compartment: fromMaybe defaultCompartment compartment
  , profile
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

defaultDeleteSubnetRequest :: OciProfile -> Maybe CompartmentId -> SubnetId -> DeleteSubnetRequest
defaultDeleteSubnetRequest profile@{ defaultCompartment } compartment subnetId =
  { subnetId
  , compartment: fromMaybe defaultCompartment compartment
  , profile
  }

type DeleteSubnetResponse =
  { "data" :: List String
  }

fromTerminateSubnetResponse :: DeleteSubnetResponse -> F Boolean
fromTerminateSubnetResponse { "data": _resp } = pure true

deleteSubnet :: DeleteSubnetRequest -> Effect (Either MultipleErrors Boolean)
deleteSubnet req@{ subnetId } = do
  let
    cli = ociCliBase' req "network subnet delete --force " <>
      (" --subnet-id " <> unwrap subnetId)
  outputJson <- runOciCli cli
  pure $ runExcept $ fromTerminateSubnetResponse =<< readJSON' =<< outputJson