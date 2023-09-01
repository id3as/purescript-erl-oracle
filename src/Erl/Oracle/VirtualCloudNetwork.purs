module Erl.Oracle.VirtualCloudNetwork
  ( CreateVcnRequest
  , DeleteVcnRequest
  , ListVcnsRequest
  , createVcn
  , defaultCreateVcnRequest
  , defaultDeleteVcnRequest
  , defaultListVcnsRequest
  , deleteVcn
  , listVcns
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Debug (spy)
import Effect (Effect)
import Erl.Data.List (List, null)
import Erl.Oracle.Shared (BaseRequest, ociCliBase, runOciCli, escapeJson)
import Erl.Oracle.Types.Common (CompartmentId, DefinedTags, DhcpOptionsId, FreeformTags, RouteTableId, SecurityListId, VcnId)
import Erl.Oracle.Types.VirtualCloudNetwork (VcnLifecycleState, VcnDetails)
import Foreign (F, MultipleErrors)
import Simple.JSON (readJSON', writeJSON)

type ListVcnsRequest = BaseRequest
  ( compartment :: CompartmentId
  , lifecycleState :: Maybe VcnLifecycleState
  )

defaultListVcnsRequest :: CompartmentId -> ListVcnsRequest
defaultListVcnsRequest compartment =
  { compartment
  , lifecycleState: Nothing
  }

type ListVcnsResponse =
  { "data" :: List VcnDetailsInt
  }

type VcnDetailsInt =
  { "byiop6-cidr-blocks" :: Maybe (List String)
  , "cidr-block" :: Maybe String
  , "cidr-blocks" :: List String
  , "compartment-id" :: CompartmentId
  , "default-dhcp-options-id" :: Maybe DhcpOptionsId
  , "default-route-table-id" :: Maybe RouteTableId
  , "default-security-list-id" :: Maybe SecurityListId
  , "defined-tags" :: DefinedTags
  , "display-name" :: Maybe String
  , "dns-label" :: Maybe String
  , "freeform-tags" :: FreeformTags
  , "id" :: VcnId
  , "ipv6-cidr-block" :: Maybe String
  , "ipv6-private-cidr-blocks" :: Maybe (List String)
  , "ipv6-public-cidr-block" :: Maybe String
  , "lifecycle-state" :: VcnLifecycleState
  , "time-created" :: String
  , "vcn-domain-name" :: Maybe String
  }

fromVcnDetailsInt :: VcnDetailsInt -> F VcnDetails
fromVcnDetailsInt
  { "byiop6-cidr-blocks": byiop6CidrBlocks
  , "cidr-block": cidrBlock
  , "cidr-blocks": cidrBlocks
  , "compartment-id": compartmentId
  , "default-dhcp-options-id": defaultDhcpOptionsId
  , "default-route-table-id": defaultRouteTableId
  , "default-security-list-id": defaultSecurityListId
  , "defined-tags": definedTags
  , "display-name": displayName
  , "dns-label": dnsLabel
  , "freeform-tags": freeformTags
  , "id": id
  , "ipv6-cidr-block": ipv6CidrBlock
  , "ipv6-private-cidr-blocks": ipv6PrivateCidrBlocks
  , "ipv6-public-cidr-block": ipv6PublicCidrBlock
  , "lifecycle-state": lifecycleState
  , "time-created": timeCreated
  , "vcn-domain-name": vcnDomainName
  } = do
  pure $
    { byiop6CidrBlocks
    , cidrBlock
    , cidrBlocks
    , compartmentId
    , defaultDhcpOptionsId
    , defaultRouteTableId
    , defaultSecurityListId
    , definedTags
    , displayName
    , dnsLabel
    , freeformTags
    , id
    , ipv6CidrBlock
    , ipv6PrivateCidrBlocks
    , ipv6PublicCidrBlock
    , lifecycleState
    , timeCreated
    , vcnDomainName
    }

fromListVcnsResponse :: ListVcnsResponse -> F (List VcnDetails)
fromListVcnsResponse { "data": entries } = ado
  vcns <- traverse fromVcnDetailsInt entries
  in vcns

listVcns :: ListVcnsRequest -> Effect (Either MultipleErrors (List VcnDetails))
listVcns req@{ compartment } = do
  let
    cli = ociCliBase req $ " network vcn list"
      <> " --all "
      <> (" --compartment-id " <> unwrap compartment)
  outputJson <- runOciCli cli
  pure $ runExcept $ fromListVcnsResponse =<< readJSON' =<< outputJson

type CreateVcnResponseInt =
  { "data" :: VcnDetailsInt
  }

fromCreateVcnResponse :: CreateVcnResponseInt -> F VcnDetails
fromCreateVcnResponse { "data": details } = ado
  resp <- fromVcnDetailsInt details
  in resp

type CreateVcnRequest = BaseRequest
  ( compartment :: CompartmentId
  , cidrBlocks :: Maybe (List String)
  , definedTags :: Maybe DefinedTags
  , displayName :: Maybe String
  , dnsLabel :: Maybe String
  , freeformTags :: Maybe FreeformTags
  , isIpv6Enabled :: Maybe Boolean
  )

defaultCreateVcnRequest :: CompartmentId -> CreateVcnRequest
defaultCreateVcnRequest compartment =
  { compartment
  , cidrBlocks: Nothing
  , definedTags: Nothing
  , displayName: Nothing
  , dnsLabel: Nothing
  , freeformTags: Nothing
  , isIpv6Enabled: Nothing
  }

createVcn :: CreateVcnRequest -> Effect (Either MultipleErrors VcnDetails)
createVcn
  req@
    { compartment
    , cidrBlocks
    , definedTags
    , displayName
    , dnsLabel
    , freeformTags
    , isIpv6Enabled
    } = do

  let
    options =
      "network vcn create"
        <> (" --compartment-id " <> unwrap compartment)
        <> (fromMaybe "" $ (\r -> " --cidr-blocks \"" <> r <> "\"") <$> escapeJson <$> spy "json" writeJSON <$> cidrBlocks)
        <> (fromMaybe "" $ (\r -> " --defined-tags '" <> r <> "'") <$> writeJSON <$> definedTags)
        <> (fromMaybe "" $ (\r -> " --display-name '" <> r <> "'") <$> displayName)
        <> (fromMaybe "" $ (\r -> " --dns-label '" <> r <> "'") <$> dnsLabel)
        <> (fromMaybe "" $ (\r -> " --freeform-tags '" <> r <> "'") <$> writeJSON <$> freeformTags)
        <> (fromMaybe "" $ (\r -> " --is-ipv6-enabled " <> r) <$> show <$> isIpv6Enabled)

    cli = ociCliBase req options

  outputJson <- runOciCli cli
  pure $ runExcept $ fromCreateVcnResponse =<< readJSON' =<< outputJson

type DeleteVcnRequest = BaseRequest
  ( vcnId :: VcnId
  )

defaultDeleteVcnRequest :: VcnId -> DeleteVcnRequest
defaultDeleteVcnRequest vcn =
  { vcnId: vcn
  }

type EmptyResponse =
  { "data" :: List String
  }

fromEmptyResponse :: EmptyResponse -> F { success :: Boolean }
fromEmptyResponse { "data": d } = ado
  success <- pure $ null d
  in { success }

deleteVcn :: DeleteVcnRequest -> Effect (Either MultipleErrors { success :: Boolean })
deleteVcn
  req@
    { vcnId
    } = do

  let
    options =
      "network vcn delete"
        <> " --force "
        <> (" --vcn-id " <> unwrap vcnId)

    cli = ociCliBase req options

  outputJson <- runOciCli cli
  pure $ runExcept $ fromEmptyResponse =<< readJSON' =<< outputJson

