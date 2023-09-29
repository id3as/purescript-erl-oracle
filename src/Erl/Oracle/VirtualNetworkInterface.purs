module Erl.Oracle.VirtualNetworkInterface
  ( GetVnicRequest
  , defaultGetVnic
  , getVnic
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Effect (Effect)
import Erl.Data.List (List)
import Erl.Kernel.Inet (Ip4Address, Ip6Address)
import Erl.Oracle.Shared (BaseRequest, ociCliBase', runOciCli)
import Erl.Oracle.Types.Common (AvailabilityDomainId(..), CompartmentId(..), DefinedTags, OciProfile, SubnetId(..), VlanId(..), VnicId(..), FreeformTags)
import Erl.Oracle.Types.VirtualNetworkInterface (VnicLifecycleState, Vnic)
import Foreign (MultipleErrors, F)
import Simple.JSON (readJSON')

type GetVnicRequest = BaseRequest
  ( vnicId :: VnicId
  )

type GetVnicResponse =
  { "data" :: VnicInt
  }

type VnicInt =
  { "availability-domain" :: String
  , "compartment-id" :: String
  , "defined-tags" :: Maybe DefinedTags
  , "display-name" :: Maybe String
  , "freeform-tags" :: Maybe FreeformTags
  , "hostname-label" :: Maybe String
  , "id" :: String
  , "ipv6-address" :: Maybe Ip6Address
  , "is-primary" :: Maybe Boolean
  , "lifecycle-state" :: VnicLifecycleState
  , "mac-address" :: Maybe String
  , "nsg-ids" :: Maybe (List String)
  , "private-ip" :: Maybe Ip4Address
  , "public-ip" :: Maybe Ip4Address
  , "skip-source-dest-check" :: Maybe Boolean
  , "subnet-id" :: Maybe String
  , "time-created" :: String
  , "vlan-id" :: Maybe String
  }

fromVnicInt :: VnicInt -> F Vnic
fromVnicInt
  { "availability-domain": availabilityDomain
  , "compartment-id": compartment
  , "defined-tags": definedTags
  , "display-name": displayName
  , "freeform-tags": freeformTags
  , "hostname-label": hostnameLabel
  , "id": id
  , "ipv6-address": ipv6Address
  , "is-primary": isPrimary
  , "lifecycle-state": lifecycleState
  , "mac-address": macAddress
  , "nsg-ids": nsgIds
  , "private-ip": privateIp
  , "public-ip": publicIp
  , "skip-source-dest-check": skipSourceDestCheck
  , "subnet-id": subnetId
  , "time-created": timeCreated
  , "vlan-id": vlanId
  } = do
  pure $
    { availabilityDomain: AvailabilityDomainId availabilityDomain
    , compartment: CompartmentId compartment
    , definedTags
    , displayName
    , freeformTags
    , hostnameLabel
    , id: VnicId id
    , ipv6Address
    , isPrimary
    , lifecycleState
    , macAddress
    , nsgIds
    , privateIp
    , publicIp
    , skipSourceDestCheck
    , subnetId: maybe Nothing (\t -> Just $ SubnetId t) subnetId
    , timeCreated
    , vlanId: maybe Nothing (\t -> Just $ VlanId t) vlanId
    }

fromVnicResponse :: GetVnicResponse -> F Vnic
fromVnicResponse { "data": vnic } = fromVnicInt vnic

defaultGetVnic :: OciProfile -> Maybe CompartmentId -> VnicId -> GetVnicRequest
defaultGetVnic profile@{ defaultCompartment } compartment vnicId =
  { compartment: fromMaybe defaultCompartment compartment
  , profile
  , vnicId
  }

getVnic :: GetVnicRequest -> Effect (Either MultipleErrors Vnic)
getVnic req@{ vnicId } = do
  let
    cli = ociCliBase' req $ " network vnic get "
      <> (" --vnic-id " <> unwrap vnicId)
  outputJson <- runOciCli cli
  pure $ runExcept $ fromVnicResponse =<< readJSON' =<< outputJson