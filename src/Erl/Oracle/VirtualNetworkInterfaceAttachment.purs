module Erl.Oracle.VirtualNetworkInterfaceAttachment
  ( ListVnicAttachmentsRequest
  , defaultListVnicAttachments
  , listVnicAttachments
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect (Effect)
import Erl.Data.List (List)
import Erl.Oracle.Shared (BaseRequest, ociCliBase, runOciCli)
import Erl.Oracle.Types.Common (AvailabilityDomainId(..), CompartmentId(..), InstanceId(..), SubnetId(..), VlanId(..), VnicAttachmentId(..), VnicId(..), OciProfile)
import Erl.Oracle.Types.VirtualNetworkInterfaceAttachment (VnicAttachmentLifecycleState, VnicAttachment)
import Foreign (MultipleErrors, F)
import Simple.JSON (readJSON')

type ListVnicAttachmentsRequest = BaseRequest
  ( instanceId :: Maybe InstanceId
  )

type ListVnicAttachmentsResponse =
  { "data" :: List VnicAttachmentInt
  }

type VnicAttachmentInt =
  { "availability-domain" :: String
  , "compartment-id" :: String
  , "display-name" :: Maybe String
  , "id" :: String
  , "instance-id" :: String
  , "lifecycle-state" :: VnicAttachmentLifecycleState
  , "nic-index" :: Maybe Int
  , "subnet-id" :: Maybe String
  , "time-created" :: String
  , "vlan-id" :: Maybe String
  , "vlan-tag" :: Maybe Int
  , "vnic-id" :: Maybe String
  }

fromVnicAttachmentInt :: VnicAttachmentInt -> F VnicAttachment
fromVnicAttachmentInt
  { "availability-domain": availabilityDomain
  , "compartment-id": compartment
  , "display-name": displayName
  , "id": id
  , "instance-id": instanceId
  , "lifecycle-state": lifecycleState
  , "nic-index": nicIndex
  , "subnet-id": subnetId
  , "time-created": timeCreated
  , "vlan-id": vlanId
  , "vlan-tag": vlanTag
  , "vnic-id": vnicId
  } = do
  pure $
    { availabilityDomain: AvailabilityDomainId availabilityDomain
    , compartment: CompartmentId compartment
    , displayName
    , id: VnicAttachmentId id
    , instanceId: InstanceId instanceId
    , lifecycleState
    , nicIndex
    , subnet: maybe Nothing (\t -> Just $ SubnetId t) subnetId
    , timeCreated
    , vlanId: maybe Nothing (\t -> Just $ VlanId t) vlanId
    , vlanTag
    , vnicId: maybe Nothing (\t -> Just $ VnicId t) vnicId
    }

fromVnicAttachmentResponse :: ListVnicAttachmentsResponse -> F (List VnicAttachment)
fromVnicAttachmentResponse { "data": entries } = ado
  instances <- traverse fromVnicAttachmentInt entries
  in instances

defaultListVnicAttachments :: OciProfile -> Maybe CompartmentId -> ListVnicAttachmentsRequest
defaultListVnicAttachments profile@{ defaultCompartment } compartment =
  { compartment: fromMaybe defaultCompartment compartment
  , profile
  , instanceId: Nothing
  }

listVnicAttachments :: ListVnicAttachmentsRequest -> Effect (Either MultipleErrors (List VnicAttachment))
listVnicAttachments req@{ instanceId } = do
  let
    cli = ociCliBase req $ " compute vnic-attachment list "
      <> (fromMaybe "" $ (\r -> " --instance-id '" <> r <> "'") <$> unwrap <$> instanceId)
  outputJson <- runOciCli cli
  pure $ runExcept $ fromVnicAttachmentResponse =<< readJSON' =<< outputJson