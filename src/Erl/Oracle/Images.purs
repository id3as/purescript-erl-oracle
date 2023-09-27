module Erl.Oracle.Images
  ( ListImagesRequest
  , defaultListImagesRequest
  , listImages
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
import Erl.Oracle.Types.Common (CompartmentId(..), DefinedTags, ImageId(..), FreeformTags, OciProfile)
import Erl.Oracle.Types.Images (ImageLifecycleState, InstanceAgentFeatures, LaunchOptions, ImageDescription)
import Foreign (F, MultipleErrors)
import Simple.JSON (readJSON')

type ListImagesRequest = BaseRequest ()

defaultListImagesRequest :: OciProfile -> Maybe CompartmentId -> ListImagesRequest
defaultListImagesRequest profile@{ defaultCompartment } compartment =
  { compartment: fromMaybe defaultCompartment compartment
  , profile
  }

type LaunchOptionsInt =
  { "boot-volume-type" :: Maybe String
  , "firmware" :: Maybe String
  , "is-consistent-volume-naming-enabled" :: Maybe Boolean
  , "is-pv-encryption-in-transit-enabled" :: Maybe Boolean
  , "network-type" :: Maybe String
  , "remote-data-volume-type" :: Maybe String
  }

type InstanceAgentFeaturesInt =
  { "is-management-supported" :: Maybe Boolean -- unused
  , "is-monitoring-supproted" :: Maybe Boolean -- unused
  }

type ImageDescriptionInt =
  { "agent-features" :: Maybe InstanceAgentFeaturesInt
  , "base-image-id" :: Maybe String
  , "billable-size-in-gbs" :: Maybe Int
  , "compartment-id" :: Maybe String
  , "create-image-allowed" :: Boolean
  , "defined-tags" :: Maybe DefinedTags
  , "display-name" :: Maybe String
  , "freeform-tags" :: Maybe FreeformTags
  , "id" :: String
  , "launch-mode" :: Maybe String
  , "launch-options" :: Maybe LaunchOptionsInt
  , "lifecycle-state" :: ImageLifecycleState
  , "listing-type" :: Maybe String
  , "operating-system" :: String
  , "operating-system-version" :: String
  , "size-in-mbs" :: Maybe Int
  , "time-created" :: String
  }

fromInstanceAgentFeaturesInt :: Maybe InstanceAgentFeaturesInt -> F (Maybe InstanceAgentFeatures)
fromInstanceAgentFeaturesInt features =
  case features of
    Just
      { "is-management-supported": isManagementSupported
      , "is-monitoring-supproted": isMonitoringSupported
      } -> do
      pure $ Just { isManagementSupported, isMonitoringSupported }
    Nothing -> pure Nothing

fromLaunchOptionsInt :: Maybe LaunchOptionsInt -> F (Maybe LaunchOptions)
fromLaunchOptionsInt opts =
  case opts of
    Just
      { "boot-volume-type": bootVolumeType
      , "firmware": firmware
      , "is-consistent-volume-naming-enabled": isConsistentVolumeNamingEnabled
      , "is-pv-encryption-in-transit-enabled": isPvEncryptionInTransitEnabled
      , "network-type": networkType
      , "remote-data-volume-type": remoteDataVolumeType
      } -> do
      pure $ Just
        { bootVolumeType
        , firmware
        , isConsistentVolumeNamingEnabled
        , isPvEncryptionInTransitEnabled
        , networkType
        , remoteDataVolumeType
        }
    Nothing -> pure Nothing

fromImageDescriptionInt :: ImageDescriptionInt -> F ImageDescription
fromImageDescriptionInt
  { "agent-features": agentFeaturesInt
  , "base-image-id": baseImageId
  , "billable-size-in-gbs": billableSizeInGbs
  , "compartment-id": compartmentId
  , "create-image-allowed": createImageAllowed
  , "defined-tags": definedTags
  , "display-name": displayName
  , "freeform-tags": freeformTags
  , "id": id
  , "launch-mode": launchMode
  , "launch-options": launchOptionsInt
  , "lifecycle-state": lifecycleState
  , "listing-type": listingType
  , "operating-system": operatingSystem
  , "operating-system-version": operatingSystemVersion
  , "size-in-mbs": sizeInMbs
  , "time-created": timeCreated
  } = ado
  agentFeatures <- fromInstanceAgentFeaturesInt agentFeaturesInt
  launchOptions <- fromLaunchOptionsInt launchOptionsInt
  in
    { agentFeatures
    , baseImageId
    , billableSizeInGbs
    , compartmentId: maybe Nothing (\c -> Just $ CompartmentId c) compartmentId
    , createImageAllowed
    , definedTags
    , displayName
    , freeformTags
    , id: ImageId id
    , launchMode
    , launchOptions
    , lifecycleState
    , listingType
    , operatingSystem
    , operatingSystemVersion
    , sizeInMbs
    , timeCreated
    }

type ImageDescriptionsInt =
  { "data" :: List ImageDescriptionInt
  }

fromImagesResponseInt :: ImageDescriptionsInt -> F (List ImageDescription)
fromImagesResponseInt { "data": shapeData } = ado
  shapes <- traverse fromImageDescriptionInt shapeData
  in shapes

listImages :: ListImagesRequest -> Effect (Either MultipleErrors (List ImageDescription))
listImages req@{ compartment } = do
  let
    cli = ociCliBase req "compute image list "
      <> " --all "
  outputJson <- runOciCli cli
  pure $ runExcept $ fromImagesResponseInt =<< readJSON' =<< outputJson

