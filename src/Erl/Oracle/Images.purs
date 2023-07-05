module Erl.Oracle.Images
  ( ImageDescription
  , ImageLifecycleState(..)
  , InstanceAgentFeatures
  , LaunchOptions
  , ListImagesRequest
  , defaultListImagesRequest
  , listImages
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Effect (Effect)
import Erl.Data.List (List)
import Erl.Oracle.Shared (BaseRequest, ociCliBase, runOciCli)
import Erl.Oracle.Types (CompartmentId(..), DefinedTags, ImageId(..), FreeformTags)
import Foreign (F, MultipleErrors, unsafeFromForeign)
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON (class ReadForeign, readJSON')

data ImageLifecycleState
  = Available
  | Deleted
  | Disabled
  | Exporting
  | Importing
  | Provisioning

derive instance Eq ImageLifecycleState
derive instance Generic ImageLifecycleState _
instance ReadForeign ImageLifecycleState where
  readImpl f =
    case unsafeFromForeign f of
      "DELETED" -> pure Deleted
      "PROVISIONING" -> pure Provisioning
      "IMPORTING" -> pure Importing
      "AVAILABLE" -> pure Available
      "EXPORTING" -> pure Exporting
      "DISABLED" -> pure Disabled
      somethingElse -> unsafeCrashWith $ "Unexpected LifecycleState " <> somethingElse

instance Show ImageLifecycleState where
  show = genericShow

type ListImagesRequest = BaseRequest
  ( compartment :: Maybe CompartmentId
  )

defaultListImagesRequest :: ListImagesRequest
defaultListImagesRequest =
  { compartment: Nothing
  }

type LaunchOptionsInt =
  { "boot-volume-type" :: Maybe String
  , "firmware" :: Maybe String
  , "is-consistent-volume-naming-enabled" :: Maybe Boolean
  , "is-pv-encryption-in-transit-enabled" :: Maybe Boolean
  , "network-type" :: Maybe String
  , "remote-data-volume-type" :: Maybe String
  }

type LaunchOptions =
  { bootVolumeType :: Maybe String
  , firmware :: Maybe String
  , isConsistentVolumeNamingEnabled :: Maybe Boolean
  , isPvEncryptionInTransitEnabled :: Maybe Boolean
  , networkType :: Maybe String
  , remoteDataVolumeType :: Maybe String
  }

type InstanceAgentFeaturesInt =
  { "is-management-supported" :: Maybe Boolean -- unused
  , "is-monitoring-supproted" :: Maybe Boolean -- unused
  }

type InstanceAgentFeatures =
  { isManagementSupported :: Maybe Boolean
  , isMonitoringSupported :: Maybe Boolean
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

type ImageDescription =
  { agentFeatures :: Maybe InstanceAgentFeatures
  , baseImageId :: Maybe String
  , billableSizeInGbs :: Maybe Int
  , compartmentId :: Maybe CompartmentId
  , createImageAllowed :: Boolean
  , definedTags :: Maybe DefinedTags
  , displayName :: Maybe String
  , freeformTags :: Maybe FreeformTags
  , id :: ImageId
  , launchMode :: Maybe String
  , launchOptions :: Maybe LaunchOptions
  , lifecycleState :: ImageLifecycleState
  , listingType :: Maybe String
  , operatingSystem :: String
  , operatingSystemVersion :: String
  , sizeInMbs :: Maybe Int
  , timeCreated :: String
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
      <> (fromMaybe "" $ (\r -> " --compartment-id " <> r) <$> unwrap <$> compartment)
  outputJson <- runOciCli cli
  pure $ runExcept $ fromImagesResponseInt =<< readJSON' =<< outputJson

