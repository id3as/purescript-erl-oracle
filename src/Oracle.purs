module Erl.Oracle
  ( CompartmentId(..)
  , ImageId(..)
  , Shape(..)
  , listCompartments
  , listImages
  , listShapes
  ) where

import Prelude

import Control.Monad.Except (except, runExcept)
import Data.Bifunctor (bimap)
import Data.DateTime (DateTime)
import Data.DateTime.Parsing (parseFullDateTime, toUTC)
import Data.Either (Either(..), note)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Int (floor)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Debug (spy, traceM)
import Effect (Effect)
import Erl.Data.List (List)
import Erl.Data.List as List
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Json (genericTaggedReadForeign, genericTaggedWriteForeign, genericEnumReadForeign, genericEnumWriteForeign)
import Erl.Kernel.Inet (Hostname, IpAddress, parseIpAddress)
import Foreign (F, ForeignError(..), MultipleErrors, readString, unsafeFromForeign)
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON (class ReadForeign, class WriteForeign, class WriteForeignKey, E, read', readJSON', writeImpl, writeJSON)
import Text.Parsing.Parser (ParserT, fail, parseErrorMessage, runParser)
import Unsafe.Coerce (unsafeCoerce)

newtype CompartmentId = CompartmentId String

derive newtype instance Eq CompartmentId
derive newtype instance Ord CompartmentId
derive newtype instance ReadForeign CompartmentId
derive newtype instance WriteForeign CompartmentId
derive newtype instance WriteForeignKey CompartmentId
derive instance Newtype CompartmentId _
derive instance Generic CompartmentId _
instance Show CompartmentId where
  show = genericShow

newtype Shape = Shape String

derive newtype instance Eq Shape
derive newtype instance Ord Shape
derive newtype instance ReadForeign Shape
derive newtype instance WriteForeign Shape
derive newtype instance WriteForeignKey Shape
derive instance Newtype Shape _
derive instance Generic Shape _
instance Show Shape where
  show = genericShow

newtype ImageId = ImageId String

derive newtype instance Eq ImageId
derive newtype instance Ord ImageId
derive newtype instance ReadForeign ImageId
derive newtype instance WriteForeign ImageId
derive newtype instance WriteForeignKey ImageId
derive instance Newtype ImageId _
derive instance Generic ImageId _
instance Show ImageId where
  show = genericShow

type BaseRequest a =
  { compartment :: Maybe CompartmentId
  | a
  }

type ListImageShapeCompatibilityEntries = BaseRequest
  ( imageId :: ImageId
  )

type ListImagesRequest = BaseRequest ()

type InstanceAgentFeaturesInt =
  { "is-management-supported" :: Maybe Boolean -- unused
  , "is-monitoring-supproted" :: Maybe Boolean -- unused
  }

type InstanceAgentFeatures =
  { isManagementSupported :: Maybe Boolean
  , isMonitoringSupported :: Maybe Boolean
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

type ImageDescriptionInt =
  { "agent-features" :: Maybe InstanceAgentFeaturesInt
  , "base-image-id" :: Maybe String
  , "billable-size-in-gbs" :: Maybe Int
  , "compartment-id" :: Maybe String
  , "create-image-allowed" :: Boolean
  , "defined-tags" :: Maybe (Map String (Map String String))
  , "display-name" :: Maybe String
  , "freeform-tags" :: Maybe (Map String String)
  , "id" :: String
  , "launch-mode" :: Maybe String
  , "launch-options" :: Maybe LaunchOptionsInt
  , "lifecycle-state" :: String
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
  , definedTags :: Maybe (Map String (Map String String))
  , displayName :: Maybe String
  , freeformTags :: Maybe (Map String String)
  , id :: ImageId
  , launchMode :: Maybe String
  , launchOptions :: Maybe LaunchOptions
  , lifecycleState :: String
  , listingType :: Maybe String
  , operatingSystem :: String
  , operatingSystemVersion :: String
  , sizeInMbs :: Maybe Int
  , timeCreated :: String

  }

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
listImages req = do
  let
    cli = ociCliBase req "compute image list"
  outputJson <- runOciCli cli
  pure $ runExcept $ fromImagesResponseInt =<< readJSON' =<< outputJson

type ListShapesRequest = BaseRequest ()

type MaxVnicAttachmentOptionsInt =
  { "default-per-ocpu" :: Maybe Number
  , "max" :: Maybe Number
  , "min" :: Maybe Int
  }

type MaxVnicAttachmentOptions =
  { defaultPerOcpu :: Maybe Number
  , max :: Maybe Number
  , min :: Maybe Int
  }

fromMaxVnicAttachmentOptionsInt :: Maybe MaxVnicAttachmentOptionsInt -> F (Maybe MaxVnicAttachmentOptions)
fromMaxVnicAttachmentOptionsInt opts =
  case opts of
    Just
      { "default-per-ocpu": defaultPerOcpu
      , "max": max
      , "min": min
      } -> do
      pure $ Just
        { defaultPerOcpu
        , max
        , min
        }
    Nothing -> pure Nothing

type MemoryOptionsInt =
  { "default-per-ocpu-in-gbps" :: Maybe Number
  , "max-in-gbps" :: Maybe Number
  , "max-per-ocpu-in-gbps" :: Maybe Number
  , "min-in-gbps" :: Maybe Number
  , "min-per-ocpu-in-gbps" :: Maybe Number
  }

type MemoryOptions =
  { defaultPerOcpuInGbps :: Maybe Number
  , maxInGbps :: Maybe Number
  , maxPerOcpuInGbps :: Maybe Number
  , minInGbps :: Maybe Number
  , minPerOcpuInGbps :: Maybe Number
  }

fromMemoryOptionsInt :: Maybe MemoryOptionsInt -> F (Maybe MemoryOptions)
fromMemoryOptionsInt opts =
  case opts of
    Just
      { "default-per-ocpu-in-gbps": defaultPerOcpuInGbps
      , "max-in-gbps": maxInGbps
      , "max-per-ocpu-in-gbps": maxPerOcpuInGbps
      , "min-in-gbps": minInGbps
      , "min-per-ocpu-in-gbps": minPerOcpuInGbps
      } -> do
      pure $ Just
        { defaultPerOcpuInGbps
        , maxInGbps
        , maxPerOcpuInGbps
        , minInGbps
        , minPerOcpuInGbps
        }
    Nothing -> pure Nothing

type NetworkingBandwidthOptionsInt =
  { "default-per-ocpus-in-gbps" :: Maybe Number
  , "max-in-gbps" :: Maybe Number
  , "min-in-gbps" :: Maybe Number
  }

type NetworkingBandwidthOptions =
  { defaultPerOcpusInGbps :: Maybe Number
  , maxInGbps :: Maybe Number
  , minInGbps :: Maybe Number
  }

fromNetworkingBandwithOptionsInt :: Maybe NetworkingBandwidthOptionsInt -> F (Maybe NetworkingBandwidthOptions)
fromNetworkingBandwithOptionsInt opts =
  case opts of
    Just
      { "default-per-ocpus-in-gbps": defaultPerOcpusInGbps
      , "max-in-gbps": maxInGbps
      , "min-in-gbps": minInGbps
      } -> do
      pure $ Just
        { defaultPerOcpusInGbps
        , maxInGbps
        , minInGbps
        }
    Nothing -> pure Nothing

type OcpuOptionsInt =
  { "max" :: Maybe Number
  , "min" :: Maybe Number
  }

type OcpuOptions =
  { max :: Maybe Number
  , min :: Maybe Number
  }

fromOcpuOptionsInt :: Maybe OcpuOptionsInt -> F (Maybe OcpuOptions)
fromOcpuOptionsInt opts =
  case opts of
    Just
      { "max": max
      , "min": min
      } -> do
      pure $ Just
        { max
        , min
        }
    Nothing -> pure Nothing

type ServiceEnabledOptionsInt =
  { "allowed-values" :: Maybe (List Boolean)
  , "is-default-enabled" :: Maybe Boolean
  }

type ServiceEnabledOptions =
  { allowedValues :: Maybe (List Boolean)
  , isDefaultEnabled :: Maybe Boolean
  }

fromServiceEnabledOptionsInt :: Maybe ServiceEnabledOptionsInt -> F (Maybe ServiceEnabledOptions)
fromServiceEnabledOptionsInt opts =
  case opts of
    Just
      { "allowed-values": allowedValues
      , "is-default-enabled": isDefaultEnabled
      } -> do
      pure $ Just
        { allowedValues
        , isDefaultEnabled
        }
    Nothing -> pure Nothing

type PercentageOfCoresEnabledOptionsInt =
  { "default-value" :: Maybe Int
  , "max" :: Maybe Int
  , "min" :: Maybe Int
  }

type PercentageOfCoresEnabledOptions =
  { defaultValue :: Maybe Int
  , max :: Maybe Int
  , min :: Maybe Int
  }

fromPercentageOfCoresEnabledOptionsInt :: Maybe PercentageOfCoresEnabledOptionsInt -> F (Maybe PercentageOfCoresEnabledOptions)
fromPercentageOfCoresEnabledOptionsInt opts =
  case opts of
    Just
      { "default-value": defaultValue
      , "max": max
      , "min": min
      } -> do
      pure $ Just
        { defaultValue
        , max
        , min
        }
    Nothing -> pure Nothing

type PlatformConfigOptionsInt =
  { "access-control-service-options" :: Maybe ServiceEnabledOptionsInt
  , "input-output-memory-management-unit-options" :: Maybe ServiceEnabledOptionsInt
  , "measured-boot-options" :: Maybe ServiceEnabledOptionsInt
  , "numa-notes-per-socket-platform-options" :: Maybe ServiceEnabledOptionsInt
  , "percentage-of-cores-enabled-options" :: Maybe PercentageOfCoresEnabledOptionsInt
  , "secure-boot-options" :: Maybe ServiceEnabledOptionsInt
  , "symmetric-multi-threading-options" :: Maybe ServiceEnabledOptionsInt
  , "trusted-platform-module-options" :: Maybe ServiceEnabledOptionsInt
  , "type" :: Maybe String
  , "virtual-instruction-options" :: Maybe ServiceEnabledOptionsInt
  }

type PlatformConfigOptions =
  { accessControlServiceOptions :: Maybe ServiceEnabledOptions
  , inputOutputMemoryManagementUnitOptions :: Maybe ServiceEnabledOptions
  , measuredBootOptions :: Maybe ServiceEnabledOptions
  , numaNodesPerSocketPlatformOptions :: Maybe ServiceEnabledOptions
  , percentageOfCoresEnabledOptions :: Maybe PercentageOfCoresEnabledOptions
  , secureBootOptions :: Maybe ServiceEnabledOptions
  , symmetricMultiThreadingOptions :: Maybe ServiceEnabledOptions
  , trustedPlatformModuleOptions :: Maybe ServiceEnabledOptions
  , type :: Maybe String
  , virtualInstructionOptions :: Maybe ServiceEnabledOptions
  }

fromPlatformConfigOptionsInt :: Maybe PlatformConfigOptionsInt -> F (Maybe PlatformConfigOptions)
fromPlatformConfigOptionsInt opts =
  case opts of
    Just
      { "access-control-service-options": accessControlServiceOptionsInt
      , "input-output-memory-management-unit-options": inputOutputMemoryManagementUnitOptionsInt
      , "measured-boot-options": measuredBootOptionsInt
      , "numa-notes-per-socket-platform-options": numaNodesPerSocketPlatformOptionsInt
      , "percentage-of-cores-enabled-options": percentageOfCoresEnabledOptionsInt
      , "secure-boot-options": secureBootOptionsInt
      , "symmetric-multi-threading-options": symmetricMultiThreadingOptionsInt
      , "trusted-platform-module-options": trustedPlatformModuleOptionsInt
      , "type": platformType
      , "virtual-instruction-options": virtualInstructionsEnabledPlatformOptionsInt
      } -> do
      accessControlServiceOptions <- fromServiceEnabledOptionsInt accessControlServiceOptionsInt
      inputOutputMemoryManagementUnitOptions <- fromServiceEnabledOptionsInt inputOutputMemoryManagementUnitOptionsInt
      measuredBootOptions <- fromServiceEnabledOptionsInt measuredBootOptionsInt
      numaNodesPerSocketPlatformOptions <- fromServiceEnabledOptionsInt numaNodesPerSocketPlatformOptionsInt
      percentageOfCoresEnabledOptions <- fromPercentageOfCoresEnabledOptionsInt percentageOfCoresEnabledOptionsInt
      secureBootOptions <- fromServiceEnabledOptionsInt secureBootOptionsInt
      symmetricMultiThreadingOptions <- fromServiceEnabledOptionsInt symmetricMultiThreadingOptionsInt
      trustedPlatformModuleOptions <- fromServiceEnabledOptionsInt trustedPlatformModuleOptionsInt
      virtualInstructionOptions <- fromServiceEnabledOptionsInt virtualInstructionsEnabledPlatformOptionsInt
      pure $
        Just
          { accessControlServiceOptions
          , inputOutputMemoryManagementUnitOptions
          , measuredBootOptions
          , numaNodesPerSocketPlatformOptions
          , percentageOfCoresEnabledOptions
          , secureBootOptions
          , symmetricMultiThreadingOptions
          , trustedPlatformModuleOptions
          , type: platformType
          , virtualInstructionOptions
          }
    Nothing -> pure Nothing

type AlternativeObjectInt =
  { "shape-name" :: String
  }

type AlternativeObject =
  { shapeName :: String
  }

fromRecommendedAlternative :: AlternativeObjectInt -> F AlternativeObject
fromRecommendedAlternative { "shape-name": shapeName } = do pure $ { shapeName }

fromRecommendedAlternativesInt :: Maybe (List AlternativeObjectInt) -> F (Maybe (List AlternativeObject))
fromRecommendedAlternativesInt alts =
  case alts of
    Just alts' -> ado
      alternatives <- traverse fromRecommendedAlternative alts'
      in Just alternatives
    Nothing -> pure $ Nothing

type ShapeDescriptionInt =
  { "availability-domain" :: Maybe String
  , "baseline-ocpu-utilizations" :: Maybe (List String)
  , "billing-type" :: Maybe String
  , "defined-tags" :: Maybe (Map String (Map String String))
  , "freeform-tags" :: Maybe (Map String String)
  , "gpu-description" :: Maybe String
  , "gpus" :: Maybe Int
  , "is-billed-for-stopped-instance" :: Maybe Boolean
  , "is-flexible" :: Maybe Boolean
  , "is-live-migration-supported" :: Maybe Boolean
  , "is-subcore" :: Maybe Boolean
  , "local-disk-description" :: Maybe String
  , "local-disks" :: Maybe Int
  , "local-disks-total-size-in-gbs" :: Maybe Number
  , "max-vnic-attachment-options" :: Maybe MaxVnicAttachmentOptionsInt
  , "max-vnic-attachments" :: Maybe Int
  , "memory-in-gbs" :: Maybe Number
  , "memory-options" :: Maybe MemoryOptionsInt
  , "min-total-baseline-ocpus-required" :: Maybe Number
  , "network-ports" :: Maybe Int
  , "networking-bandwidth-in-gbps" :: Maybe Number
  , "networking-bandwidth-options" :: Maybe NetworkingBandwidthOptionsInt
  , "ocpu-options" :: Maybe OcpuOptionsInt
  , "ocpus" :: Maybe Number
  , "platform-config-options" :: Maybe PlatformConfigOptionsInt
  , "processor-description" :: Maybe String
  , "quota-names" :: Maybe (List String)
  , "rdma-bandwidth-in-gbps" :: Maybe Int
  , "rdma-ports" :: Maybe Int
  , "recommended-alternatives" :: Maybe (List AlternativeObjectInt)
  , "resize-compatible-shapes" :: Maybe (List String)
  , "shape" :: String
  }

type ShapeDescription =
  { availabilityDomain :: Maybe String
  , baselineOcpuUtilizations :: Maybe (List String)
  , billingType :: Maybe String
  , definedTags :: Maybe (Map String (Map String String))
  , freeformTags :: Maybe (Map String String)
  , gpuDescription :: Maybe String
  , gpus :: Maybe Int
  , isBilledForStoppedInstance :: Maybe Boolean
  , isFlexible :: Maybe Boolean
  , isLiveMigrationSupported :: Maybe Boolean
  , isSubcore :: Maybe Boolean
  , localDiskDescription :: Maybe String
  , localDisks :: Maybe Int
  , localDisksTotalSizeInGbs :: Maybe Number
  , maxVnicAttachmentOptions :: Maybe MaxVnicAttachmentOptions
  , maxVnicAttachments :: Maybe Int
  , memoryInGbs :: Maybe Number
  , memoryOptions :: Maybe MemoryOptions
  , minTotalBaselineOcpusRequired :: Maybe Number
  , networkPorts :: Maybe Int
  , networkingBandwidthInGbps :: Maybe Number
  , networkingBandwidthOptions :: Maybe NetworkingBandwidthOptions
  , ocpuOptions :: Maybe OcpuOptions
  , ocpus :: Maybe Number
  , platformConfigOptions :: Maybe PlatformConfigOptions
  , processorDescription :: Maybe String
  , quotaNames :: Maybe (List String)
  , rdmaBandwidthInGbps :: Maybe Int
  , rdmaPorts :: Maybe Int
  , recommendedAlternatives :: Maybe (List AlternativeObject)
  , resizeCompatibleShapes :: Maybe (List String)
  , shape :: Shape
  }

fromShapesDescriptionInt :: ShapeDescriptionInt -> F ShapeDescription
fromShapesDescriptionInt
  { "availability-domain": availabilityDomain
  , "baseline-ocpu-utilizations": baselineOcpuUtilizations
  , "billing-type": billingType
  , "defined-tags": definedTags
  , "freeform-tags": freeformTags
  , "gpu-description": gpuDescription
  , "gpus": gpus
  , "is-billed-for-stopped-instance": isBilledForStoppedInstance
  , "is-flexible": isFlexible
  , "is-live-migration-supported": isLiveMigrationSupported
  , "is-subcore": isSubcore
  , "local-disk-description": localDiskDescription
  , "local-disks": localDisks
  , "local-disks-total-size-in-gbs": localDisksTotalSizeInGbs
  , "max-vnic-attachment-options": maxVnicAttachmentOptionsInt
  , "max-vnic-attachments": maxVnicAttachments
  , "memory-in-gbs": memoryInGbs
  , "memory-options": memoryOptionsInt
  , "min-total-baseline-ocpus-required": minTotalBaselineOcpusRequired
  , "network-ports": networkPorts
  , "networking-bandwidth-in-gbps": networkingBandwidthInGbps
  , "networking-bandwidth-options": networkingBandwidthOptionsInt
  , "ocpu-options": ocpuOptionsInt
  , "ocpus": ocpus
  , "platform-config-options": platformConfigOptionsInt
  , "processor-description": processorDescription
  , "quota-names": quotaNames
  , "rdma-bandwidth-in-gbps": rdmaBandwidthInGbps
  , "rdma-ports": rdmaPorts
  , "recommended-alternatives": recommendedAlternativesInt
  , "resize-compatible-shapes": resizeCompatibleShapes
  , "shape": shape
  } = ado
  maxVnicAttachmentOptions <- fromMaxVnicAttachmentOptionsInt maxVnicAttachmentOptionsInt
  memoryOptions <- fromMemoryOptionsInt memoryOptionsInt
  networkingBandwidthOptions <- fromNetworkingBandwithOptionsInt networkingBandwidthOptionsInt
  ocpuOptions <- fromOcpuOptionsInt ocpuOptionsInt
  platformConfigOptions <- fromPlatformConfigOptionsInt platformConfigOptionsInt
  recommendedAlternatives <- fromRecommendedAlternativesInt recommendedAlternativesInt
  in
    { availabilityDomain
    , baselineOcpuUtilizations
    , billingType
    , definedTags
    , freeformTags
    , gpuDescription
    , gpus
    , isBilledForStoppedInstance
    , isFlexible
    , isLiveMigrationSupported
    , isSubcore
    , localDiskDescription
    , localDisks
    , localDisksTotalSizeInGbs
    , maxVnicAttachmentOptions
    , maxVnicAttachments
    , memoryInGbs
    , memoryOptions
    , minTotalBaselineOcpusRequired
    , networkPorts
    , networkingBandwidthInGbps
    , networkingBandwidthOptions
    , ocpuOptions
    , ocpus
    , platformConfigOptions
    , processorDescription
    , quotaNames
    , rdmaBandwidthInGbps
    , rdmaPorts
    , recommendedAlternatives
    , resizeCompatibleShapes
    , shape: Shape shape
    }

type ShapeDescriptionsInt =
  { "data" :: List ShapeDescriptionInt
  }

fromShapesResponseInt :: ShapeDescriptionsInt -> F (List ShapeDescription)
fromShapesResponseInt { "data": shapeData } = ado
  shapes <- traverse fromShapesDescriptionInt shapeData
  in shapes

listShapes :: ListShapesRequest -> Effect (Either MultipleErrors (List ShapeDescription))
listShapes req = do
  let
    cli = ociCliBase req "compute shape list"
  outputJson <- runOciCli cli
  pure $ runExcept $ fromShapesResponseInt =<< readJSON' =<< outputJson

type ListCompartmentsRequest = BaseRequest ()

type CompartmentDescriptionInt =
  { "compartment-id" :: String
  , "defined-tags" :: Maybe (Map String (Map String String))
  , "description" :: String
  , "freeform-tags" :: Maybe (Map String String)
  , "id" :: String
  , "inactive-status" :: Maybe Int
  , "is-accessible" :: Maybe Boolean
  , "lifecycle-state" :: String
  , "name" :: String
  , "time-created" :: String
  }

type CompartmentDescription =
  { compartmentId :: CompartmentId
  , definedTags :: Maybe (Map String (Map String String))
  , description :: String
  , freeformTags :: Maybe (Map String String)
  , id :: String
  , inactiveStatus :: Maybe Int
  , isAccessible :: Maybe Boolean
  , lifecycleState :: String
  , name :: String
  , timeCreated :: String
  }

fromCompartmentInt :: CompartmentDescriptionInt -> F CompartmentDescription
fromCompartmentInt
  { "compartment-id": compartmentId
  , "defined-tags": definedTags
  , "description": description
  , "freeform-tags": freeformTags
  , "id": id
  , "inactive-status": inactiveStatus
  , "is-accessible": isAccessible
  , "lifecycle-state": lifecycleState
  , "name": name
  , "time-created": timeCreated
  } = do
  pure $
    { compartmentId: CompartmentId compartmentId
    , definedTags
    , description
    , freeformTags
    , id
    , inactiveStatus
    , isAccessible
    , lifecycleState
    , name
    , timeCreated
    }

type CompartmentDescriptionsInt =
  { "data" :: List CompartmentDescriptionInt
  }

fromCompartmentResponseInt :: CompartmentDescriptionsInt -> F (List CompartmentDescription)
fromCompartmentResponseInt { "data": compartmentData } = ado
  compartments <- traverse fromCompartmentInt compartmentData
  in compartments

listCompartments :: ListCompartmentsRequest -> Effect (Either MultipleErrors (List CompartmentDescription))
listCompartments req = do
  let
    cli = ociCliBase req "iam compartment list"
  outputJson <- runOciCli cli
  pure $ runExcept $ fromCompartmentResponseInt =<< readJSON' =<< outputJson

ociCliBase :: forall t. BaseRequest t -> String -> String
ociCliBase { compartment } command = do
  "oci "
    <> command
    <> " --output json --all "
    <> (fromMaybe "" $ (\r -> " --compartment-id " <> r) <$> unwrap <$> compartment)

runOciCli :: String -> Effect (F String)
runOciCli cmd = do
  res <- runCommand $ spy "CMD:" cmd
  case res of
    Left { output } -> pure $ except $ Left $ singleton $ ForeignError $ "oci cli failure: " <> output
    Right output -> pure $ except $ Right output

type CmdError =
  { exitStatus :: Number
  , output :: String
  }

foreign import runCommand :: String -> Effect (Either CmdError String)

foreign import base64Decode :: String -> Maybe String
