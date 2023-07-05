module Erl.Oracle.Shape
  ( AlternativeObject
  , ListShapesRequest
  , MaxVnicAttachmentOptions
  , MemoryOptions
  , NetworkingBandwidthOptions
  , OcpuOptions
  , PercentageOfCoresEnabledOptions
  , PlatformConfigOptions
  , ServiceEnabledOptions
  , ShapeDescription
  , defaultListShapesRequest
  , listShapes
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect (Effect)
import Erl.Data.List (List)
import Erl.Data.Map (Map)
import Erl.Oracle.Shared (BaseRequest, ociCliBase, runOciCli)
import Erl.Oracle.Types (CompartmentId(..), Shape(..))
import Foreign (F, MultipleErrors)
import Simple.JSON (readJSON')

type ListShapesRequest = BaseRequest
  ( compartment :: Maybe CompartmentId
  )

defaultListShapesRequest :: ListShapesRequest
defaultListShapesRequest =
  { compartment: Nothing
  }

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
listShapes req@{ compartment } = do
  let
    cli = ociCliBase req "compute shape list"
      <> " --all "
      <> (fromMaybe "" $ (\r -> " --compartment-id " <> r) <$> unwrap <$> compartment)
  outputJson <- runOciCli cli
  pure $ runExcept $ fromShapesResponseInt =<< readJSON' =<< outputJson
