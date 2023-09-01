module Erl.Oracle.Types.Shape
  ( AlternativeObject
  , MaxVnicAttachmentOptions
  , MemoryOptions
  , NetworkingBandwidthOptions
  , OcpuOptions
  , PercentageOfCoresEnabledOptions
  , PlatformConfigOptions
  , ServiceEnabledOptions
  , ShapeDescription
  ) where

import Data.Maybe (Maybe)
import Erl.Data.List (List)
import Erl.Data.Map (Map)
import Erl.Oracle.Types.Common (Shape)

type AlternativeObject =
  { shapeName :: String
  }

type PercentageOfCoresEnabledOptions =
  { defaultValue :: Maybe Int
  , max :: Maybe Int
  , min :: Maybe Int
  }

type ServiceEnabledOptions =
  { allowedValues :: Maybe (List Boolean)
  , isDefaultEnabled :: Maybe Boolean
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

type OcpuOptions =
  { max :: Maybe Number
  , min :: Maybe Number
  }

type NetworkingBandwidthOptions =
  { defaultPerOcpusInGbps :: Maybe Number
  , maxInGbps :: Maybe Number
  , minInGbps :: Maybe Number
  }

type MemoryOptions =
  { defaultPerOcpuInGbps :: Maybe Number
  , maxInGbps :: Maybe Number
  , maxPerOcpuInGbps :: Maybe Number
  , minInGbps :: Maybe Number
  , minPerOcpuInGbps :: Maybe Number
  }

type MaxVnicAttachmentOptions =
  { defaultPerOcpu :: Maybe Number
  , max :: Maybe Number
  , min :: Maybe Int
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
