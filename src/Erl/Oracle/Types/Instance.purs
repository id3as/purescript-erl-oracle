module Erl.Oracle.Types.Instance
  ( AgentConfig
  , AgentPluginConfigDetails
  , AvailabilityConfig
  , InstanceAgentConfig
  , InstanceAgentPluginConfigDetails
  , InstanceAvailabilityConfig
  , InstanceDescription
  , InstanceLifecycleState(..)
  , InstanceOptions
  , InstancePlatformConfig
  , InstanceShapeConfig
  , LaunchInstanceRequest
  , LaunchOptions
  , PlatformConfig
  , PreemptibleInstanceConfig
  , PreemptionAction
  , ShapeConfig
  )
  where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Erl.Data.List (List)
import Erl.Data.Map (Map)
import Erl.Oracle.Shared (BaseRequest)
import Erl.Oracle.Types.Common (AvailabilityDomainId, CapacityReservationId, CompartmentId, DedicatedVmHostId, DefinedTags, ExtendedMetadata, FaultDomainId(..), FreeformTags, ImageId, InstanceId, LaunchMode, Metadata, Shape, SubnetId(..), VolumeId(..))
import Foreign (unsafeFromForeign)
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON (class ReadForeign, class WriteForeign, write)

data InstanceLifecycleState
  = Moving
  | Provisioning
  | Running
  | Starting
  | Stopping
  | Stopped
  | CreatingImage
  | Terminating
  | Terminated

derive instance Eq InstanceLifecycleState
derive instance Generic InstanceLifecycleState _
instance ReadForeign InstanceLifecycleState where
  readImpl f =
    case unsafeFromForeign f of
      "MOVING" -> pure Moving
      "PROVISIONING" -> pure Provisioning
      "RUNNING" -> pure Running
      "STARTING" -> pure Starting
      "STOPPING" -> pure Stopping
      "STOPPED" -> pure Stopped
      "CREATING_IMAGE" -> pure CreatingImage
      "TERMINATING" -> pure Terminating
      "TERMINATED" -> pure Terminated
      somethingElse -> unsafeCrashWith $ "Unexpected InstanceLifecycleState " <> somethingElse

instance WriteForeign InstanceLifecycleState where
  writeImpl f =
    case f of
      Moving -> write "MOVING"
      Provisioning -> write "PROVISIONING"
      Running -> write "RUNNING"
      Starting -> write "STARTING"
      Stopping -> write "STOPPING"
      Stopped -> write "STOPPED"
      CreatingImage -> write "CREATING_IMAGE"
      Terminating -> write "TERMINATING"
      Terminated -> write "TERMINATED"

instance Show InstanceLifecycleState where
  show = genericShow

type InstanceAgentPluginConfigDetails =
  { desiredState :: String
  , name :: String
  }

type InstanceAgentConfig =
  { areAllPluginsDisabled :: Maybe Boolean
  , isManagementDisabled :: Maybe Boolean
  , isMonitoringDisabled :: Maybe Boolean
  , pluginsConfig :: Maybe (List InstanceAgentPluginConfigDetails)
  }

type InstanceAvailabilityConfig =
  { isLiveMigrationPreferred :: Maybe Boolean
  , recoveryAction :: Maybe String
  }

type InstanceOptions =
  { areLegacyLmdsEndpointsDisabled :: Maybe Boolean
  }

type LaunchOptions =
  { bootVolumeType :: Maybe String
  , firmware :: Maybe String
  , isConsistentVolumeNamingEnabled :: Maybe Boolean
  , isPvEncryptionInTransitEnabled :: Maybe Boolean
  , networkType :: Maybe String
  , remoteDataVolumeType :: Maybe String
  }

type InstancePlatformConfig =
  { isMeasuredBootEnabled :: Maybe Boolean
  , isSecureBootEnabled :: Maybe Boolean
  , isTrustedPlatformEnabled :: Maybe Boolean
  , type :: String
  }

type PreemptionAction =
  { type :: String
  }

type PreemptibleInstanceConfig =
  { preemptionAction :: PreemptionAction
  }

type AgentConfig =
  { areAllPluginsDisabled :: Maybe Boolean
  , isManagementDisabled :: Maybe Boolean
  , isMonitoringDisabled :: Maybe Boolean
  , pluginsConfig :: Maybe (List AgentPluginConfigDetails)
  }

type AgentPluginConfigDetails =
  { desiredState :: String
  , name :: String
  }

type AvailabilityConfig =
  { isLiveMigrationPreferred :: Maybe Boolean
  , recoveryAction :: Maybe String
  }

type ShapeConfig =
  { baselineOcpuUtilization :: Maybe String
  , memoryInGBs :: Maybe Number
  , nvmes :: Maybe Int
  , ocpus :: Maybe Number
  }

type PlatformConfig =
  { isMeasuredBootEnabled :: Maybe Boolean
  , isSecureBootEnabled :: Maybe Boolean
  , isTrustedPlatformModuleEnabled :: Maybe Boolean
  , type :: Maybe String
  }

type InstanceShapeConfig =
  { baselineOcpuUtilization :: Maybe String
  , gpuDescription :: Maybe String
  , gpus :: Maybe Int
  , localDiskDescription :: Maybe String
  , localDisks :: Maybe Int
  , localDiskSizeInGbs :: Maybe Number
  , maxVnicAttachments :: Maybe Int
  , memoryInGbs :: Maybe Number
  , networkingBandwidthInGbps :: Maybe Number
  , ocpus :: Maybe Number
  , processorDescription :: Maybe String
  }

type InstanceDescription =
  { agentConfig :: Maybe InstanceAgentConfig
  , availabilityConfig :: Maybe InstanceAvailabilityConfig
  , availabilityDomain :: AvailabilityDomainId
  , capacityReservationId :: Maybe CapacityReservationId
  , compartmentId :: CompartmentId
  , dedicatedVmHostId :: Maybe DedicatedVmHostId
  , definedTags :: Maybe DefinedTags
  , displayName :: Maybe String
  , extendedMetadata :: Maybe ExtendedMetadata
  , faultDomain :: Maybe String
  , freeformTags :: Maybe FreeformTags
  , id :: InstanceId
  , imageId :: ImageId
  , instanceOptions :: Maybe InstanceOptions
  , ipxeScript :: Maybe String
  , launchMode :: Maybe LaunchMode
  , launchOptions :: Maybe LaunchOptions
  , lifecycleState :: InstanceLifecycleState
  , metadata :: Maybe Metadata
  , platformConfig :: Maybe InstancePlatformConfig
  , preemptibleInstanceConfig :: Maybe PreemptibleInstanceConfig
  , shape :: Shape
  , shapeConfig :: Maybe InstanceShapeConfig
  --, sourceDetails :: Maybe InstanceSourceDetails
  , timeCreated :: String
  , timeMaintenanceRebootDue :: Maybe String
  }

type LaunchInstanceRequest = BaseRequest
  ( availabilityDomain :: AvailabilityDomainId
  , compartment :: CompartmentId
  , shape :: Shape
  , subnet :: SubnetId
  , capacityReservation :: Maybe CapacityReservationId
  , dedicatedVmHost :: Maybe DedicatedVmHostId
  , definedTags :: Maybe DefinedTags
  , displayName :: Maybe String
  , extendedMetadata :: Maybe ExtendedMetadata
  , faultDomain :: Maybe FaultDomainId
  , freeformTags :: Maybe (Map String String)
  , hostname :: Maybe String
  , imageId :: Maybe ImageId
  , launchOptions :: Maybe LaunchOptions
  , instanceOptions :: Maybe InstanceOptions
  , availabilityConfig :: Maybe AvailabilityConfig
  , preemptibleInstanceConfig :: Maybe PreemptibleInstanceConfig
  , metadata :: Maybe (Map String String)
  , agentConfig :: Maybe AgentConfig
  , shapeConfig :: Maybe ShapeConfig
  , isPvEncryptionInTransitEnabled :: Maybe Boolean
  , ipxeScript :: Maybe String
  , platformConfig :: Maybe PlatformConfig
  , vnicDisplayName :: Maybe String
  , nsgIds :: Maybe (List String)
  , assignPublicIp :: Maybe Boolean
  , privateIp :: Maybe String
  , skipSourceDestCheck :: Maybe Boolean
  , userData :: Maybe String
  , sshAuthorizedKeys :: Maybe String
  , sourceBootVolume :: Maybe VolumeId
  , bootVolumeSizeInGbps :: Maybe Int
  )
