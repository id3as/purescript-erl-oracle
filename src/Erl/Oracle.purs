module Erl.Oracle
  ( fromAvailabilityDomainInt
  , listAvailabilityDomains
  , listCapacityReservations
  , listCompartments
  , listCompatibleShapes
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
import Data.Newtype (class Newtype, overF, unwrap)
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
import Erl.Oracle.CapactityReservationLifecycleState (CapactityReservationLifecycleState)
import Erl.Oracle.CompartmentTypes (CompartmentLifecycleState)
import Erl.Oracle.ImageTypes (ImageLifecycleState)
import Erl.Oracle.InstanceLifecycleTypes (InstanceLifecycleState)
import Erl.Oracle.Types (AvailabilityDomainId(..), CapacityReservationId(..), CompartmentId(..), ComputeClusterId(..), DedicatedVmHostId(..), FaultDomainId(..), HpcIslandId(..), ImageId(..), InstanceId(..), KmsKeyId(..), NetworkBlockId(..), Shape(..))
import Foreign (F, ForeignError(..), MultipleErrors, readString, unsafeFromForeign)
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON (class ReadForeign, class WriteForeign, class WriteForeignKey, E, read', readJSON', writeImpl, writeJSON)
import Text.Parsing.Parser (ParserT, fail, parseErrorMessage, runParser)
import Unsafe.Coerce (unsafeCoerce)

type BaseRequest a =
  {
  | a
  }

type InstanceAgentPluginConfigDetailsInt =
  { "desired-state" :: String
  , "name" :: String
  }

type InstanceAgentPluginConfigDetails =
  { desiredState :: String
  , name :: String
  }

fromInstanceAgentPluginConfigDetailInt :: InstanceAgentPluginConfigDetailsInt -> F InstanceAgentPluginConfigDetails
fromInstanceAgentPluginConfigDetailInt
  { "desired-state": desiredState
  , "name": name
  } = do
  pure $
    { desiredState
    , name
    }

fromInstanceAgentPluginConfigDetailsInt :: Maybe (List InstanceAgentPluginConfigDetailsInt) -> F (Maybe (List InstanceAgentPluginConfigDetails))
fromInstanceAgentPluginConfigDetailsInt configs =
  case configs of
    Just configs' -> ado
      pluginConfigs <- traverse fromInstanceAgentPluginConfigDetailInt configs'
      in Just pluginConfigs
    Nothing -> pure $ Nothing

type InstanceAgentConfigInt =
  { "are-all-plugins-disabled" :: Maybe Boolean
  , "is-management-disabled" :: Maybe Boolean
  , "is-monitoring-disabled" :: Maybe Boolean
  , "plugins-config" :: Maybe (List InstanceAgentPluginConfigDetailsInt)
  }

type InstanceAgentConfig =
  { areAllPluginsDisabled :: Maybe Boolean
  , isManagementDisabled :: Maybe Boolean
  , isMonitoringDisabled :: Maybe Boolean
  , pluginsConfig :: Maybe (List InstanceAgentPluginConfigDetails)
  }

fromInstanceAgentConfigInt :: Maybe InstanceAgentConfigInt -> F (Maybe InstanceAgentConfig)
fromInstanceAgentConfigInt config =
  case config of
    Just
      { "are-all-plugins-disabled": areAllPluginsDisabled
      , "is-management-disabled": isManagementDisabled
      , "is-monitoring-disabled": isMonitoringDisabled
      , "plugins-config": pluginsConfigInt
      } -> do
      pluginsConfig <- fromInstanceAgentPluginConfigDetailsInt pluginsConfigInt
      pure $
        Just
          { areAllPluginsDisabled
          , isManagementDisabled
          , isMonitoringDisabled
          , pluginsConfig
          }
    Nothing -> pure Nothing

type InstanceAvailabilityConfigInt =
  { "is-live-migration-preferred" :: Maybe Boolean
  , "recovery-action" :: Maybe String
  }

type InstanceAvailabilityConfig =
  { isLiveMigrationPreferred :: Maybe Boolean
  , recoveryAction :: Maybe String
  }

fromInstanceAvailabilityConfigInt :: Maybe InstanceAvailabilityConfigInt -> F (Maybe InstanceAvailabilityConfig)
fromInstanceAvailabilityConfigInt config =
  case config of
    Just
      { "is-live-migration-preferred": isLiveMigrationPreferred
      , "recovery-action": recoveryAction
      } -> do
      pure $ Just
        { isLiveMigrationPreferred
        , recoveryAction
        }
    Nothing -> pure Nothing

type InstanceOptionsInt =
  { "are-legacy-lmds-endpoints-disabled" :: Maybe Boolean
  }

type InstanceOptions =
  { areLegacyLmdsEndpointsDisabled :: Maybe Boolean
  }

fromInstanceOptionsInt :: Maybe InstanceOptionsInt -> F (Maybe InstanceOptions)
fromInstanceOptionsInt opts =
  case opts of
    Just { "are-legacy-lmds-endpoints-disabled": areLegacyLmdsEndpointsDisabled } ->
      do pure $ Just { areLegacyLmdsEndpointsDisabled }
    Nothing -> pure Nothing

type PreemptionActionInt =
  { "type" :: String
  }

type PreemptionAction =
  { type :: String
  }

fromPreemptionActionInt :: PreemptionActionInt -> F PreemptionAction
fromPreemptionActionInt { "type": actiontype } = do pure { type: actiontype }

type PreemptibleInstanceConfigInt =
  { "preemption-action" :: PreemptionActionInt
  }

type PreemptibleInstanceConfig =
  { preemptionAction :: PreemptionAction
  }

fromPreemptibleInstanceConfigInt :: Maybe PreemptibleInstanceConfigInt -> F (Maybe PreemptibleInstanceConfig)
fromPreemptibleInstanceConfigInt config =
  case config of
    Just { "preemption-action": preemptionAction } -> do
      action <- fromPreemptionActionInt preemptionAction
      pure $ Just { preemptionAction: action }
    Nothing -> pure $ Nothing

type InstanceShapeConfigInt =
  { "baseline-ocpu-utilization" :: Maybe String
  , "gpu-description" :: Maybe String
  , "gpus" :: Maybe Int
  , "local-disk-description" :: Maybe String
  , "local-disks" :: Maybe Int
  , "local-disk-size-in-gbs" :: Maybe Number
  , "max-vnic-attachments" :: Maybe Int
  , "memory-in-gbs" :: Maybe Number
  , "networking-bandwidth-in-gbps" :: Maybe Number
  , "ocpus" :: Maybe Number
  , "processor-description" :: Maybe String
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

fromInstanceShapeConfigInt :: Maybe InstanceShapeConfigInt -> F (Maybe InstanceShapeConfig)
fromInstanceShapeConfigInt config =
  case config of
    Just
      { "baseline-ocpu-utilization": baselineOcpuUtilization
      , "gpu-description": gpuDescription
      , "gpus": gpus
      , "local-disk-description": localDiskDescription
      , "local-disks": localDisks
      , "local-disk-size-in-gbs": localDiskSizeInGbs
      , "max-vnic-attachments": maxVnicAttachments
      , "memory-in-gbs": memoryInGbs
      , "networking-bandwidth-in-gbps": networkingBandwidthInGbps
      , "ocpus": ocpus
      , "processor-description": processorDescription
      } ->
      do
        pure $ Just
          { baselineOcpuUtilization
          , gpuDescription
          , gpus
          , localDiskDescription
          , localDisks
          , localDiskSizeInGbs
          , maxVnicAttachments
          , memoryInGbs
          , networkingBandwidthInGbps
          , ocpus
          , processorDescription
          }
    Nothing -> pure Nothing

type BootVolumeDetailsInt =
  { "source-type" :: String
  , "boot-volume-id" :: String
  }

type BootVolumeDetails =
  { sourceType :: String
  , bootVolumeId :: String
  }

fromBootVolumeDetailsInt :: BootVolumeDetailsInt -> F BootVolumeDetails
fromBootVolumeDetailsInt
  { "source-type": sourceType
  , "boot-volume-id": bootVolumeId
  } = do
  pure
    { sourceType
    , bootVolumeId
    }

type SourceViaImageDetailsInt =
  { "source-type" :: String
  , "boot-volume-size-in-gbps" :: Maybe Int
  , "boot-volume-vpus-per-gb" :: Maybe Int
  , "image-id" :: String
  , "kms-key-id" :: Maybe String
  }

type SourceViaImageDetails =
  { sourceType :: String
  , bootVolumeSizeInGbps :: Maybe Int
  , bootVolumeVpusPerGb :: Maybe Int
  , imageId :: ImageId
  , kmsKeyId :: Maybe KmsKeyId
  }

fromSourceViaImageDetailsInt :: SourceViaImageDetailsInt -> F SourceViaImageDetails
fromSourceViaImageDetailsInt
  { "source-type": sourceType
  , "boot-volume-size-in-gbps": bootVolumeSizeInGbps
  , "boot-volume-vpus-per-gb": bootVolumeVpusPerGb
  , "image-id": imageId
  , "kms-key-id": kmsKeyId
  } = do
  pure
    { sourceType
    , bootVolumeSizeInGbps
    , bootVolumeVpusPerGb
    , imageId: ImageId imageId
    , kmsKeyId: maybe Nothing (\t -> Just $ KmsKeyId t) kmsKeyId
    }

data InstanceSourceTypeInt
  = BootVolumeInt BootVolumeDetailsInt
  | ImageInt SourceViaImageDetailsInt

-- derive instance Eq InstanceSourceTypeInt
-- derive instance Generic InstanceSourceTypeInt _
-- instance ReadForeign InstanceSourceTypeInt where
--   readImpl f =
--     case spy "instanceSourceType" unsafeFromForeign f of
--       _ -> BootVolumeInt { "source-type": "Foo", "boot-volume-id": "12345" }

data InstanceSourceType
  = BootVolume BootVolumeDetails
  | Image SourceViaImageDetails

type InstanceSourceDetailsInt =
  { "source-type" :: InstanceSourceTypeInt
  }

type InstanceSourceDetails =
  { sourceType :: InstanceSourceType
  }

-- fromInstanceSourceType :: InstanceSourceTypeInt -> F InstanceSourceType
-- fromInstanceSourceType sourceType = do
--   pure $ case sourceType of
--     BootVolumeInt t -> BootVolume $ fromBootVolumeDetailsInt t
--     ImageInt t -> Image $ fromSourceViaImageDetailsInt t

-- fromInstanceSourceDetailsInt :: Maybe InstanceSourceDetailsInt -> F (Maybe InstanceSourceDetails)
-- fromInstanceSourceDetailsInt =
--   case _ of
--     Just
--       { "source-type": sourceTypeInt
--       } -> ado
--       sourceType <- fromInstanceSourceType sourceTypeInt
--       in
--         Just
--           { sourceType
--           }
--     Nothing -> pure $ Nothing

type InstancePlatformConfigInt =
  { "is-measured-boot-enabled" :: Maybe Boolean
  , "is-secure-boot-enabled" :: Maybe Boolean
  , "is-trusted-platform-enabled" :: Maybe Boolean
  , "type" :: String
  }

type InstancePlatformConfig =
  { isMeasuredBootEnabled :: Maybe Boolean
  , isSecureBootEnabled :: Maybe Boolean
  , isTrustedPlatformEnabled :: Maybe Boolean
  , type :: String
  }

fromInstancePlatformConfigInt :: Maybe InstancePlatformConfigInt -> F (Maybe InstancePlatformConfig)
fromInstancePlatformConfigInt config =
  case config of
    Just
      { "is-measured-boot-enabled": isMeasuredBootEnabled
      , "is-secure-boot-enabled": isSecureBootEnabled
      , "is-trusted-platform-enabled": isTrustedPlatformEnabled
      , "type": platformType
      } -> pure $ Just
      { isMeasuredBootEnabled
      , isSecureBootEnabled
      , isTrustedPlatformEnabled
      , type: platformType
      }
    Nothing -> pure $ Nothing

type InstanceDescriptionInt =
  { "agent-config" :: Maybe InstanceAgentConfigInt
  , "availability-config" :: Maybe InstanceAvailabilityConfigInt
  , "availability-domain" :: String
  , "capacity-reservation-id" :: Maybe String
  , "compartment-id" :: String
  , "dedicated-vm-host-id" :: Maybe String
  , "defined-tags" :: Maybe (Map String (Map String String))
  , "display-name" :: Maybe String
  , "extended-metadata" :: Maybe (Map String String)
  , "fault-domain" :: Maybe String
  , "freeform-tags" :: Maybe (Map String String)
  , "id" :: String
  , "image-id" :: String
  , "instance=options" :: Maybe InstanceOptionsInt
  , "ipxe-script" :: Maybe String
  , "launch-mode" :: Maybe String
  , "launch-options" :: Maybe LaunchOptionsInt
  , "lifecycle-state" :: InstanceLifecycleState
  , "metadata" :: Maybe String
  , "platform-config" :: Maybe InstancePlatformConfigInt
  , "preemptible-instance-config" :: Maybe PreemptibleInstanceConfigInt
  , "shape" :: String
  , "shape-config" :: Maybe InstanceShapeConfigInt
  --, "source-details" :: Maybe InstanceSourceDetailsInt
  , "time-created" :: String
  , "time-maintenance-reboot-due" :: Maybe String
  }

type InstanceDescription =
  { agentConfig :: Maybe InstanceAgentConfig
  , availabilityConfig :: Maybe InstanceAvailabilityConfig
  , availabilityDomain :: AvailabilityDomainId
  , capacityReservationId :: Maybe CapacityReservationId
  , compartmentId :: CompartmentId
  , dedicatedVmHostId :: Maybe DedicatedVmHostId
  , definedTags :: Maybe (Map String (Map String String))
  , displayName :: Maybe String
  , extendedMetadata :: Maybe (Map String String)
  , faultDomain :: Maybe String
  , freeformTags :: Maybe (Map String String)
  , id :: InstanceId
  , imageId :: ImageId
  , instanceOptions :: Maybe InstanceOptions
  , ipxeScript :: Maybe String
  , launchMode :: Maybe String
  , launchOptions :: Maybe LaunchOptions
  , lifecycleState :: InstanceLifecycleState
  , metadata :: Maybe String
  , platformConfig :: Maybe InstancePlatformConfig
  , preemptibleInstanceConfig :: Maybe PreemptibleInstanceConfig
  , shape :: Shape
  , shapeConfig :: Maybe InstanceShapeConfig
  --, sourceDetails :: Maybe InstanceSourceDetails
  , timeCreated :: String
  , timeMaintenanceRebootDue :: Maybe String
  }

fromListInstanceDescription :: InstanceDescriptionInt -> F InstanceDescription
fromListInstanceDescription
  { "agent-config": instanceAgentConfigInt
  , "availability-config": instanceAvailabilityConfigInt
  , "availability-domain": availabilityDomain
  , "capacity-reservation-id": capacityReservation
  , "compartment-id": compartment
  , "dedicated-vm-host-id": dedicatedVmHost
  , "defined-tags": definedTags
  , "display-name": displayName
  , "extended-metadata": extendedMetadata
  , "fault-domain": faultDomain
  , "freeform-tags": freeformTags
  , "id": id
  , "image-id": imageId
  , "instance=options": instanceOptionsInt
  , "ipxe-script": ipxeScript
  , "launch-mode": launchMode
  , "launch-options": launchOptionsInt
  , "lifecycle-state": lifecycleState
  , "metadata": metadata
  , "platform-config": instancePlatformConfigInt
  , "preemptible-instance-config": preemptibleInstanceConfigInt
  , "shape": shape
  , "shape-config": shapeConfigInt
  --, "source-details": sourceDetailsInt
  , "time-created": timeCreated
  , "time-maintenance-reboot-due": timeMaintenanceRebootDue
  } = ado
  agentConfig <- fromInstanceAgentConfigInt instanceAgentConfigInt
  availabilityConfig <- fromInstanceAvailabilityConfigInt instanceAvailabilityConfigInt
  instanceOptions <- fromInstanceOptionsInt instanceOptionsInt
  launchOptions <- fromLaunchOptionsInt launchOptionsInt
  platformConfig <- fromInstancePlatformConfigInt instancePlatformConfigInt
  preemptibleInstanceConfig <- fromPreemptibleInstanceConfigInt preemptibleInstanceConfigInt
  shapeConfig <- fromInstanceShapeConfigInt shapeConfigInt
  -- sourceDetails <- fromInstanceSourceDetailsInt sourceDetailsInt
  in
    { agentConfig
    , availabilityConfig
    , availabilityDomain: AvailabilityDomainId availabilityDomain
    , capacityReservationId: maybe Nothing (\t -> Just $ CapacityReservationId t) capacityReservation
    , compartmentId: CompartmentId compartment
    , dedicatedVmHostId: maybe Nothing (\t -> Just $ DedicatedVmHostId t) dedicatedVmHost
    , definedTags
    , displayName
    , extendedMetadata
    , faultDomain
    , freeformTags
    , id: InstanceId id
    , imageId: ImageId imageId
    , instanceOptions
    , ipxeScript
    , launchMode
    , launchOptions
    , lifecycleState
    , metadata
    , platformConfig
    , preemptibleInstanceConfig
    , shape: Shape shape
    , shapeConfig
    --, sourceDetails: Nothing
    , timeCreated
    , timeMaintenanceRebootDue
    }

type ListInstancesRequest = BaseRequest
  ( availabilityDomain :: Maybe AvailabilityDomainId
  , capacityReservation :: Maybe CapacityReservationId
  , computeCluster :: Maybe ComputeClusterId
  , compartment :: CompartmentId
  , displayName :: Maybe String
  , lifecycleState :: Maybe InstanceLifecycleState
  )

type ListInstancesResponse =
  { "data" :: List InstanceDescriptionInt
  }

fromListInstancesResponse :: ListInstancesResponse -> F (List InstanceDescription)
fromListInstancesResponse { "data": entries } = ado
  instances <- traverse fromListInstanceDescription entries
  in instances

listInstances :: ListInstancesRequest -> Effect (Either MultipleErrors (List InstanceDescription))
listInstances req@{ compartment, availabilityDomain } = do
  let
    cli = ociCliBase req $ " compute capacity-reservation list"
      <> (" --compartment-id " <> unwrap compartment)
      <> (fromMaybe "" $ (\r -> " --availability-domain " <> r) <$> unwrap <$> availabilityDomain)
  outputJson <- runOciCli cli
  pure $ runExcept $ fromListInstancesResponse =<< readJSON' =<< outputJson

-- Our oci-api doesn't support this yet?
type ListComputeClusterRequest = BaseRequest
  ( compartment :: CompartmentId
  )

type ListCapactityReservationRequest = BaseRequest
  ( compartment :: Maybe CompartmentId
  , availabilityDomain :: Maybe AvailabilityDomainId
  )

type ClusterConfigDetailsInt =
  { "hpc-island-id" :: String
  , "network-block-ids" :: Maybe (List String)
  }

type ClusterConfigDetails =
  { hpcIslandId :: HpcIslandId
  , networkBlockIds :: Maybe (List NetworkBlockId)
  }

fromClusterConfigDetailsInt :: Maybe ClusterConfigDetailsInt -> F (Maybe ClusterConfigDetails)
fromClusterConfigDetailsInt details =
  case details of
    Just
      { "hpc-island-id": hpcIslandId
      , "network-block-ids": networkBlockIds
      } ->
      pure $ Just
        { hpcIslandId: HpcIslandId hpcIslandId
        , networkBlockIds: maybe Nothing (\list -> Just $ map (\t -> NetworkBlockId t) list) networkBlockIds
        }
    Nothing -> pure Nothing

type InstanceReservationShapeDetailsInt =
  { "memory-in-gbs" :: Maybe Number
  , "ocpus" :: Maybe Number
  }

type InstanceReservationShapeDetails =
  { memoryInGbs :: Maybe Number
  , ocpus :: Maybe Number
  }

fromInstanceReservationShapeDetailsInt :: Maybe InstanceReservationShapeDetailsInt -> F (Maybe InstanceReservationShapeDetails)
fromInstanceReservationShapeDetailsInt details =
  case details of
    Just
      { "memory-in-gbs": memoryInGbs
      , "ocpus": ocpus
      } -> pure $ Just
      { memoryInGbs
      , ocpus
      }
    Nothing -> pure Nothing

type InstanceReservationConfigInt =
  { "cluster-config" :: Maybe ClusterConfigDetailsInt
  , "fault-domain" :: Maybe String
  , "instance-shape" :: String
  , "instance-shape-config" :: Maybe InstanceReservationShapeDetailsInt
  , "reserved-count" :: Int
  , "used-count" :: Int
  }

type InstanceReservationConfig =
  { clusterConfig :: Maybe ClusterConfigDetails
  , faultDomain :: Maybe FaultDomainId
  , instanceShape :: Shape
  , instanceShapeConfig :: Maybe InstanceReservationShapeDetails
  , reservedCount :: Int
  , usedCount :: Int
  }

fromInstanceReservationConfigInt :: InstanceReservationConfigInt -> F InstanceReservationConfig
fromInstanceReservationConfigInt
  { "cluster-config": clusterConfigInt
  , "fault-domain": faultDomain
  , "instance-shape": instanceShape
  , "instance-shape-config": instanceShapeConfigInt
  , "reserved-count": reservedCount
  , "used-count": usedCount
  } = ado
  clusterConfig <- fromClusterConfigDetailsInt clusterConfigInt
  instanceShapeConfig <- fromInstanceReservationShapeDetailsInt instanceShapeConfigInt
  in
    { clusterConfig
    , faultDomain: maybe Nothing (\t -> Just $ FaultDomainId t) faultDomain
    , instanceShape: Shape instanceShape
    , instanceShapeConfig
    , reservedCount
    , usedCount
    }

type CapacityReservationInt =
  { "availability-domain" :: String
  , "compartment-id" :: String
  , "defined-tags" :: Maybe (Map String (Map String String))
  , "display-name" :: Maybe String
  , "freeform-tags" :: Maybe (Map String String)
  , "id" :: String
  , "instance-reservation-configs" :: Maybe (List InstanceReservationConfigInt)
  , "is-default-reservation" :: Maybe Boolean
  , "lifecycle-state" :: CapactityReservationLifecycleState
  , "reserved-instance-count" :: Maybe Int
  , "time-created" :: String
  , "time-updated" :: Maybe String
  , "used-instance-count" :: Maybe Int
  }

type CapacityReservation =
  { availabilityDomain :: AvailabilityDomainId
  , compartmentId :: CompartmentId
  , definedTags :: Maybe (Map String (Map String String))
  , displayName :: Maybe String
  , freeformTags :: Maybe (Map String String)
  , id :: CapacityReservationId
  , instanceReservationConfigs :: Maybe (List InstanceReservationConfig)
  , isDefaultReservation :: Maybe Boolean
  , lifecycleState :: CapactityReservationLifecycleState
  , reservedInstanceCount :: Maybe Int
  , timeCreated :: String
  , timeUpdated :: Maybe String
  , usedInstanceCount :: Maybe Int
  }

fromInstanceReservationConfigsInt :: Maybe (List InstanceReservationConfigInt) -> F (Maybe (List InstanceReservationConfig))
fromInstanceReservationConfigsInt configs =
  case configs of
    Just configs' -> ado
      reservationConfigs <- traverse fromInstanceReservationConfigInt configs'
      in Just reservationConfigs
    Nothing -> pure $ Nothing

fromCapactityReservationInt :: CapacityReservationInt -> F CapacityReservation
fromCapactityReservationInt
  { "availability-domain": availabilityDomain
  , "compartment-id": compartmentId
  , "defined-tags": definedTags
  , "display-name": displayName
  , "freeform-tags": freeformTags
  , "id": id
  , "instance-reservation-configs": instanceReservationConfigsInt
  , "is-default-reservation": isDefaultReservation
  , "lifecycle-state": lifecycleState
  , "reserved-instance-count": reservedInstanceCount
  , "time-created": timeCreated
  , "time-updated": timeUpdated
  , "used-instance-count": usedInstanceCount
  } = ado
  instanceReservationConfigs <- fromInstanceReservationConfigsInt instanceReservationConfigsInt
  in
    { availabilityDomain: AvailabilityDomainId availabilityDomain
    , compartmentId: CompartmentId compartmentId
    , definedTags
    , displayName
    , freeformTags
    , id: CapacityReservationId id
    , instanceReservationConfigs
    , isDefaultReservation
    , lifecycleState
    , reservedInstanceCount
    , timeCreated
    , timeUpdated
    , usedInstanceCount
    }

type CapactityReservationsResponse =
  { "data" :: List CapacityReservationInt
  }

fromCapacityReservationResponse :: Maybe CapactityReservationsResponse -> F (List CapacityReservation)
fromCapacityReservationResponse resp =
  case resp of
    Just { "data": entries } -> do
      reservations <- traverse fromCapactityReservationInt $ spy "Entries" entries
      pure $ spy "reservations" reservations
    Nothing -> pure $ List.nil

listCapacityReservations :: ListCapactityReservationRequest -> Effect (Either MultipleErrors (List CapacityReservation))
listCapacityReservations req@{ compartment, availabilityDomain } = do
  let
    cli = ociCliBase req $ " compute capacity-reservation list"
      <> (fromMaybe "" $ (\r -> " --compartment-id " <> r) <$> unwrap <$> compartment)
      <> (fromMaybe "" $ (\r -> " --availability-domain " <> r) <$> unwrap <$> availabilityDomain)
  outputJson <- runOciCli cli
  pure $ runExcept $ fromCapacityReservationResponse =<< readJSON' =<< outputJson

type ListAvailabilityDomainRequest = BaseRequest
  ( compartment :: Maybe CompartmentId
  )

type AvailabilityDomainInt =
  { "compartment-id" :: Maybe String
  , "id" :: Maybe String
  , "name" :: Maybe String
  }

type AvailabilityDomain =
  { compartmentId :: Maybe CompartmentId
  , id :: Maybe AvailabilityDomainId
  , name :: Maybe String
  }

fromAvailabilityDomainInt :: AvailabilityDomainInt -> F AvailabilityDomain
fromAvailabilityDomainInt
  { "compartment-id": compartmentId
  , "id": id
  , "name": name
  } = do
  pure
    { compartmentId: maybe Nothing (\c -> Just $ CompartmentId c) compartmentId
    , id: maybe Nothing (\d -> Just $ AvailabilityDomainId d) id
    , name
    }

type AvailabilityDomainsResponse =
  { "data" :: List AvailabilityDomainInt
  }

fromAvailabilityDomainResponse :: AvailabilityDomainsResponse -> F (List AvailabilityDomain)
fromAvailabilityDomainResponse { "data": entries } = ado
  domains <- traverse fromAvailabilityDomainInt entries
  in domains

listAvailabilityDomains :: ListAvailabilityDomainRequest -> Effect (Either MultipleErrors (List AvailabilityDomain))
listAvailabilityDomains req@{ compartment } = do
  let
    cli = ociCliBase req $ "iam availability-domain list" <>
      (fromMaybe "" $ (\r -> " --compartment-id " <> r) <$> unwrap <$> compartment)
  outputJson <- runOciCli cli
  pure $ runExcept $ fromAvailabilityDomainResponse =<< readJSON' =<< outputJson

type ListImageShapeCompatibilityRequest = BaseRequest
  ( imageId :: ImageId
  )

type ImageMemoryConstraintsInt =
  { "max-in-gbs" :: Maybe Int
  , "min-in-gbs" :: Maybe Int
  }

type ImageMemoryConstraints =
  { maxInGbs :: Maybe Int
  , minInGbs :: Maybe Int
  }

fromImageMemoryConstraintsInt :: Maybe ImageMemoryConstraintsInt -> F (Maybe ImageMemoryConstraints)
fromImageMemoryConstraintsInt constraints =
  case constraints of
    Just { "max-in-gbs": maxInGbs, "min-in-gbs": minInGbs } ->
      pure $ Just { maxInGbs, minInGbs }
    Nothing -> pure Nothing

type ImageOcpuConstraintsInt =
  { "max" :: Maybe Int
  , "min" :: Maybe Int
  }

type ImageOcpuConstraints =
  { max :: Maybe Int
  , min :: Maybe Int
  }

fromImageOcpuConstraintsInt :: Maybe ImageOcpuConstraintsInt -> F (Maybe ImageOcpuConstraints)
fromImageOcpuConstraintsInt constraints =
  case constraints of
    Just { "max": max, "min": min } ->
      pure $ Just { max, min }
    Nothing -> pure Nothing

type ImageShapeCompatibilityInt =
  { "image-id" :: String
  , "memory-constraints" :: Maybe ImageMemoryConstraintsInt
  , "ocpu-constraints" :: Maybe ImageOcpuConstraintsInt
  , "shape" :: String
  }

type ImageShapeCompatibility =
  { imageId :: ImageId
  , memoryConstraints :: Maybe ImageMemoryConstraints
  , ocpuConstraints :: Maybe ImageOcpuConstraints
  , shape :: Shape
  }

fromImageShapeCompatibilityInt :: ImageShapeCompatibilityInt -> F ImageShapeCompatibility
fromImageShapeCompatibilityInt
  { "image-id": imageId
  , "memory-constraints": memoryConstraintsInt
  , "ocpu-constraints": ocpuConstraintsInt
  , "shape": shape
  } = ado
  memoryConstraints <- fromImageMemoryConstraintsInt memoryConstraintsInt
  ocpuConstraints <- fromImageOcpuConstraintsInt ocpuConstraintsInt
  in
    { imageId: ImageId imageId
    , memoryConstraints
    , ocpuConstraints
    , shape: Shape shape
    }

type ImageShapeCompatibilityResponse =
  { "data" :: List ImageShapeCompatibilityInt
  }

fromImageShapeCompatibilityResponse :: ImageShapeCompatibilityResponse -> F (List ImageShapeCompatibility)
fromImageShapeCompatibilityResponse { "data": entries } = ado
  shapes <- traverse fromImageShapeCompatibilityInt entries
  in shapes

listCompatibleShapes :: ListImageShapeCompatibilityRequest -> Effect (Either MultipleErrors (List ImageShapeCompatibility))
listCompatibleShapes req@{ imageId } = do
  let
    cli = ociCliBase req $ "compute image-shape-compatibility-entry list --image-id " <> unwrap imageId
  outputJson <- runOciCli cli
  pure $ runExcept $ fromImageShapeCompatibilityResponse =<< readJSON' =<< outputJson

type ListImagesRequest = BaseRequest (compartment :: Maybe CompartmentId)

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
  , "defined-tags" :: Maybe (Map String (Map String String))
  , "display-name" :: Maybe String
  , "freeform-tags" :: Maybe (Map String String)
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
  , definedTags :: Maybe (Map String (Map String String))
  , displayName :: Maybe String
  , freeformTags :: Maybe (Map String String)
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
    cli = ociCliBase req "compute image list " <>
      (fromMaybe "" $ (\r -> " --compartment-id " <> r) <$> unwrap <$> compartment)
  outputJson <- runOciCli cli
  pure $ runExcept $ fromImagesResponseInt =<< readJSON' =<< outputJson

type ListShapesRequest = BaseRequest (compartment :: Maybe CompartmentId)

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
listShapes req@{ compartment } = do
  let
    cli = ociCliBase req "compute shape list" <>
      (fromMaybe "" $ (\r -> " --compartment-id " <> r) <$> unwrap <$> compartment)
  outputJson <- runOciCli cli
  pure $ runExcept $ fromShapesResponseInt =<< readJSON' =<< outputJson

type ListCompartmentsRequest = BaseRequest (compartment :: Maybe CompartmentId)

type CompartmentDescriptionInt =
  { "compartment-id" :: String
  , "defined-tags" :: Maybe (Map String (Map String String))
  , "description" :: String
  , "freeform-tags" :: Maybe (Map String String)
  , "id" :: String
  , "inactive-status" :: Maybe Int
  , "is-accessible" :: Maybe Boolean
  , "lifecycle-state" :: CompartmentLifecycleState
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
  , lifecycleState :: CompartmentLifecycleState
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
listCompartments req@{ compartment } = do
  let
    cli = ociCliBase req "iam compartment list " <>
      (fromMaybe "" $ (\r -> " --compartment-id " <> r) <$> unwrap <$> compartment)

  outputJson <- runOciCli cli
  pure $ runExcept $ fromCompartmentResponseInt =<< readJSON' =<< outputJson

ociCliBase :: forall t. BaseRequest t -> String -> String
ociCliBase {} command = do
  "oci "
    <> command
    <> " --output json --all "

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
