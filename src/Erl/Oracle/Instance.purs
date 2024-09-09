module Erl.Oracle.Instance
  ( defaultLaunchInstanceRequest
  , defaultListInstancesRequest
  , defaultStopInstanceRequest
  , defaultTerminateInstanceRequest
  , launchInstance
  , listInstances
  , stopInstance
  , terminateInstance
  ) where

import Prelude

import Control.Monad.Except (ExceptT, runExcept)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect (Effect)
import Erl.Data.Binary.IOData (fromString)
import Erl.Data.List (List)
import Erl.Kernel.File (fileToString, writeFile)
import Erl.Oracle.Shared (BaseRequest, ociCliBase, ociCliBase', runOciCli)
import Erl.Oracle.Types.Common (AvailabilityDomainId(..), CapacityReservationId(..), CompartmentId(..), ComputeClusterId, DedicatedVmHostId(..), DefinedTags, FreeformTags, ImageId(..), InstanceId(..), LaunchMode, Metadata, OciProfile, Shape(..), SubnetId, ExtendedMetadata)
import Erl.Oracle.Types.Images (LaunchOptions)
import Erl.Oracle.Types.Instance (InstanceAgentConfig, InstanceAgentPluginConfigDetails, InstanceAvailabilityConfig, InstanceLifecycleState, InstanceOptions, InstancePlatformConfig, InstanceShapeConfig, LaunchInstanceRequest, PreemptibleInstanceConfig, PreemptionAction, InstanceDescription)
import Erl.Stdlib.FileLib (mkTempDir)
import Erl.Types (SandboxedFile)
import Foreign (F, ForeignError, MultipleErrors)
import Partial.Unsafe (unsafeCrashWith)
import Pathy (Abs, File, Path, file, rootDir, sandbox, (</>))
import Simple.JSON (readJSON', writeJSON)
import Type.Prelude (Proxy(..))

type ListInstancesRequest = BaseRequest
  ( availabilityDomain :: Maybe AvailabilityDomainId
  , capacityReservation :: Maybe CapacityReservationId
  , computeCluster :: Maybe ComputeClusterId
  , displayName :: Maybe String
  , lifecycleState :: Maybe InstanceLifecycleState
  )

defaultListInstancesRequest :: OciProfile -> Maybe CompartmentId -> ListInstancesRequest
defaultListInstancesRequest profile@{ defaultCompartment } compartment =
  { availabilityDomain: Nothing
  , capacityReservation: Nothing
  , computeCluster: Nothing
  , compartment: fromMaybe defaultCompartment compartment
  , displayName: Nothing
  , lifecycleState: Nothing
  , profile
  }

type InstanceAgentPluginConfigDetailsInt =
  { "desired-state" :: String
  , "name" :: String
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
fromInstanceAgentPluginConfigDetailsInt =
  case _ of
    Just configs -> ado
      pluginConfigs <- traverse fromInstanceAgentPluginConfigDetailInt configs
      in Just pluginConfigs
    Nothing -> pure $ Nothing

type InstanceAgentConfigInt =
  { "are-all-plugins-disabled" :: Maybe Boolean
  , "is-management-disabled" :: Maybe Boolean
  , "is-monitoring-disabled" :: Maybe Boolean
  , "plugins-config" :: Maybe (List InstanceAgentPluginConfigDetailsInt)
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

fromInstanceOptionsInt :: Maybe InstanceOptionsInt -> F (Maybe InstanceOptions)
fromInstanceOptionsInt opts =
  case opts of
    Just { "are-legacy-lmds-endpoints-disabled": areLegacyLmdsEndpointsDisabled } ->
      do pure $ Just { areLegacyLmdsEndpointsDisabled }
    Nothing -> pure Nothing

type LaunchOptionsInt =
  { "boot-volume-type" :: Maybe String
  , "firmware" :: Maybe String
  , "is-consistent-volume-naming-enabled" :: Maybe Boolean
  , "is-pv-encryption-in-transit-enabled" :: Maybe Boolean
  , "network-type" :: Maybe String
  , "remote-data-volume-type" :: Maybe String
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

type InstancePlatformConfigInt =
  { "is-measured-boot-enabled" :: Maybe Boolean
  , "is-secure-boot-enabled" :: Maybe Boolean
  , "is-trusted-platform-enabled" :: Maybe Boolean
  , "type" :: String
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

type PreemptionActionInt =
  { "type" :: String
  }

fromPreemptionActionInt :: PreemptionActionInt -> F PreemptionAction
fromPreemptionActionInt { "type": actiontype } = do pure { type: actiontype }

type PreemptibleInstanceConfigInt =
  { "preemption-action" :: PreemptionActionInt
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

type InstanceDescriptionInt =
  { "agent-config" :: Maybe InstanceAgentConfigInt
  , "availability-config" :: Maybe InstanceAvailabilityConfigInt
  , "availability-domain" :: String
  , "capacity-reservation-id" :: Maybe String
  , "compartment-id" :: String
  , "dedicated-vm-host-id" :: Maybe String
  , "defined-tags" :: Maybe DefinedTags
  , "display-name" :: Maybe String
  , "extended-metadata" :: Maybe ExtendedMetadata
  , "fault-domain" :: Maybe String
  , "freeform-tags" :: Maybe FreeformTags
  , "id" :: String
  , "image-id" :: String
  , "instance=options" :: Maybe InstanceOptionsInt
  , "ipxe-script" :: Maybe String
  , "launch-mode" :: Maybe LaunchMode
  , "launch-options" :: Maybe LaunchOptionsInt
  , "lifecycle-state" :: InstanceLifecycleState
  , "metadata" :: Maybe Metadata
  , "platform-config" :: Maybe InstancePlatformConfigInt
  , "preemptible-instance-config" :: Maybe PreemptibleInstanceConfigInt
  , "shape" :: String
  , "shape-config" :: Maybe InstanceShapeConfigInt
  , "time-created" :: String
  , "time-maintenance-reboot-due" :: Maybe String
  }

fromInstanceDescription :: InstanceDescriptionInt -> F InstanceDescription
fromInstanceDescription
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

type ListInstancesResponse =
  { "data" :: List InstanceDescriptionInt
  }

fromListInstancesResponse :: ListInstancesResponse -> F (List InstanceDescription)
fromListInstancesResponse { "data": entries } = ado
  instances <- traverse fromInstanceDescription entries
  in instances

listInstances :: ListInstancesRequest -> Effect (Either MultipleErrors (List InstanceDescription))
listInstances req@{ availabilityDomain } = do
  let
    cli = ociCliBase req $ " compute instance list"
      <> " --all "
      <> (fromMaybe "" $ (\r -> " --availability-domain " <> r) <$> unwrap <$> availabilityDomain)
  outputJson <- runOciCli cli
  pure $ runExcept $ fromListInstancesResponse =<< readJSON' =<< outputJson

defaultLaunchInstanceRequest :: OciProfile -> Maybe CompartmentId -> AvailabilityDomainId -> Shape -> SubnetId -> LaunchInstanceRequest
defaultLaunchInstanceRequest profile@{ defaultCompartment } compartment availabilityDomain shape subnet =
  { availabilityDomain
  , compartment: fromMaybe defaultCompartment compartment
  , shape
  , subnet
  , capacityReservation: Nothing
  , dedicatedVmHost: Nothing
  , definedTags: Nothing
  , displayName: Nothing
  , extendedMetadata: Nothing
  , faultDomain: Nothing
  , freeformTags: Nothing
  , hostname: Nothing
  , imageId: Nothing
  , launchOptions: Nothing
  , instanceOptions: Nothing
  , availabilityConfig: Nothing
  , preemptibleInstanceConfig: Nothing
  , metadata: Nothing
  , agentConfig: Nothing
  , shapeConfig: Nothing
  , isPvEncryptionInTransitEnabled: Nothing
  , ipxeScript: Nothing
  , platformConfig: Nothing
  , vnicDisplayName: Nothing
  , nsgIds: Nothing
  , assignPublicIp: Nothing
  , privateIp: Nothing
  , skipSourceDestCheck: Nothing
  , userData: Nothing
  , sshAuthorizedKeys: Nothing
  , sourceBootVolume: Nothing
  , bootVolumeSizeInGbps: Nothing
  , profile
  }

type LaunchInstanceResponse =
  { "data" :: InstanceDescriptionInt
  }

fromLaunchInstanceResponse :: LaunchInstanceResponse -> F InstanceDescription
fromLaunchInstanceResponse { "data": resp } = fromInstanceDescription resp

launchInstance :: LaunchInstanceRequest -> Effect (Either MultipleErrors InstanceDescription)
launchInstance
  req@
    { availabilityDomain
    , shape
    , capacityReservation
    , dedicatedVmHost
    , definedTags
    , displayName
    , extendedMetadata
    , faultDomain
    , freeformTags
    , hostname
    , imageId
    , launchOptions
    , instanceOptions
    , availabilityConfig
    , preemptibleInstanceConfig
    , metadata
    , agentConfig
    , shapeConfig
    , isPvEncryptionInTransitEnabled
    , ipxeScript
    , platformConfig
    , vnicDisplayName
    , nsgIds
    , assignPublicIp
    , privateIp
    , skipSourceDestCheck
    , userData
    , sshAuthorizedKeys
    , sourceBootVolume
    , subnet
    , bootVolumeSizeInGbps
    } = do
  tempDir <- mkTempDir
  let
    writeIpxeFile :: Maybe String -> Effect String
    writeIpxeFile = do
      case _ of
        Just script' -> do
          let
            ipxeFile = unsafeSandbox $ tempDir </> file (Proxy :: _ "ipxe")
          void $ writeFile ipxeFile $ fromString script'
          pure $ " --ipxe-script-file " <> fileToString ipxeFile
        Nothing -> pure ""

    writeUserDataFile :: Maybe String -> Effect String
    writeUserDataFile = do
      case _ of
        Just d -> do
          let
            userDataFile = unsafeSandbox $ tempDir </> file (Proxy :: _ "userData")
          void $ writeFile userDataFile $ fromString d
          pure $ " --user-data-file " <> fileToString userDataFile
        Nothing -> pure ""

    writeMetadataFile :: Maybe Metadata -> Effect String
    writeMetadataFile = do
      case _ of
        Just d -> do
          let
            metadataFile = unsafeSandbox $ tempDir </> file (Proxy :: _ "metadata")
          void $ writeFile metadataFile $ fromString $ writeJSON d
          pure $ (" --metadata file://" <> fileToString metadataFile)
        Nothing -> pure ""

    writeExtendedMetadataFile :: Maybe ExtendedMetadata -> Effect String
    writeExtendedMetadataFile = do
      case _ of
        Just d -> do
          let
            metadataFile = unsafeSandbox $ tempDir </> file (Proxy :: _ "extendedmetadata")
          void $ writeFile metadataFile $ fromString $ writeJSON d
          pure $ (" --extended-metadata file://" <> fileToString metadataFile)
        Nothing -> pure ""

    writeSshKeyFile :: Maybe String -> Effect String
    writeSshKeyFile = do
      case _ of
        Just sshKeys -> do
          let
            sshKeyFile = unsafeSandbox $ tempDir </> file (Proxy :: _ "sshKeys")
          void $ writeFile sshKeyFile $ fromString sshKeys
          pure $ " --ssh-authorized-keys-file " <> fileToString sshKeyFile
        Nothing -> pure ""

  ipxeFile' <- writeIpxeFile $ ipxeScript
  userDataFile' <- writeUserDataFile $ userData
  sshKeyFile' <- writeSshKeyFile $ sshAuthorizedKeys
  writeMetadataFile' <- writeMetadataFile metadata
  writeExtendedMetadataFile' <- writeExtendedMetadataFile extendedMetadata

  let
    cli :: String
    cli = ociCliBase req $ "compute instance launch "
      <> (" --availability-domain " <> unwrap availabilityDomain)
      <> (" --shape " <> unwrap shape)
      <> (" --subnet-id " <> unwrap subnet)
      <> (fromMaybe "" $ (\r -> " --capacity-reservation-id " <> r) <$> unwrap <$> capacityReservation)
      <> (fromMaybe "" $ (\r -> " --dedicated-vm-host-id " <> r) <$> unwrap <$> dedicatedVmHost)
      <> (fromMaybe "" $ (\r -> " --display-name '" <> r <> "'") <$> displayName)
      <> (fromMaybe "" $ (\r -> " --fault-domain " <> r) <$> unwrap <$> faultDomain)
      <> (fromMaybe "" $ (\r -> " --hostname-label " <> r) <$> hostname)
      <> (fromMaybe "" $ (\r -> " --image-id " <> r) <$> unwrap <$> imageId)
      <> (fromMaybe "" $ (\r -> " --source-boot-volume-id " <> r) <$> unwrap <$> sourceBootVolume)
      <> (fromMaybe "" $ (\r -> " --boot-volume-size-in-gbs " <> r) <$> show <$> bootVolumeSizeInGbps)
      <> (fromMaybe "" $ (\r -> " --vnic-display-name  " <> r) <$> vnicDisplayName)
      <> (fromMaybe "" $ (\r -> " --is-pv-encryption-in-transit-enabled " <> r) <$> show <$> isPvEncryptionInTransitEnabled)
      <> (fromMaybe "" $ (\r -> " --private-ip " <> r) <$> privateIp)
      <> (fromMaybe "" $ (\r -> " --assign-public-ip " <> r) <$> show <$> assignPublicIp)
      <> (fromMaybe "" $ (\r -> " --skip-source-dest-check " <> r) <$> show <$> skipSourceDestCheck)
      <> (fromMaybe "" $ (\r -> " --defined-tags '" <> r <> "'") <$> writeJSON <$> definedTags)
      <> (fromMaybe "" $ (\r -> " --freeform-tags '" <> r <> "'") <$> writeJSON <$> freeformTags)
      -- <> (fromMaybe "" $ (\r -> " --metadata '" <> r <> "'") <$> spy "Write metadata" writeJSON <$> metadata)
      -- <> (fromMaybe "" $ (\r -> " --extended-metadata '" <> r <> "'") <$> writeJSON <$> extendedMetadata)
      <> ipxeFile'
      <> userDataFile'
      <> sshKeyFile'
      <> writeMetadataFile'
      <> writeExtendedMetadataFile'
      <> (fromMaybe "" $ (\r -> " --launch-options '" <> r <> "'") <$> writeJSON <$> launchOptions)
      <> (fromMaybe "" $ (\r -> " --instance-options '" <> r <> "'") <$> writeJSON <$> instanceOptions)
      <> (fromMaybe "" $ (\r -> " --availability-config '" <> r <> "'") <$> writeJSON <$> availabilityConfig)
      <> (fromMaybe "" $ (\r -> " --preemptible-instance-config '" <> r <> "'") <$> writeJSON <$> preemptibleInstanceConfig)
      <> (fromMaybe "" $ (\r -> " --agent-config '" <> r <> "'") <$> writeJSON <$> agentConfig)
      <> (fromMaybe "" $ (\r -> " --shape-config '" <> r <> "'") <$> writeJSON <$> shapeConfig)
      <> (fromMaybe "" $ (\r -> " --platform-config '" <> r <> "'") <$> writeJSON <$> platformConfig)
      <> (fromMaybe "" $ (\r -> " --nsg-ids '" <> r <> "'") <$> writeJSON <$> nsgIds)

  (outputJson :: ExceptT (NonEmptyList ForeignError) Identity String) <- runOciCli cli

  pure $ runExcept $ fromLaunchInstanceResponse =<< readJSON' =<< outputJson

type TerminateInstanceRequest = BaseRequest
  ( instanceId :: InstanceId
  )

defaultTerminateInstanceRequest :: OciProfile -> Maybe CompartmentId -> InstanceId -> TerminateInstanceRequest
defaultTerminateInstanceRequest profile@{ defaultCompartment } compartment instanceId =
  { instanceId
  , profile
  , compartment: fromMaybe defaultCompartment compartment
  }

type TerminateInstanceResponse =
  { "data" :: List String
  }

fromTerminateInstanceResponse :: TerminateInstanceResponse -> F Boolean
fromTerminateInstanceResponse { "data": _resp } = pure true

terminateInstance :: TerminateInstanceRequest -> Effect (Either MultipleErrors Boolean)
terminateInstance req@{ instanceId } = do
  let
    cli = ociCliBase' req "compute instance terminate --force " <>
      (" --instance-id " <> unwrap instanceId)
  outputJson <- runOciCli cli
  pure $ runExcept $ fromTerminateInstanceResponse =<< readJSON' =<< outputJson

type StopInstanceRequest = BaseRequest
  ( instanceId :: InstanceId
  )

defaultStopInstanceRequest :: OciProfile -> Maybe CompartmentId -> InstanceId -> StopInstanceRequest
defaultStopInstanceRequest profile@{ defaultCompartment } compartment instanceId =
  { instanceId
  , profile
  , compartment: fromMaybe defaultCompartment compartment
  }

type StopInstanceResponse =
  { "data" :: InstanceDescriptionInt
  }

fromStopInstanceResponse :: StopInstanceResponse -> F InstanceDescription
fromStopInstanceResponse { "data": resp } = fromInstanceDescription resp

stopInstance :: StopInstanceRequest -> Effect (Either MultipleErrors InstanceDescription)
stopInstance req@{ instanceId } = do
  let
    cli = ociCliBase' req "compute instance action --action SOFTSTOP " <>
      (" --instance-id " <> unwrap instanceId)
  outputJson <- runOciCli cli
  pure $ runExcept $ fromStopInstanceResponse =<< readJSON' =<< outputJson

unsafeSandbox :: Path Abs File -> SandboxedFile
unsafeSandbox path =
  case sandbox rootDir path of
    Just val -> Left val
    Nothing -> unsafeCrashWith "sandbox to root failed"

