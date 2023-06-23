module Erl.Oracle
  ( listCompartments
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
import Simple.JSON (class ReadForeign, class WriteForeign, class WriteForeignKey, E, readJSON', writeImpl, writeJSON)
import Text.Parsing.Parser (ParserT, fail, parseErrorMessage, runParser)
import Unsafe.Coerce (unsafeCoerce)

newtype InstanceId = InstanceId String

derive newtype instance Eq InstanceId
derive newtype instance Ord InstanceId
derive newtype instance ReadForeign InstanceId
derive newtype instance WriteForeign InstanceId
derive newtype instance WriteForeignKey InstanceId
derive instance Newtype InstanceId _
derive instance Generic InstanceId _
instance Show InstanceId where
  show = genericShow

newtype InstanceType = InstanceType String

derive newtype instance Eq InstanceType
derive newtype instance Ord InstanceType
derive newtype instance ReadForeign InstanceType
derive newtype instance WriteForeign InstanceType
derive instance Newtype InstanceType _
derive instance Generic InstanceType _
instance Show InstanceType where
  show = genericShow

newtype ImageId = ImageId String

derive newtype instance Eq ImageId
derive newtype instance Ord ImageId
derive newtype instance ReadForeign ImageId
derive newtype instance WriteForeign ImageId
derive instance Newtype ImageId _
derive instance Generic ImageId _
instance Show ImageId where
  show = genericShow

newtype Region = Region String

derive newtype instance Eq Region
derive newtype instance Ord Region
derive newtype instance ReadForeign Region
derive newtype instance WriteForeign Region
derive newtype instance WriteForeignKey Region
derive instance Newtype Region _
derive instance Generic Region _
instance Show Region where
  show = genericShow

newtype Profile = Profile String

derive newtype instance Eq Profile
derive newtype instance Ord Profile
derive newtype instance ReadForeign Profile
derive newtype instance WriteForeign Profile
derive instance Newtype Profile _
derive instance Generic Profile _
instance Show Profile where
  show = genericShow

newtype SecurityGroupId = SecurityGroupId String

derive newtype instance Eq SecurityGroupId
derive newtype instance Ord SecurityGroupId
derive newtype instance ReadForeign SecurityGroupId
derive newtype instance WriteForeign SecurityGroupId
derive instance Newtype SecurityGroupId _
derive instance Generic SecurityGroupId _
instance Show SecurityGroupId where
  show = genericShow

newtype SubnetId = SubnetId String

derive newtype instance Eq SubnetId
derive newtype instance Ord SubnetId
derive newtype instance ReadForeign SubnetId
derive newtype instance WriteForeign SubnetId
derive instance Newtype SubnetId _
derive instance Generic SubnetId _
instance Show SubnetId where
  show = genericShow

newtype KeyName = KeyName String

derive newtype instance Eq KeyName
derive newtype instance Ord KeyName
derive newtype instance ReadForeign KeyName
derive newtype instance WriteForeign KeyName
derive instance Newtype KeyName _
derive instance Generic KeyName _
instance Show KeyName where
  show = genericShow

newtype ClientToken = ClientToken String

derive newtype instance Eq ClientToken
derive newtype instance Ord ClientToken
derive newtype instance ReadForeign ClientToken
derive newtype instance WriteForeign ClientToken
derive instance Newtype ClientToken _
derive instance Generic ClientToken _
instance Show ClientToken where
  show = genericShow

newtype UserData = UserData String

derive newtype instance Eq UserData
derive newtype instance Ord UserData
derive newtype instance ReadForeign UserData
derive newtype instance WriteForeign UserData
derive instance Newtype UserData _
derive instance Generic UserData _
instance Show UserData where
  show = genericShow

type RunningInstance =
  { publicDnsName :: Maybe Hostname
  , privateDnsName :: Hostname
  , privateIpAddress :: IpAddress
  }

data InstanceState
  = Pending
  | Running RunningInstance
  | ShuttingDown
  | Terminated
  | Stopping
  | Stopped

derive instance Eq InstanceState

instance WriteForeign InstanceState where
  writeImpl s =
    let
      instanceData = case s of
        Running i -> Just i
        _ -> Nothing
      state = case s of
        Pending -> "pending"
        Running _ -> "running"
        ShuttingDown -> "shutting-down"
        Terminated -> "terminated"
        Stopping -> "stopping"
        Stopped -> "stopped"
    in
      writeImpl { state, instanceData }

type InstanceDescription =
  { instanceId :: InstanceId
  , instanceType :: InstanceType
  , imageId :: ImageId
  , tags :: Map String String
  , launchTime :: DateTime
  , state :: InstanceState
  , clientToken :: Maybe ClientToken
  }

data OptInStatus
  = OptInNotRequired
  | OptedIn
  | NotOptedIn

derive instance Generic OptInStatus _
instance WriteForeign OptInStatus where
  writeImpl = genericEnumWriteForeign

instance ReadForeign OptInStatus where
  readImpl = genericEnumReadForeign

type RegionDescription =
  { regionName :: Region
  , endpoint :: String
  , optInStatus :: OptInStatus
  }

type TagInt =
  { "Key" :: String
  , "Value" :: String
  }

tagIntsToTags :: List TagInt -> Map String String
tagIntsToTags = foldl insertTag Map.empty
  where
  insertTag acc { "Key": key, "Value": value } = Map.insert key value acc

tagsToTagInts :: Map String String -> List TagInt
tagsToTagInts tags = (\(Tuple key value) -> { "Key": key, "Value": value }) <$> Map.toUnfoldable tags

newtype DateTimeInt = DateTimeInt DateTime

instance readForeignDateTimeInt :: ReadForeign DateTimeInt where
  readImpl =
    readString >=> parseFDT
    where
    parseFDT s =
      except
        $ bimap (singleton <<< ForeignError <<< parseErrorMessage) DateTimeInt (runParser s parseDateTime)

parseDateTime :: forall m. Monad m => ParserT String m DateTime
parseDateTime = parseFullDateTime >>= (\full -> maybe (fail "Invalid datetime offset") (pure) $ toUTC full)

type BaseRequest a =
  { compartment :: Maybe String
  | a
  }

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
  , shape :: String
  }

fromShapesDescriptionInt :: ShapeDescriptionInt -> F ShapeDescription
fromShapesDescriptionInt
  { "availability-domain": availabilityDomain
  , "baseline-ocpu-utilizations": baselineOcpuUtilizations
  , "billing-type": billingType
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
    , shape
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
  --, "defined-tags" :: Maybe DefinedTags
  , "description" :: String
  --, "freeform-tags" :: Maybe FreeformTags
  , "id" :: String
  , "inactive-status" :: Maybe Int
  , "is-accessible" :: Maybe Boolean
  , "lifecycle-state" :: String
  , "name" :: String
  , "time-created" :: String
  }

type CompartmentDescription =
  { compartmentId :: String
  , description :: String
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
  , "description": description
  , "id": id
  , "inactive-status": inactiveStatus
  , "is-accessible": isAccessible
  , "lifecycle-state": lifecycleState
  , "name": name
  , "time-created": timeCreated
  } = do
  pure $
    { compartmentId
    , description
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
    <> (fromMaybe "" $ (\r -> " --compartment-id " <> r) <$> compartment)

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
