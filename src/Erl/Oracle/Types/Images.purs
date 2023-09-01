module Erl.Oracle.Types.Images
  ( ImageDescription
  , ImageLifecycleState(..)
  , InstanceAgentFeatures
  , LaunchOptions
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Erl.Oracle.Types.Common (CompartmentId, DefinedTags, FreeformTags, ImageId)
import Foreign (unsafeFromForeign)
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON (class ReadForeign)

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

type LaunchOptions =
  { bootVolumeType :: Maybe String
  , firmware :: Maybe String
  , isConsistentVolumeNamingEnabled :: Maybe Boolean
  , isPvEncryptionInTransitEnabled :: Maybe Boolean
  , networkType :: Maybe String
  , remoteDataVolumeType :: Maybe String
  }

type InstanceAgentFeatures =
  { isManagementSupported :: Maybe Boolean
  , isMonitoringSupported :: Maybe Boolean
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