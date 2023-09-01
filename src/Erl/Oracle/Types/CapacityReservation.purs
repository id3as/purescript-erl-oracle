module Erl.Oracle.Types.CapacityReservation
  ( CapacityReservation
  , CapacityReservationLifecycleState(..)
  , ClusterConfigDetails
  , InstanceReservationConfig
  , InstanceReservationShapeDetails
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Erl.Data.List (List)
import Erl.Data.Map (Map)
import Erl.Oracle.Types.Common (AvailabilityDomainId, CapacityReservationId, CompartmentId, FaultDomainId, HpcIslandId, NetworkBlockId, Shape)
import Foreign (unsafeFromForeign)
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON (class ReadForeign)

data CapacityReservationLifecycleState
  = Active
  | Creating
  | Updating
  | Moving
  | Deleted
  | Deleting

derive instance Eq CapacityReservationLifecycleState
derive instance Generic CapacityReservationLifecycleState _
instance ReadForeign CapacityReservationLifecycleState where
  readImpl f =
    case unsafeFromForeign f of
      "ACTIVE" -> pure Active
      "CREATING" -> pure Creating
      "UPDATING" -> pure Updating
      "MOVING" -> pure Moving
      "DELETED" -> pure Deleted
      "DELETING" -> pure Deleting
      somethingElse -> unsafeCrashWith $ "Unexpected CapacityReservationLifecycleState " <> somethingElse

instance Show CapacityReservationLifecycleState where
  show = genericShow

type ClusterConfigDetails =
  { hpcIslandId :: HpcIslandId
  , networkBlockIds :: Maybe (List NetworkBlockId)
  }

type InstanceReservationShapeDetails =
  { memoryInGbs :: Maybe Number
  , ocpus :: Maybe Number
  }

type InstanceReservationConfig =
  { clusterConfig :: Maybe ClusterConfigDetails
  , faultDomain :: Maybe FaultDomainId
  , instanceShape :: Shape
  , instanceShapeConfig :: Maybe InstanceReservationShapeDetails
  , reservedCount :: Int
  , usedCount :: Int
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
  , lifecycleState :: CapacityReservationLifecycleState
  , reservedInstanceCount :: Maybe Int
  , timeCreated :: String
  , timeUpdated :: Maybe String
  , usedInstanceCount :: Maybe Int
  }
