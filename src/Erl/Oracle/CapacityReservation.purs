module Erl.Oracle.CapacityReservation
  ( ListCapactityReservationRequest
  , defaultListCapacityReservationRequest
  , listCapacityReservations
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect (Effect)
import Erl.Data.List (List)
import Erl.Data.List as List
import Erl.Data.Map (Map)
import Erl.Oracle.Shared (BaseRequest, ociCliBase, runOciCli)
import Erl.Oracle.Types.Common (AvailabilityDomainId(..), CapacityReservationId(..), CompartmentId(..), FaultDomainId(..), HpcIslandId(..), NetworkBlockId(..), Shape(..), OciProfile)
import Erl.Oracle.Types.CapacityReservation (CapacityReservation, CapacityReservationLifecycleState, ClusterConfigDetails, InstanceReservationShapeDetails, InstanceReservationConfig)
import Foreign (F, MultipleErrors)
import Simple.JSON (readJSON')

type ClusterConfigDetailsInt =
  { "hpc-island-id" :: String
  , "network-block-ids" :: Maybe (List String)
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
  , "lifecycle-state" :: CapacityReservationLifecycleState
  , "reserved-instance-count" :: Maybe Int
  , "time-created" :: String
  , "time-updated" :: Maybe String
  , "used-instance-count" :: Maybe Int
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
      reservations <- traverse fromCapactityReservationInt entries
      pure reservations
    Nothing -> pure $ List.nil

type ListCapactityReservationRequest = BaseRequest
  ( availabilityDomain :: Maybe AvailabilityDomainId
  )

defaultListCapacityReservationRequest :: OciProfile -> Maybe CompartmentId -> ListCapactityReservationRequest
defaultListCapacityReservationRequest profile@{ defaultCompartment } compartment =
  { compartment: fromMaybe defaultCompartment compartment
  , availabilityDomain: Nothing
  , profile
  }

listCapacityReservations :: ListCapactityReservationRequest -> Effect (Either MultipleErrors (List CapacityReservation))
listCapacityReservations req@{ availabilityDomain } = do
  let
    cli = ociCliBase req $ " compute capacity-reservation list"
      <> " --all "
      <> (fromMaybe "" $ (\r -> " --availability-domain " <> r) <$> unwrap <$> availabilityDomain)
  outputJson <- runOciCli cli
  pure $ runExcept $ fromCapacityReservationResponse =<< readJSON' =<< outputJson

