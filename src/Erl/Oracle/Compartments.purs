module Erl.Oracle.Compartments
  ( ListCompartmentsRequest
  , defaultListCompartmentsRequest
  , listCompartments
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect (Effect)
import Erl.Data.List (List)
import Erl.Oracle.Shared (BaseRequest, ociCliBase, runOciCli)
import Erl.Oracle.Types.Common (CompartmentId(..), DefinedTags, FreeformTags)
import Erl.Oracle.Types.Compartments (CompartmentLifecycleState, CompartmentDescription)
import Foreign (F, MultipleErrors)
import Simple.JSON (readJSON')

type ListCompartmentsRequest = BaseRequest (compartment :: Maybe CompartmentId)

defaultListCompartmentsRequest :: ListCompartmentsRequest
defaultListCompartmentsRequest =
  { compartment: Nothing
  }

type CompartmentDescriptionInt =
  { "compartment-id" :: String
  , "defined-tags" :: Maybe DefinedTags
  , "description" :: String
  , "freeform-tags" :: Maybe FreeformTags
  , "id" :: String
  , "inactive-status" :: Maybe Int
  , "is-accessible" :: Maybe Boolean
  , "lifecycle-state" :: CompartmentLifecycleState
  , "name" :: String
  , "time-created" :: String
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
    cli = ociCliBase req "iam compartment list "
      <> " --all "
      <> (fromMaybe "" $ (\r -> " --compartment-id " <> r) <$> unwrap <$> compartment)

  outputJson <- runOciCli cli
  pure $ runExcept $ fromCompartmentResponseInt =<< readJSON' =<< outputJson
