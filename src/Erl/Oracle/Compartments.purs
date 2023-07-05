module Erl.Oracle.Compartments
  ( CompartmentDescription
  , CompartmentLifecycleState(..)
  , ListCompartmentsRequest
  , defaultListCompartmentsRequest
  , listCompartments
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Effect (Effect)
import Erl.Data.List (List)
import Erl.Oracle.Shared (BaseRequest, ociCliBase, runOciCli)
import Erl.Oracle.Types (CompartmentId(..), DefinedTags, FreeformTags)
import Foreign (F, MultipleErrors, unsafeFromForeign)
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON (class ReadForeign, readJSON')

data CompartmentLifecycleState
  = Active
  | Creating
  | Inactive
  | Deleted
  | Deleting

derive instance Eq CompartmentLifecycleState
derive instance Generic CompartmentLifecycleState _
instance ReadForeign CompartmentLifecycleState where
  readImpl f =
    case unsafeFromForeign f of
      "ACTIVE" -> pure Active
      "CREATING" -> pure Creating
      "INACTIVE" -> pure Inactive
      "DELETED" -> pure Deleted
      "DELETING" -> pure Deleting
      somethingElse -> unsafeCrashWith $ "Unexpected CompartmentLifecycleState " <> somethingElse

instance Show CompartmentLifecycleState where
  show = genericShow

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

type CompartmentDescription =
  { compartmentId :: CompartmentId
  , definedTags :: Maybe DefinedTags
  , description :: String
  , freeformTags :: Maybe FreeformTags
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
    cli = ociCliBase req "iam compartment list "
      <> " --all "
      <> (fromMaybe "" $ (\r -> " --compartment-id " <> r) <$> unwrap <$> compartment)

  outputJson <- runOciCli cli
  pure $ runExcept $ fromCompartmentResponseInt =<< readJSON' =<< outputJson
