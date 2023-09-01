module Erl.Oracle.Types.Compartments
  ( CompartmentDescription
  , CompartmentLifecycleState(..)
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Erl.Oracle.Types.Common (CompartmentId, DefinedTags, FreeformTags)
import Foreign (unsafeFromForeign)
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON (class ReadForeign)

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
