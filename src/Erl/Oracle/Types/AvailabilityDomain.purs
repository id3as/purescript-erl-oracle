module Erl.Oracle.Types.AvailabilityDomain
  ( AvailabilityDomain
  ) where

import Data.Maybe (Maybe)
import Erl.Oracle.Types.Common (AvailabilityDomainId, CompartmentId)

type AvailabilityDomain =
  { compartmentId :: Maybe CompartmentId
  , id :: Maybe AvailabilityDomainId
  }

