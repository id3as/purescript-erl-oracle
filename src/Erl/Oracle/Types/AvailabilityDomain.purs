module Erl.Oracle.Types.AvailabilityDomain
  ( AvailabilityDomain
  ) where

import Data.Maybe (Maybe)
import Erl.Oracle.Types.Common (AvailabilityDomainId, CompartmentId)

-- The API returns "name" as a human readable string and "id" as a resource string (eg ocid1.availabilitydomain.oc1..aaaaaaaaw2i3i...)
-- but the rest of the APIs use the name as the ID, not the id because that makes sense
type AvailabilityDomain =
  { compartmentId :: Maybe CompartmentId
  , id :: Maybe AvailabilityDomainId
  }

