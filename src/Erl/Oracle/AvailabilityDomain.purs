module Erl.Oracle.AvailabilityDomain
  ( defaultListAvailabilityDomainRequest
  , listAvailabilityDomains
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect (Effect)
import Erl.Data.List (List)
import Erl.Oracle.Shared (BaseRequest, ociCliBase, runOciCli)
import Erl.Oracle.Types.Common (AvailabilityDomainId(..), CompartmentId(..))
import Erl.Oracle.Types.AvailabilityDomain (AvailabilityDomain)
import Foreign (F, MultipleErrors)
import Simple.JSON (readJSON')

type ListAvailabilityDomainRequest = BaseRequest
  ( compartment :: Maybe CompartmentId
  )

defaultListAvailabilityDomainRequest :: ListAvailabilityDomainRequest
defaultListAvailabilityDomainRequest =
  { compartment: Nothing
  }

-- This returns an id and a name but the name is used in other APIs not the id, so pretend that the
-- name is the id
type AvailabilityDomainInt =
  { "compartment-id" :: Maybe String
  , "id" :: Maybe String
  , "name" :: Maybe String
  }

fromAvailabilityDomainInt :: AvailabilityDomainInt -> F AvailabilityDomain
fromAvailabilityDomainInt
  { "compartment-id": compartmentId
  , "name": name
  } = do
  pure
    { compartmentId: maybe Nothing (\c -> Just $ CompartmentId c) compartmentId
    , id: maybe Nothing (\d -> Just $ AvailabilityDomainId d) name
    }

type AvailabilityDomainsResponse =
  { "data" :: List AvailabilityDomainInt
  }

fromAvailabilityDomainResponse :: AvailabilityDomainsResponse -> F (List AvailabilityDomain)
fromAvailabilityDomainResponse { "data": entries } = ado
  domains <- traverse fromAvailabilityDomainInt entries
  in domains

listAvailabilityDomains :: ListAvailabilityDomainRequest -> Effect (Either MultipleErrors (List AvailabilityDomain))
listAvailabilityDomains req@{ compartment } = do
  let
    cli = ociCliBase req $ "iam availability-domain list"
      <> " --all "
      <> (fromMaybe "" $ (\r -> " --compartment-id " <> r) <$> unwrap <$> compartment)
  outputJson <- runOciCli cli
  pure $ runExcept $ fromAvailabilityDomainResponse =<< readJSON' =<< outputJson
