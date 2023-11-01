module Erl.Oracle.Secrets
  ( GetSecretRequest
  , ListSecretsRequest
  , ListVaultsRequest
  , defaultGetSecretRequest
  , defaultListSecretsRequest
  , defaultListVaultsRequest
  , getSecret
  , listSecrets
  , listVaults
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Debug (spy, trace)
import Effect (Effect)
import Erl.Data.List (List)
import Erl.Oracle.Shared (BaseRequest, ociCliBase, ociCliBase', runOciCli)
import Erl.Oracle.Types.Common (CompartmentId, DefinedTags, FreeformTags, OciProfile, SecretId(..), VaultId, Metadata)
import Erl.Oracle.Types.Secrets (SecretBundleContent, SecretSummary, VaultDescription, SecretDescription)
import Foreign (MultipleErrors, F)
import Simple.JSON (readJSON')

type ListVaultsRequest = BaseRequest ()

defaultListVaultsRequest :: OciProfile -> Maybe CompartmentId -> ListVaultsRequest
defaultListVaultsRequest profile@{ defaultCompartment } compartment =
  { compartment: fromMaybe defaultCompartment compartment
  , profile
  }

type ListSecretsRequest = BaseRequest ()

defaultListSecretsRequest :: OciProfile -> Maybe CompartmentId -> ListSecretsRequest
defaultListSecretsRequest profile@{ defaultCompartment } compartment =
  { compartment: fromMaybe defaultCompartment compartment
  , profile
  }

type GetSecretRequest = BaseRequest (secretId :: SecretId)

defaultGetSecretRequest :: OciProfile -> Maybe CompartmentId -> SecretId -> GetSecretRequest
defaultGetSecretRequest profile@{ defaultCompartment } compartment secretId =
  { compartment: fromMaybe defaultCompartment compartment
  , profile
  , secretId
  }

type SecretSummaryInt =
  { "id" :: String
  , "secret-name" :: Maybe String
  , "defined-tags" :: Maybe DefinedTags
  , "freeform-tags" :: Maybe FreeformTags
  }

type SecretSummariesInt =
  { "data" :: List SecretSummaryInt
  }

fromSecretSummaryInt :: SecretSummaryInt -> F SecretSummary
fromSecretSummaryInt
  { "id": id
  , "defined-tags": definedTags
  , "secret-name": secretName
  , "freeform-tags": freeformTags
  } = do
  pure $
    { id: SecretId id
    , definedTags
    , secretName
    , freeformTags
    }

fromSecretSummariesInt :: SecretSummariesInt -> F (List SecretSummary)
fromSecretSummariesInt { "data": secretSummaries } = ado
  secrets <- traverse fromSecretSummaryInt secretSummaries
  in secrets

listSecrets :: ListSecretsRequest -> Effect (Either MultipleErrors (List SecretSummary))
listSecrets req = do
  let
    cli = ociCliBase req "vault secret list"
      <> " --all "
  outputJson <- runOciCli cli
  pure $ runExcept $ fromSecretSummariesInt =<< readJSON' =<< outputJson

type SecretInt =
  { "secret-id" :: SecretId
  , "metadata" :: Maybe Metadata
  , "secret-bundle-content" :: SecretBundleContentInt
  }

type SecretResponseInt =
  { "data" :: SecretInt
  }

type SecretBundleContentInt =
  { "content" :: String
  , "content-type" :: String
  }

fromSecretBundleContent :: SecretBundleContentInt -> F SecretBundleContent
fromSecretBundleContent
  { "content-type": contentType
  , "content": content
  } = do
  pure $ { contentType, content }

fromSecretInt :: SecretResponseInt -> F SecretDescription
fromSecretInt
  { "data":
      { "secret-id": id
      , "metadata": metadata
      , "secret-bundle-content": secretBundleContentInt
      }
  } = ado
  trace ("Got secret with id " <> show id) \_ -> pure unit

  secretBundleContent <- fromSecretBundleContent secretBundleContentInt
  in
    { id
    , metadata
    , secretBundleContent
    }

getSecret :: GetSecretRequest -> Effect (Either MultipleErrors SecretDescription)
getSecret req@{ secretId } = do
  let
    cli = ociCliBase' req "secrets secret-bundle get"
      <> (" --secret-id " <> unwrap secretId)
  outputJson <- runOciCli cli
  pure $ runExcept $ fromSecretInt =<< readJSON' =<< outputJson

type VaultDescriptionInt =
  { "id" :: VaultId
  , "defined-tags" :: Maybe DefinedTags
  , "display-name" :: Maybe String
  , "freeform-tags" :: Maybe FreeformTags
  }

type VaultDescriptionsInt =
  { "data" :: List VaultDescriptionInt
  }

fromVaultDescriptionInt :: VaultDescriptionInt -> F VaultDescription
fromVaultDescriptionInt
  { "id": id
  , "defined-tags": definedTags
  , "display-name": displayName
  , "freeform-tags": freeformTags
  } = do
  pure $ { id, definedTags, displayName, freeformTags }

fromListVaultsResponse :: VaultDescriptionsInt -> F (List VaultDescription)
fromListVaultsResponse { "data": vaultData } = ado
  vaults <- traverse fromVaultDescriptionInt vaultData
  in vaults

listVaults :: ListVaultsRequest -> Effect (Either MultipleErrors (List VaultDescription))
listVaults req = do
  let
    cli = ociCliBase req "kms management vault list"
      <> " --all "
  outputJson <- runOciCli cli
  pure $ runExcept $ fromListVaultsResponse =<< readJSON' =<< outputJson