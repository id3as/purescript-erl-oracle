module Erl.Oracle.Types.Secrets
  ( SecretBundleContent
  , SecretDescription
  , SecretSummary
  , VaultDescription
  ) where

import Data.Maybe (Maybe)
import Erl.Oracle.Types.Common (DefinedTags, FreeformTags, Metadata, SecretId, VaultId)

type SecretBundleContent =
  { content :: String
  , contentType :: String
  }

type SecretDescription =
  { metadata :: Maybe Metadata
  , secretBundleContent :: SecretBundleContent
  , id :: SecretId
  }

type SecretSummary =
  { id :: SecretId
  , secretName :: Maybe String
  , definedTags :: Maybe DefinedTags
  , freeformTags :: Maybe FreeformTags
  }

type VaultDescription =
  { id :: VaultId
  , definedTags :: Maybe DefinedTags
  , displayName :: Maybe String
  , freeformTags :: Maybe FreeformTags
  }

