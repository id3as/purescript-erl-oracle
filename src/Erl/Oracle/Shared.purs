module Erl.Oracle.Shared
  ( BaseRequest
  , escapeJson
  , ociCliBase
  , ociCliBase'
  , runOciCli
  ) where

import Prelude

import Control.Monad.Except (except)
import Data.Either (Either(..))
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.String (replaceAll, Pattern(..), Replacement(..))
import Effect (Effect)
import Erl.Oracle.Types.Common (CompartmentId, OciProfile)
import Foreign (F, ForeignError(..))

type BaseRequest a =
  { profile :: OciProfile
  , compartment :: CompartmentId
  | a
  }

ociCliBase :: forall a. BaseRequest a -> String -> String
ociCliBase req@{ compartment } command = do
  ociCliBase' req command
    <> (" --compartment-id " <> (unwrap compartment))

ociCliBase' :: forall a. BaseRequest a -> String -> String
ociCliBase' { profile: { ociProfileName, configFile } } command = do
  "oci "
    <> (" --config-file " <> configFile)
    <> (" --profile " <> ociProfileName)
    <> " "
    <> command
    <> " --output json "

runOciCli :: String -> Effect (F String)
runOciCli cmd = do
  res <- runCommand $ cmd
  case res of
    Left { output } -> pure $ except $ Left $ singleton $ ForeignError $ "oci cli failure: " <> output
    Right output -> pure $ except $ Right output

escapeJson :: String -> String
escapeJson = replaceAll (Pattern "\"") (Replacement "\\\"")

type CmdError =
  { exitStatus :: Number
  , output :: String
  }

foreign import runCommand :: String -> Effect (Either CmdError String)

foreign import base64Decode :: String -> Maybe String
