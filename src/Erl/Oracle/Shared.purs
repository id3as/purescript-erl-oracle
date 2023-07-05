module Erl.Oracle.Shared
  ( BaseRequest
  , escapeJson
  , ociCliBase
  , runOciCli
  ) where

import Prelude

import Control.Monad.Except (except)
import Data.Either (Either(..))
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe)
import Data.String (replaceAll, Pattern(..), Replacement(..))
import Effect (Effect)
import Foreign (F, ForeignError(..))

type BaseRequest a =
  {
  | a
  }

ociCliBase :: forall a. BaseRequest a -> String -> String
ociCliBase _req command = do
  "oci "
    <> command
    --    <> (if requestAll == true then " --all " else "")
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
