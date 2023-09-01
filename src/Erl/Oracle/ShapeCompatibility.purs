module Erl.Oracle.ShapeCompatibility
  ( ListImageShapeCompatibilityRequest
  , defaultListImageShapeCompatibilityRequest
  , listCompatibleShapes
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect (Effect)
import Erl.Data.List (List)
import Erl.Oracle.Shared (BaseRequest, ociCliBase, runOciCli)
import Erl.Oracle.Types.Common (ImageId(..), Shape(..))
import Erl.Oracle.Types.ShapeCompatibility (ImageMemoryConstraints, ImageShapeCompatibility, ImageOcpuConstraints)
import Foreign (MultipleErrors, F)
import Simple.JSON (readJSON')

type ListImageShapeCompatibilityRequest = BaseRequest
  ( imageId :: ImageId
  )

defaultListImageShapeCompatibilityRequest :: ImageId -> ListImageShapeCompatibilityRequest
defaultListImageShapeCompatibilityRequest imageId = { imageId }

type ImageMemoryConstraintsInt =
  { "max-in-gbs" :: Maybe Int
  , "min-in-gbs" :: Maybe Int
  }

fromImageMemoryConstraintsInt :: Maybe ImageMemoryConstraintsInt -> F (Maybe ImageMemoryConstraints)
fromImageMemoryConstraintsInt constraints =
  case constraints of
    Just { "max-in-gbs": maxInGbs, "min-in-gbs": minInGbs } ->
      pure $ Just { maxInGbs, minInGbs }
    Nothing -> pure Nothing

type ImageOcpuConstraintsInt =
  { "max" :: Maybe Int
  , "min" :: Maybe Int
  }

fromImageOcpuConstraintsInt :: Maybe ImageOcpuConstraintsInt -> F (Maybe ImageOcpuConstraints)
fromImageOcpuConstraintsInt constraints =
  case constraints of
    Just { "max": max, "min": min } ->
      pure $ Just { max, min }
    Nothing -> pure Nothing

type ImageShapeCompatibilityInt =
  { "image-id" :: String
  , "memory-constraints" :: Maybe ImageMemoryConstraintsInt
  , "ocpu-constraints" :: Maybe ImageOcpuConstraintsInt
  , "shape" :: String
  }

fromImageShapeCompatibilityInt :: ImageShapeCompatibilityInt -> F ImageShapeCompatibility
fromImageShapeCompatibilityInt
  { "image-id": imageId
  , "memory-constraints": memoryConstraintsInt
  , "ocpu-constraints": ocpuConstraintsInt
  , "shape": shape
  } = ado
  memoryConstraints <- fromImageMemoryConstraintsInt memoryConstraintsInt
  ocpuConstraints <- fromImageOcpuConstraintsInt ocpuConstraintsInt
  in
    { imageId: ImageId imageId
    , memoryConstraints
    , ocpuConstraints
    , shape: Shape shape
    }

type ImageShapeCompatibilityResponse =
  { "data" :: List ImageShapeCompatibilityInt
  }

fromImageShapeCompatibilityResponse :: ImageShapeCompatibilityResponse -> F (List ImageShapeCompatibility)
fromImageShapeCompatibilityResponse { "data": entries } = ado
  shapes <- traverse fromImageShapeCompatibilityInt entries
  in shapes

listCompatibleShapes :: ListImageShapeCompatibilityRequest -> Effect (Either MultipleErrors (List ImageShapeCompatibility))
listCompatibleShapes req@{ imageId } = do
  let
    cli = ociCliBase req $ "compute image-shape-compatibility-entry list "
      <> "--all "
      <> ("--image-id " <> unwrap imageId)
  outputJson <- runOciCli cli
  pure $ runExcept $ fromImageShapeCompatibilityResponse =<< readJSON' =<< outputJson
