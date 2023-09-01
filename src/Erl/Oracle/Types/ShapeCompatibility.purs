module Erl.Oracle.Types.ShapeCompatibility where

import Data.Maybe (Maybe)
import Erl.Oracle.Types.Common (ImageId, Shape)

type ImageMemoryConstraints =
  { maxInGbs :: Maybe Int
  , minInGbs :: Maybe Int
  }

type ImageOcpuConstraints =
  { max :: Maybe Int
  , min :: Maybe Int
  }

type ImageShapeCompatibility =
  { imageId :: ImageId
  , memoryConstraints :: Maybe ImageMemoryConstraints
  , ocpuConstraints :: Maybe ImageOcpuConstraints
  , shape :: Shape
  }

