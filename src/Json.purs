module Erl.Json
  ( class GenericTaggedWriteForeign
  , genericTaggedWriteForeignImpl
  , class GenericTaggedReadForeign
  , genericTaggedReadForeignImpl
  , class GenericEnumWriteForeign
  , genericEnumWriteForeignImpl
  , class GenericEnumReadForeign
  , genericEnumReadForeignImpl
  , genericTaggedWriteForeign
  , genericTaggedReadForeign
  , genericEnumWriteForeign
  , genericEnumReadForeign
  ) where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Except (withExcept)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Sum(..), from, to)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign (Foreign, ForeignError(..), fail)
import Foreign as Foreign
import Type.Prelude (Proxy(..))
import Simple.JSON (class ReadForeign, class WriteForeign, read', readImpl, writeImpl)

genericTaggedReadForeign
  :: forall a rep
   . Generic a rep
  => GenericTaggedReadForeign rep
  => Foreign
  -> Foreign.F a
genericTaggedReadForeign f = to <$> genericTaggedReadForeignImpl f

genericTaggedWriteForeign
  :: forall a rep
   . Generic a rep
  => GenericTaggedWriteForeign rep
  => a
  -> Foreign
genericTaggedWriteForeign a = genericTaggedWriteForeignImpl $ from a

genericEnumReadForeign
  :: forall a rep
   . Generic a rep
  => GenericEnumReadForeign rep
  => Foreign
  -> Foreign.F a
genericEnumReadForeign f = to <$> genericEnumReadForeignImpl f

genericEnumWriteForeign
  :: forall a rep
   . Generic a rep
  => GenericEnumWriteForeign rep
  => a
  -> Foreign
genericEnumWriteForeign a = genericEnumWriteForeignImpl $ from a

------------------------------------------------------------------------------
-- GenericTaggedReadForeign
class GenericTaggedReadForeign rep where
  genericTaggedReadForeignImpl :: Foreign -> Foreign.F rep

instance genericTaggedReadForeignSum ::
  ( GenericTaggedReadForeign a
  , GenericTaggedReadForeign b
  ) =>
  GenericTaggedReadForeign (Sum a b) where
  genericTaggedReadForeignImpl f =
    Inl
      <$> genericTaggedReadForeignImpl f
      <|> Inr
        <$> genericTaggedReadForeignImpl f

instance genericTaggedReadForeignConstructor ::
  ( GenericTaggedReadForeign a
  , IsSymbol name
  ) =>
  GenericTaggedReadForeign (Constructor name a) where
  genericTaggedReadForeignImpl f = do
    r :: { "type" :: String, value :: Foreign } <- read' f
    if r."type" == name then
      withExcept (map $ ErrorAtProperty name) $ Constructor <$> genericTaggedReadForeignImpl r.value
    else
      fail $ ForeignError $ "Wrong type tag " <> r."type" <> " where " <> name <> " was expected."
    where
    nameP = Proxy :: Proxy name
    name = reflectSymbol nameP

instance genericTaggedReadForeignArgument ::
  ( ReadForeign a
  ) =>
  GenericTaggedReadForeign (Argument a) where
  genericTaggedReadForeignImpl f = Argument <$> readImpl f

instance genericTaggedReadForeignNoArgument ::
  GenericTaggedReadForeign NoArguments where
  genericTaggedReadForeignImpl _ = pure NoArguments

------------------------------------------------------------------------------
-- GenericTaggedWriteForeign
class GenericTaggedWriteForeign rep where
  genericTaggedWriteForeignImpl :: rep -> Foreign

instance genericTaggedWriteForeignSum ::
  ( GenericTaggedWriteForeign a
  , GenericTaggedWriteForeign b
  ) =>
  GenericTaggedWriteForeign (Sum a b) where
  genericTaggedWriteForeignImpl (Inl a) = genericTaggedWriteForeignImpl a
  genericTaggedWriteForeignImpl (Inr b) = genericTaggedWriteForeignImpl b

instance genericTaggedWriteForeignConstructor ::
  ( GenericTaggedWriteForeign a
  , IsSymbol name
  ) =>
  GenericTaggedWriteForeign (Constructor name a) where
  genericTaggedWriteForeignImpl (Constructor a) = do
    writeImpl
      { "type": reflectSymbol (Proxy :: _ name)
      , "value": genericTaggedWriteForeignImpl a
      }

instance genericTaggedWriteForeignArg ::
  ( WriteForeign a
  ) =>
  GenericTaggedWriteForeign (Argument a) where
  genericTaggedWriteForeignImpl (Argument a) = writeImpl a

------------------------------------------------------------------------------
-- GenericEnumReadForeign
class GenericEnumReadForeign rep where
  genericEnumReadForeignImpl :: Foreign -> Foreign.F rep

instance genericEnumReadForeignSum ::
  ( GenericEnumReadForeign a
  , GenericEnumReadForeign b
  ) =>
  GenericEnumReadForeign (Sum a b) where
  genericEnumReadForeignImpl f =
    Inl
      <$> genericEnumReadForeignImpl f
      <|> Inr
        <$> genericEnumReadForeignImpl f

instance genericEnumReadForeignConstructor ::
  ( IsSymbol name
  ) =>
  GenericEnumReadForeign (Constructor name NoArguments) where
  genericEnumReadForeignImpl f = do
    s <- readImpl f
    if s == name then
      pure $ Constructor NoArguments
    else
      fail
        <<< Foreign.ForeignError
        $ "Enum string "
            <> s
            <> " did not match expected string "
            <> name
    where
    name = reflectSymbol (Proxy :: Proxy name)

------------------------------------------------------------------------------
-- GenericEnumWriteForeign
class GenericEnumWriteForeign rep where
  genericEnumWriteForeignImpl :: rep -> Foreign

instance genericEnumWriteForeignSum ::
  ( GenericEnumWriteForeign a
  , GenericEnumWriteForeign b
  ) =>
  GenericEnumWriteForeign (Sum a b) where
  genericEnumWriteForeignImpl (Inl a) = genericEnumWriteForeignImpl a
  genericEnumWriteForeignImpl (Inr b) = genericEnumWriteForeignImpl b

instance genericEnumWriteForeignConstructor ::
  ( IsSymbol name
  ) =>
  GenericEnumWriteForeign (Constructor name NoArguments) where
  genericEnumWriteForeignImpl f = writeImpl name
    where
    name = reflectSymbol (Proxy :: Proxy name)
