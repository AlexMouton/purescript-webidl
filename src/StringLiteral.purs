module WebIDL.StringLiteral where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign (ForeignError(..), fail, readString)
import Simple.JSON (class ReadForeign)
import Unsafe.Coerce (unsafeCoerce)

data StringLiteral (s :: Symbol)
instance readStringLiteral :: (IsSymbol s) => ReadForeign (StringLiteral s) where
  readImpl x = do
    str <- readString x
    case str == (reflectSymbol (SProxy :: SProxy s)) of
      false -> fail $ ForeignError "LocationParameter"
      _ -> pure $ unsafeCoerce str

instance showStringLiteral :: (IsSymbol s) => Show (StringLiteral s) where
  show _ = reflectSymbol (SProxy :: SProxy s)
