module WebIDL.BooleanLiteral where

import Prelude

import Type.Data.Boolean (class IsBoolean, kind Boolean, BProxy(..), reflectBoolean)
import Foreign (ForeignError(..), fail, readBoolean)
import Simple.JSON (class ReadForeign)
import Unsafe.Coerce (unsafeCoerce)

data BooleanLiteral (b :: Boolean)

instance readBooleanLiteral :: (IsBoolean b) => ReadForeign (BooleanLiteral b) where
  readImpl x = do
    bl <- readBoolean x
    case bl == (reflectBoolean (BProxy :: BProxy b)) of
      false -> fail $ ForeignError "LocationParameter"
      _ -> pure $ unsafeCoerce bl

instance showBooleanLiteral :: (IsBoolean b) => Show (BooleanLiteral b) where
  show _ = show $ reflectBoolean (BProxy :: BProxy b)
