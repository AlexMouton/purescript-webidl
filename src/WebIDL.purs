-- | A basic wrapper for the `webidl2` library, and some ADT sugar on top.

module WebIDL where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Type.Data.Boolean (True, False) as TB
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, wrap)
import Data.Traversable (traverse)
import Foreign (F, Foreign, ForeignError, readString)
import Foreign.Index (index)
import Simple.JSON as JSON
import WebIDL.StringLiteral (StringLiteral)
import WebIDL.BooleanLiteral (BooleanLiteral)

type RecTrivia =
  { base :: Maybe String
  , name :: Maybe String
  , open :: Maybe String
  , close :: Maybe String
  , termination :: Maybe String
  }

type RecValueRhs =
  { value :: String
  , trivia :: String -- { assign :: String, value :: String }
  , separator :: Maybe { value :: String, trivia :: String }
  }

type RecRhsItem = { type :: String, value :: String, trivia :: { assign :: String, value :: String } }
type RecRhsList = { type :: String, value :: Array RecValueRhs, trivia :: RecTrivia }

data Rhs = RhsItem RecRhsItem
  | RhsList RecRhsList

derive instance genericRhs :: Generic Rhs _

instance showRhs :: Show Rhs where
  show x = genericShow x

instance readRhs :: JSON.ReadForeign Rhs where
  readImpl f = do
    ty <- RhsItem <$> JSON.read' f
      <|> RhsList <$> JSON.read' f
    pure ty

type  RecExtendedAttribute =
  { name :: String
  , signature :: Maybe { arguments :: Array Argument, trivia :: RecTrivia }
  , type :: StringLiteral "extended-attribute"
  , rhs :: Maybe Rhs
  , trivia :: { name :: String }
  , separator :: Maybe { value :: String, trivia :: String }
  }

type RecExtendedAttributes =
  { items :: Array RecExtendedAttribute
  , trivia :: RecTrivia
  }

type RecIdlTypeArgument =
  { type :: StringLiteral "argument-type"
  , idlType :: String
  , baseName :: Maybe String
  , generic :: Maybe {value :: String, trivia :: RecTrivia}
  , nullable :: Maybe {trivia :: String}
  , union :: Boolean
  , prefix :: Maybe {value :: String, trivia :: String}
  , postfix :: Maybe {value :: String, trivia :: String}
  , separator :: Maybe {value :: String, trivia :: String}
  , extAttrs :: Maybe RecExtendedAttributes
  , trivia :: RecTrivia
  }

type RecIdlTypeReturn =
  { type :: StringLiteral "return-type"
  , generic :: Maybe {value :: String, trivia :: RecTrivia}
  , nullable :: Maybe {trivia :: String}
  , union :: Boolean
  , idlType :: String
  , baseName :: Maybe String
  , prefix :: Maybe {value :: String, trivia :: String}
  , postfix :: Maybe {value :: String, trivia :: String}
  , separator :: Maybe {value :: String, trivia :: String}
  , extAttrs :: Maybe RecExtendedAttributes
  , trivia :: RecTrivia
  }

type RecIdlTypeAttributeStr =
  { type :: StringLiteral "attribute-type"
  , generic :: Maybe {value :: String, trivia :: RecTrivia}
  , nullable :: Maybe {trivia :: String}
  , union :: Maybe Boolean
  , idlType :: String
  , baseName :: Maybe String
  , prefix :: Maybe {value :: String, trivia :: String}
  , postfix :: Maybe {value :: String, trivia :: String}
  , separator :: Maybe {value :: String, trivia :: String}
  , extAttrs :: Maybe RecExtendedAttributes
  , trivia :: RecTrivia
  }

type RecIdlTypeAttributeCons =
  { type :: StringLiteral "attribute-type"
  , generic :: Maybe {value :: String, trivia :: RecTrivia}
  , nullable :: Maybe {trivia :: String}
  , union :: Maybe Boolean
  , idlType :: Array IdlType
  , baseName :: Maybe String
  , prefix :: Maybe {value :: String, trivia :: String}
  , postfix :: Maybe {value :: String, trivia :: String}
  , separator :: Maybe {value :: String, trivia :: String}
  , extAttrs :: Maybe RecExtendedAttributes
  , trivia :: RecTrivia
  }

type RecIdlTypeUnion =
  { union :: BooleanLiteral TB.True
  , idlType :: Array IdlType
  }

type RecIdlTypeConst =
  { type :: StringLiteral "const-type"
  , generic :: Maybe {value :: String, trivia :: RecTrivia}
  , nullable :: Maybe {trivia :: String}
  , union :: Maybe Boolean
  , idlType :: String
  , baseName :: Maybe String
  , prefix :: Maybe {value :: String, trivia :: String}
  , postfix :: Maybe {value :: String, trivia :: String}
  , separator :: Maybe {value :: String, trivia :: String}
  , extAttrs :: Maybe RecExtendedAttributes
  , trivia :: RecTrivia
  }

type RecIdlTypeNull =
  { generic :: Maybe {value :: String, trivia :: RecTrivia}
  , nullable :: Maybe {trivia :: String}
  , union :: BooleanLiteral TB.False
  , idlType :: String
  , baseName :: String
  , prefix :: Maybe {value :: String, trivia :: String}
  , postfix :: Maybe {value :: String, trivia :: String}
  , separator :: Maybe {value :: String, trivia :: String}
  , extAttrs :: Maybe RecExtendedAttributes
  , trivia :: RecTrivia
  }

type RecIdlTypeDictionary =
  { type :: StringLiteral "dictionary-type"
  , generic :: Maybe {value :: String, trivia :: RecTrivia}
  , nullable :: Maybe {trivia :: String}
  , union :: Boolean
  , idlType :: String
  , baseName :: String
  , prefix :: Maybe {value :: String, trivia :: String}
  , postfix :: Maybe {value :: String, trivia :: String}
  , separator :: Maybe {value :: String, trivia :: String}
  , extAttrs :: Maybe RecExtendedAttributes
  , trivia :: RecTrivia
  }

data IdlType
  = ArgumentType RecIdlTypeArgument
  | ReturnType RecIdlTypeReturn
  | AttributeTypeStr RecIdlTypeAttributeStr
  | AttributeTypeCons RecIdlTypeAttributeCons
  | ConstType RecIdlTypeConst
  | UnionType RecIdlTypeUnion
  | GenericType { idlType :: Array IdlType }
  | DictionaryType RecIdlTypeDictionary
  | NullableType RecIdlTypeNull

derive instance genericType :: Generic IdlType _

instance showType :: Show IdlType where
  show x = genericShow x

instance readForeignType :: JSON.ReadForeign IdlType where
  readImpl f = do
    -- nullable <- readBoolean =<< index f "nullable"
    ty <- ArgumentType <$> JSON.read' f
      <|> ReturnType <$> JSON.read' f
      <|> AttributeTypeStr <$> JSON.read' f
      <|> AttributeTypeCons <$> JSON.read' f
      <|> ConstType <$> JSON.read' f
      <|> UnionType <$> JSON.read' f
      <|> DictionaryType <$> JSON.read' f
      <|> NullableType <$> JSON.read' f
      <|> GenericType <$> JSON.read' f
    pure ty
    -- pure $ if nullable then NullableType ty else ty


type RecArgument =
  { name :: String
  , escapedName :: String
  , idlType :: IdlType
  , optional :: Maybe { trivia :: String } --Maybe Boolean
  , default :: Maybe Rv
  , variadic :: Maybe { trivia :: String }
  }
newtype Argument = Argument RecArgument
derive instance newtypeArgument :: Newtype Argument _

derive instance genericArgument :: Generic Argument _
instance showArgument :: Show Argument where
  show x = genericShow x

instance readForeignArgument :: JSON.ReadForeign Argument where
  readImpl = map wrap <<< JSON.read'

type RecMemberIterable =
  { type :: StringLiteral "iterable"
  , idlType :: Array  IdlType
  , readOnly :: Maybe Boolean
  , trivia :: RecTrivia
  , extAttrs :: Maybe RecExtendedAttributes
  }

type RecMemberMaplike =
  { type :: StringLiteral "maplike"
  , idlType :: Array  IdlType
  , readOnly :: Maybe Boolean
  , trivia :: RecTrivia
  , extAttrs :: Maybe RecExtendedAttributes
  }

type RecMemberSetlike =
  { type :: StringLiteral "setlike"
  , idlType :: Array  IdlType
  , readOnly :: Maybe Boolean
  , trivia :: RecTrivia
  , extAttrs :: Maybe RecExtendedAttributes
  }
type RecMemberOperationBase =
  { type :: StringLiteral "operation"
  , trivia :: RecTrivia
  , body :: { idlType :: IdlType
            , name :: { value :: String, escaped :: String, trivia :: String }
            , arguments :: Array Argument
            }
  }

type RecMemberOperationGetter =
  { type :: StringLiteral "operation"
  , getter :: { trivia :: String }
  , trivia :: RecTrivia
  , body :: { idlType :: IdlType
            , arguments :: Array Argument
            }
  }

type RecMemberOperationSetter =
  { type :: StringLiteral "operation"
  , setter :: { trivia :: String }
  , trivia :: RecTrivia
  , body :: { idlType :: IdlType
            , arguments :: Array Argument
            }
  }

type RecMemberOperationDeleter =
  { type :: StringLiteral "operation"
  , deleter :: { trivia :: String }
  , trivia :: RecTrivia
  , body :: { idlType :: IdlType
            , arguments :: Array Argument
            }
  }

type RecMemberOperationStatic =
  { type :: StringLiteral "operation"
  , static :: { trivia :: String }
  , trivia :: RecTrivia
  , body :: { idlType :: IdlType
            , name :: { value :: String, escaped :: String, trivia :: String }
            , arguments :: Array Argument
            }
  }

type RecMemberOperationStringifier =
  { type :: StringLiteral "operation"
  , stringifier :: { trivia :: String }
  , trivia :: RecTrivia
  , body :: Maybe { idlType :: IdlType
            , name :: Maybe { value :: String, escaped :: String, trivia :: String }
            , arguments :: Array Argument
            }
  }

type RecMemberAttribute =
  { type            :: StringLiteral "attribute"
  , name            :: String
  , escapedName     :: String
  , static          :: Maybe { trivia :: String }
  , stringifier     :: Maybe { trivia :: String }
  , inherit         :: Maybe { trivia :: String }
  , readonly        :: Maybe { trivia :: String }
  , idlType         :: IdlType
  , extAttrs        :: Maybe RecExtendedAttributes
  }

type RecValueString = { type :: StringLiteral "string", value :: String }
type RecValueNumber = { type :: StringLiteral "number", value :: String }
type RecValueBoolean = { type :: StringLiteral "boolean", value :: Boolean }
type RecValueNull = { type :: StringLiteral "null" }
type RecValueInf = { type :: StringLiteral "Infinity", negative :: Boolean}
type RecValueNan = { type :: StringLiteral "NaN"}
type RecValueSequence = { type :: StringLiteral "sequence", value :: Array Boolean }

data Rv
  = Sequence RecValueSequence
  | NaN RecValueNan
  | Inf RecValueInf
  | Null RecValueNull
  | Bool RecValueBoolean
  | Number RecValueNumber
  | Str RecValueString

derive instance genericRv :: Generic Rv _
instance showRv :: Show Rv where
  show x = genericShow x

instance readForeignRv :: JSON.ReadForeign Rv where
  readImpl f
    = Sequence <$> JSON.read' f
    <|> NaN <$> JSON.read' f
    <|> Inf <$> JSON.read' f
    <|> Null <$> JSON.read' f
    <|> Bool <$> JSON.read' f
    <|> Number <$> JSON.read' f
    <|> Str <$> JSON.read' f

type RecMemberConstant =
  { type :: StringLiteral "const"
  , idlType :: IdlType
  , name :: String
  , value :: Rv
  , trivia :: RecTrivia
  , extAttrs :: Maybe RecExtendedAttributes
  }

type RecMemberField =
  { type :: StringLiteral "field"
  , name :: String
  , required :: Maybe { trivia :: String }
  , idlType :: IdlType
  , extAttrs :: Maybe RecExtendedAttributes
  , default :: Maybe Rv
  }

data Member
  = IterableMember RecMemberIterable
  | MaplikeMember RecMemberMaplike
  | SetlikeMember RecMemberSetlike
  | OperationMemberGetter RecMemberOperationGetter
  | OperationMemberSetter RecMemberOperationSetter
  | OperationMemberDeleter RecMemberOperationDeleter
  | OperationMemberStatic RecMemberOperationStatic
  | OperationMemberStringifier RecMemberOperationStringifier
  | OperationMemberBase RecMemberOperationBase
  | AttributeMember RecMemberAttribute
  | ConstantMember RecMemberConstant
  | FieldMember RecMemberField
  | OtherMember String

derive instance genericMember :: Generic Member _

instance showMember :: Show Member where
  show x = genericShow x

instance readForeignMember :: JSON.ReadForeign Member where
  readImpl f =
    IterableMember <$> JSON.read' f
    <|> MaplikeMember<$> JSON.read' f
    <|> SetlikeMember<$> JSON.read' f
    <|> OperationMemberGetter <$> JSON.read' f
    <|> OperationMemberSetter <$> JSON.read' f
    <|> OperationMemberDeleter <$> JSON.read' f
    <|> OperationMemberStatic <$> JSON.read' f
    <|> OperationMemberStringifier <$> JSON.read' f
    <|> OperationMemberBase <$> JSON.read' f
    <|> AttributeMember <$> JSON.read' f
    <|> ConstantMember <$> JSON.read' f
    <|> FieldMember <$> JSON.read' f

-- | A node represented as a PureScript data type.

type RecNodeInterface =
  { type :: StringLiteral "interface"
  , name :: String
  , escapedName :: String
  , partial :: Maybe { trivia :: String }
  , members :: Array Member
  , trivia :: RecTrivia
  , inheritance :: Maybe { name :: String }
  , extAttrs :: Maybe RecExtendedAttributes
  }

type RecNodeImplements =
  { target          :: String
  , implements      :: String
  }

type RecNodeTypeDef =
  { type            :: StringLiteral "typedef"
  , idlType         :: IdlType
  , name            :: String
  , trivia          :: RecTrivia
  , extAttrs        :: Maybe RecExtendedAttributes
  }

type RecIncludes =
  { type :: StringLiteral "includes"
  , target :: String
  , includes :: String
  , extAttrs :: Maybe RecExtendedAttributes
  }

type RecNodeNamespace =
  { type :: StringLiteral "namespace"
  , name :: String
  , partial :: Maybe { trivia :: String }
  , members :: Array Member
  , trivia :: RecTrivia
  , extAttrs :: Maybe RecExtendedAttributes
  }

type RecNodeCallback =
  { type :: StringLiteral "callback"
  , name :: String
  , idlType :: RecIdlTypeReturn 
  , arguments :: Array Argument
  , trivia :: RecTrivia
  , extAttrs :: Maybe RecExtendedAttributes
  }

type RecNodeDictionary =
  { type :: StringLiteral "dictionary"
  , name :: String
  , escapedName:: String
  , partial :: Maybe { trivia :: String }
  , members :: Array RecMemberField
  , trivia :: RecTrivia
  , inheritance :: Maybe { name :: String }
  , extAttrs :: Maybe RecExtendedAttributes
  }

type RecNodeException =
  { name            :: String
  , members         :: Array Member
  , inheritance     :: Maybe String
  }

type RecEnumValue =
  { type :: StringLiteral "enum-value"
  , value :: String
  , trivia :: String
  , separator :: Maybe { value :: String, trivia :: String }
  }

type RecNodeEnum =
  { type :: StringLiteral "enum"
  , name            :: String
  , values          :: Array RecEnumValue
  }

data Node
  = InterfaceNode RecNodeInterface
  | ImplementsNode RecNodeImplements
  | TypeDefNode RecNodeTypeDef
  | NamespaceNode RecNodeNamespace
  | CallbackNode RecNodeCallback
  | DictionaryNode RecNodeDictionary
  | ExceptionNode RecNodeException
  | EnumNode RecNodeEnum
  | OtherNode String

derive instance genericNode :: Generic Node _

instance showNode :: Show Node where
  show x = genericShow x

instance readForeignNode :: JSON.ReadForeign Node where
  readImpl f = readNode f

readNode :: Foreign -> F Node
readNode f = do
  _type <- readString =<< index f "type"
  case _type of
    "interface" -> InterfaceNode <$> JSON.read' f
    "implements" -> ImplementsNode <$> JSON.read' f
    "typedef" -> TypeDefNode <$> JSON.read' f
    "callback" -> CallbackNode <$> JSON.read' f
    "dictionary" -> DictionaryNode <$> JSON.read' f
    "exception" -> ExceptionNode <$> JSON.read' f
    "enum" -> EnumNode <$> JSON.read' f
    _ -> pure $ OtherNode _type


foreign import parseImpl ::
  (String -> Either String (Array Foreign))
  -> ((Array Foreign) -> Either String (Array Foreign))
  -> String
  -> Either String (Array Foreign)

foreign import parseStringify :: String -> String

type Parse = Either String (Either (NonEmptyList ForeignError) (Array Node))

parse :: String -> Parse
parse =
  parseImpl Left Right -- Either String (Array Foreign)
  >>> map (traverse ( runExcept <<< readNode ))
