module Test.Main where

import Prelude

import Affjax (get)
import Affjax.ResponseFormat (printResponseFormatError, string)
import Data.Either (either)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (logShow, log)
import WebIDL (parse)

root :: String
root = "https://raw.githubusercontent.com/w3c/webidl2.js/develop/test/syntax/idl/"

files :: Array String
files = [ "allowany.widl"
, "attributes.widl"
, "callback.widl"
, "constants.widl"
, "constructor.widl"
, "dictionary-inherits.widl"
, "dictionary.widl"
, "documentation-dos.widl"
, "documentation.widl"
, "enum.widl"
, "equivalent-decl.widl"
, "escaped-name.widl"
, "extended-attributes.widl"
, "generic.widl"
, "getter-setter.widl"
, "identifier-qualified-names.widl"
, "indexed-properties.widl"
, "inherits-getter.widl"
, "interface-inherits.widl"
, "iterable.widl"
, "maplike.widl"
, "mixin.widl"
, "namedconstructor.widl"
, "namespace.widl"
, "nointerfaceobject.widl"
, "nullable.widl"
, "nullableobjects.widl"
, "operation-optional-arg.widl"
, "overloading.widl"
, "overridebuiltins.widl"
, "partial-interface.widl"
, "primitives.widl"
, "promise-void.widl"
, "prototyperoot.widl"
, "putforwards.widl"
, "record.widl"
, "reg-operations.widl"
, "replaceable.widl"
, "sequence.widl"
, "setlike.widl"
, "static.widl"
, "stringifier-attribute.widl"
, "stringifier-custom.widl"
, "stringifier.widl"
, "treatasnull.widl"
, "treatasundefined.widl"
, "typedef-union.widl"
, "typedef.widl"
, "typesuffixes.widl"
, "uniontype.widl"
, "variadic-operations.widl"
]

examples :: Array String -> Aff (Array Unit)
examples =
  traverse (
    (<>) root
    >>> get string -- :: Aff (Response (Either ResponseFormatError a))
    >>> map (
      _.body -- (Either ResponseFormatError a)
      >>> either (printResponseFormatError >>> log) (parse >>> logShow)
      >>> liftEffect
    )
    >>> join
  )

interface :: String
interface = """
  interface Foo {
  };
  """

interfaceOperation :: String
interfaceOperation = """
  interface Foo : Bar {
    object baz(string bam);
  };
  """

main :: Effect Unit
main = launchAff_ $ examples files
