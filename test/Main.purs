module Test.Main where

import Prelude

import Affjax (get)
import Affjax.ResponseFormat (string)
import Data.Either (fromRight, isLeft, isRight)
import Data.String.Common (joinWith)
import Data.Traversable (traverse)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import WebIDL (Parse, parse, parseStringify)
import Mozilla.Dom as Moz

invalids :: Array String
invalids =
  [ "array.widl"
  , "caller.widl"
  , "dict-required-default.widl"
  , "duplicate-escaped.widl"
  , "duplicate.widl"
  , "enum-bodyless.widl"
  , "enum-empty.widl"
  , "enum-wo-comma.widl"
  , "enum.widl"
  , "exception.widl"
  , "extattr-empty-ids.widl"
  , "id-underscored-number.widl"
  , "implements.widl"
  , "implements_and_includes_ws.widl"
  , "iterable-empty.widl"
  , "iterator.widl"
  , "legacyiterable.widl"
  , "maplike-1type.widl"
  , "module.widl"
  , "namespace-readwrite.widl"
  , "no-semicolon-callback.widl"
  , "no-semicolon.widl"
  , "nonnullableany.widl"
  , "nonnullableobjects.widl"
  , "operation-too-special.widl"
  , "promise-nullable.widl"
  , "promise-with-extended-attribute.widl"
  , "raises.widl"
  , "readonly-iterable.widl"
  , "record-key-with-extended-attribute.widl"
  , "record-key.widl"
  , "record-single.widl"
  , "scopedname.widl"
  , "sequenceAsAttribute.widl"
  , "setlike-2types.widl"
  , "setter-creator.widl"
  , "spaced-negative-infinity.widl"
  , "spaced-variadic.widl"
  , "special-omittable.widl"
  , "stray-slash.widl"
  , "stringconstants.widl"
  , "typedef-nested.widl"
  , "union-dangling-or.widl"
  , "union-one.widl"
  , "union-zero.widl"
  , "unknown-generic.widl"
  ]

syntaxes :: Array String
syntaxes = [
  "allowany.widl"
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

webGls :: Array String
webGls =
  [ "https://www.khronos.org/registry/webgl/specs/latest/1.0/webgl.idl"
  , "https://www.khronos.org/registry/webgl/specs/latest/2.0/webgl2.idl"
  ]

parses :: String -> String -> Spec Unit
parses desc src =
  describe desc do
    it "parses" do
      let parsed = parse src -- Either String (Either (NonEmptyList ForeignError) (Array Node))
      when (isLeft parsed || (isLeft $ unsafePartial $ fromRight parsed)) $
        fail $ joinWith "\n\n" [src, show parsed, parseStringify src]

good :: Parse -> Boolean
good parsed = isRight parsed && (isRight $ unsafePartial $ fromRight parsed)

bad :: Parse -> Boolean
bad parsed = isLeft parsed || (isLeft $ unsafePartial $ fromRight parsed)

examples :: String -> (Parse -> Boolean) -> Array String -> Spec Unit
examples title predicate files =
  describe title do
    void $ traverse (\file ->
        describe file do
          it "parses" do
            resp <- get string $ file
            let str = unsafePartial $ fromRight resp.body
            let parsed = parse str
            when (not $ predicate parsed) $
              fail $ joinWith "\n\n" [str, show parsed, parseStringify str]
      ) files

main :: Effect Unit
main = run [consoleReporter] do
  describe "Webidl2" do
    examples "Positives" good $ ((<>) "https://raw.githubusercontent.com/w3c/webidl2.js/develop/test/syntax/idl/") <$> syntaxes
    examples "Negatives" bad $ ((<>) "https://raw.githubusercontent.com/w3c/webidl2.js/develop/test/invalid/idl/") <$> invalids

  examples "Webgl" good webGls
  -- examples "Mozilla" good Moz.files
