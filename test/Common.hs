{-# LANGUAGE UnicodeSyntax #-}
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck
import Test.HUnit
import Test.QuickCheck hiding (generate)
import Control.Monad

import qualified Swift
import Static
import qualified Parse
import Language
import Util
import Service

emptySpec = Parse.parse "-\n-" @?= []

method = Parse.parseMethod ["met n: Void"] @?= Just (Method (Identifier "n" "n") [] (TypeName "Void"))
methodN = Parse.parseMethod ["met r as l: Void"] @?= Just (Method (Identifier "l" "r") [] (TypeName "Void"))
record = Parse.parseRecord ["rec n"] @?= Just (Record (Identifier "n" "n") [] Nothing)
var = Parse.parseVar "a T" @?= Just (Variable "a" (TypeName "T") Nothing)
emptyInt = snd (translator (Swift.swift False "Tr" "I") []) @?= intDefs "Tr" "I" []
exampleSpec = do
  spec ← readFile "spec.bb"
  e ← readFile "xcode/TestGen/Entities.swift"
  i ← readFile "xcode/TestGen/Interface.swift"
  let (ge, gi) = translator (Swift.swift True "Transport" "Interface") (Parse.parse spec)
  let rmHead = unlines ∘ tail ∘ lines -- remove header comment
  unless ((ge ≡ rmHead e) ∧ (gi ≡ rmHead i)) (assertFailure "generating wrong code")

instance Arbitrary Type where
  arbitrary = oneof [ liftM Array arbitrary
                    -- no dictionaries because swift and burnbar spec synax differs
                    --, liftM2 Dictionary (elements Language.atoms) arbitrary
                    , liftM Optional arbitrary
                    , elements Language.atoms]
  coarbitrary = undefined

typeId = ap (≡) (Parse.parseType ∘ Swift.fromType)

tests = [testGroup "Misc" [ testCase "empty" emptySpec, testCase "rec" record, testCase "met" method
                          , testCase "var" var, testCase "met nm" methodN, testCase "empty int" emptyInt
                          , testCase "example" exampleSpec, testProperty "parse/gen t" typeId ]]

main = defaultMain tests

