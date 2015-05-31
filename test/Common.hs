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

method = Parse.parseMethod ["met m Void"] @?= Just (Method "m" (TypeName "Void") "m" [])
methodN = Parse.parseMethod ["met m Void n"] @?= Just (Method "m" (TypeName "Void") "n" [])
record = Parse.parseRecord ["rec r"] @?= Just (Record "r" [] Nothing)
var = Parse.parseVar "a T" @?= Just (Variable "a" (TypeName "T") Nothing)
emptyInt = snd (translator (Swift.swift False "Tr" "I") []) @?= intDefs "Tr" "I" []

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
                          , testProperty "parse/gen t" typeId ]]

main = defaultMain tests

