module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck
import Test.HUnit
import Test.QuickCheck
import Control.Monad

import Parse
import Language
import Swift
import Unicode

emptySpec = parse "-\n-" @?= []

method = Parse.parseMethod ["met m Void"] @?= Just (Method "m" (Typename "Void") "m" [])
methodN = Parse.parseMethod ["met m Void n"] @?= Just (Method "m" (Typename "Void") "n" [])
record = Parse.parseRecord ["rec r"] @?= Just (Record "r" [] Nothing)
var = Parse.parseVar "a T" @?= Variable "a" (Typename "T")

instance Arbitrary Type where
  arbitrary = oneof [ liftM Array arbitrary
                    -- no dictionaries because swift and burnbar spec synax differs
                    --, liftM2 Dictionary (elements Language.atoms) arbitrary
                    , liftM Optional arbitrary
                    , elements Language.atoms]

typeId = ap (≡) (Parse.parseType ∘ Swift.fromType)

tests = [testGroup "Misc" [ testCase "empty" emptySpec, testCase "rec" record, testCase "met" method
                          , testCase "var" var, testCase "named met" methodN
                          , testProperty "parse/write type" typeId ]]

main = defaultMain tests

