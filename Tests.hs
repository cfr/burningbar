module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.HUnit
import Test.QuickCheck
import Control.Monad

import Parse
import Language
import Swift
import Unicode

emptySpec = (parse "") @?= []

ping = (parse "met ping Void") @?= [Method "ping" (Typename "Void") "ping" []]

instance Arbitrary Type where
  arbitrary = oneof [ liftM Array arbitrary
                    , liftM2 Dictionary (elements Language.atoms) arbitrary
                    , liftM Optional arbitrary
                    , elements Language.atoms]

typeId = (Parse.parseType ∘ Swift.fromType) ≡ id

tests = [testGroup "Misc" [ testCase "empty" emptySpec
                          , testCase "ping" ping
                          , testProperty "parse/write type" typeId ]]

main = defaultMain tests

