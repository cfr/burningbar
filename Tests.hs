module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Parse
import Language

emptySpec = TestCase $ assertEqual "parse \"\" == []" (parse "") []

ping = TestCase $ assertEqual "parsing ping spec"
                              (parse "met ping Void")
                              [Method "ping" (Typename "Void") "ping" []]

tests = hUnitTestToTests $ TestList [ TestLabel "empty" emptySpec
                                    , TestLabel "ping" ping ]

main = defaultMain tests

