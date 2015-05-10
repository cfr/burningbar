module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

testfoo = TestCase $ assertEqual "Foo == Foo"
    "Foo" "Foo"

testfoo2 = TestCase $ assertEqual "Foo != Bar"
    "Foo" "Bar"

-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests = hUnitTestToTests $ TestList [TestLabel "testfoo" testfoo,
                                     TestLabel "testfoo2" testfoo2]

main = defaultMain tests

