import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck
import Test.HUnit
import Test.QuickCheck
import Control.Monad

import qualified Swift
import qualified Parse
import Language
import Util
import Service

emptySpec = Parse.parse "-\n-" @?= []

method = Parse.parseMethod ["met m Void"] @?= Just (Method "m" (Typename "Void") "m" [])
methodN = Parse.parseMethod ["met m Void n"] @?= Just (Method "m" (Typename "Void") "n" [])
record = Parse.parseRecord ["rec r"] @?= Just (Record "r" [] Nothing)
var = Parse.parseVar "a T" @?= Variable "a" (Typename "T") Nothing
struct = Swift.struct "s" [] Nothing @?= "public struct s : ToJSON {\n\
                                         \    public let asJSON: [String : AnyObject]\n\
                                         \    public init(_ json: [String : AnyObject]) {\n\
                                         \        asJSON = json\n    }\n\
                                         \    public static let Name = \"s\"\n\
                                         \    public let Name = \"s\"\n\n}\n\n"

instance Arbitrary Type where
  arbitrary = oneof [ liftM Array arbitrary
                    -- no dictionaries because swift and burnbar spec synax differs
                    --, liftM2 Dictionary (elements Language.atoms) arbitrary
                    , liftM Optional arbitrary
                    , elements Language.atoms]
  coarbitrary = undefined

typeId = ap (≡) (Parse.parseType ∘ Swift.fromType)

tests = [testGroup "Misc" [ testCase "empty" emptySpec, testCase "rec" record, testCase "met" method
                          , testCase "var" var, testCase "met nm" methodN, testCase "struct" struct
                          , testProperty "parse/gen t" typeId ]]

main = defaultMain tests

