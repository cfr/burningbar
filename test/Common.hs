import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck
import Test.HUnit
import Test.QuickCheck hiding (generate)
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
emptyInt = snd (translator (Swift.swift "Tr" "I") []) @?= "\
\import Foundation\n\
\\n\
\public func idTf<T>(a: T) -> T { return a }\n\
\public protocol Tr {\n\
\    typealias CancellationToken\n\
\    func cancel(token: CancellationToken)\n\
\    func cast(method: String, arguments: [String : AnyObject])\n\
\    func listen(event: String,\n\
\                completion: [String : AnyObject] -> Void) -> CancellationToken\n\
\    func call(method: String, arguments: [String : AnyObject],\n\
\              completion: [String : AnyObject] -> Void) -> CancellationToken\n\
\}\n\
\\n\
\public class I <T: Tr> {\n\
\    public func cancel(token: Tr.CancellationToken) { transport.cancel(token) }\n\
\    public func listen(event: String,\n\
\        completion: [String : AnyObject] -> Void) -> Tr.CancellationToken { transport.listen(event, completion: completion) }\n\
\    public func cast(method: String, arguments: [String : AnyObject]) { transport.cast(method, arguments: arguments) }\n\
\    public init(transport: Tr) { self.transport = transport }\n\
\    public let transport: Tr\n\
\\n\
\}\n"

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

