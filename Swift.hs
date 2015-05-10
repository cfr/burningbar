{-# LANGUAGE UnicodeSyntax #-}
module Swift (swift) where

import Language hiding (wrapInterface, wrapEntities, generate)

import Data.Char (toLower)
import Unicode

swift ∷ Typename → Typename → Language
swift transportType interfaceType = Language generate wrapEntities wrapInterface'
  where wrapInterface' = wrapInterface transportType interfaceType

generate ∷ Declaration → String
generate (Record name vars) = record name vars
generate method = function name remoteName args rawRetType
    where (Method remoteName rawRetType name args) = method

function name rpc args t = "" ⇝ "public func " ⧺ name
                           ⧺ "(" ⧺ argList ⧺ "completion: " ⧺ fromType t ⧺ " -> Void)"
                           ⧺ " -> T.CancellationToken"
                           ⧺ " {" ↝ body ⇝ "}\n"
  where body = s 6 ⧺ "return t.call(\"" ⧺ rpc ⧺ "\", arguments: " ⧺ passedArgs ⧺ ") { " ⧺ parseReply
        argList = args ≫= fromArg
        fromArg (Variable n t) = n ≑ t ⧺ ", "
        passedArgs = constructDict serializeVar args
        passArg (Variable n _) = "\"" ⧺ n ⧺ "\": " ⧺ n ⧺ " as Any, "
        parseReply | t ≡ Typename "Void" = " _ in }"
                   | otherwise = "r in"
                                 ⟿  "let v = " ⧺ fromType t ⧺ "(r)"
                                 ⟿  "completion(v)" ↝ s 6 ⧺ "}"

record name vars = "public struct " ⧺ name ⧺ " {"
                   ↝ concat decls ⧺ "}\n\n"
  where decls = initDecl : serializeDecl : map varDecl vars
        initDecl = s 4 ⧺ "public init(_ json: " ⧺ jsonT ⧺ ") {"
                   ↝ list initDict ⧺ list initVar ⧺ s 4 ⧺ "}\n"
        serializeDecl = s 4 ⧺ "public var serialized: " ⧺ jsonT ⧺ " { get {"
                        ⟿  "return " ⧺ serialized ⧺ " } }\n"
        serialized = constructDict serializeVar vars
        varDecl (Variable n t) = s 4 ⧺ "public var " ⧺ n ≑ t ⧺ "\n"
        initDict (Variable n d@(Dictionary k t)) | t ∉ primitives = s 8 ⧺ n ⧧ fromType d ⧺ "()\n"
        initDict (Variable n (Optional d@(Dictionary k t))) | t ∉ primitives = s 8 ⧺ n ⧧ fromType d ⧺ "()\n"
        initDict _ = ""
        list = (vars ≫=)

constructDict ∷ (Variable → String) → [Variable] → String
constructDict rule vars | null vars = "[:]"
                        | otherwise = "[" ⧺ (init ∘ init ∘ (vars ≫=)) rule ⧺ "]"
serializeVar (Variable n _) = "\"" ⧺ n ⧺ "\": " ⧺ n ⧺ " as Any, "

initVar v@(Variable n (Optional t)) | t ∈ primitives = initPrimitive (Optional t) n
                                    | otherwise = withOptionalJSON n (initNewtype n t)
initVar (Variable n d@(Dictionary k t)) | t ∈ primitives = initPrimitive (Dictionary k t) n
                                        | otherwise = s 8 ⧺ mapJSON n d ⧺ "\n"
initVar (Variable n (Array t))    | t ∈ primitives = initPrimitive (Array t) n
                                  | otherwise = s 8 ⧺ n ⧧ mapJSON n (Array t) ⧺ "\n"
initVar (Variable n t)            | t ∈ primitives = initPrimitive t n
                                  | otherwise = s 8 ⧺ n ⧧ initNewtype n t (sub n ⧺ " as! " ⧺ jsonT) ⧺ "\n"

initWithElem n = s 8 ⧺ n ⧧ sub n ⧺ " as"
initNewtype n d@(Dictionary _ _) _ = mapJSON (n ⧺ "!") d
--initNewtype n d@(Array _ ) _ = mapJSON (n ⧺ "!") d
initNewtype n t from = n ⧧ fromType t ⧺ "(" ⧺ from ⧺ ")"
initPrimitive (Optional t) n = initWithElem n ⧺ "? " ⧺ fromType t ⧺ "\n"
initPrimitive t n = initWithElem n ⧺ "! " ⧺ fromType t ⧺ "\n"
withOptionalJSON n init = s 8 ⧺ "if let json" ⧧ sub n ⧺ " as? " ⧺ jsonT ⧺ ""
                          ⧺ " {" ↝ s 10 ⧺ init "json" ⟿  "} else { " ⧺ n ⧧ "nil }\n"
mapJSON n t = "map(" ⧺ castArr ⧺ ") {" ⧺ closure ⧺ "}"
  where castArr | Array _ ← t = sub n ⧺ " as! [" ⧺ jsonT ⧺ "]"
                | Dictionary _ _ ← t = "json.keys"
        closure | Array t' ← t = fromType t' ⧺ "($0)"
                | Dictionary k v ← t = "(k" ≑ k ⧺ ") in self." ⧺ n ⧺ "[k]"
                                       ⧧ fromType v ⧺ "(" ⧺ ("json[k] as! " ⧺ jsonT) ⧺ ")"

n ⧧ v = n ⧺ " = " ⧺ v
n ≑ t = n ⧺ ": " ⧺ fromType t
jsonT = "[String: AnyObject]"
sub k = "json[\"" ⧺ k ⧺ "\"]"

primitives = foldr (=≪) atoms [opt, dict, opt, ap Array, opt]
  where atoms = map Typename ["String", "NSNumber"] -- TODO: "Bool", "Int", "Float", "URL", "IntString"
        ap = (take 2 ∘) ∘ iterate
        opt = ap Optional
        dict a = a : [Dictionary x a | x ← atoms]

fromType (Array t) = "[" ⧺ fromType t ⧺ "]"
fromType (Optional t) = fromType t ⧺ "?"
fromType (Dictionary tk tv) = "[" ⧺ fromType tk ≑ tv ⧺ "]"
fromType (Typename typename) = typename

wrapInterface ∷ Typename → Typename → String → String
wrapInterface transportType interfaceType rpcs = foundation header
  where header = defTransport transportType ↝ "public class " ⧺ interfaceType
                 ⧺ " <T: " ⧺ transportType ⧺ "> {\n"
                 ⇝ "public init(" ⧺ transport ⧺ ": T) { t = " ⧺ transport ⧺ " }" ↝ rpcs
                 ⇝ "private let t: T" ↝ "}\n"
        decapitalize (c:cs) = toLower c : cs
        transport = decapitalize transportType

wrapEntities ∷ String → String
wrapEntities = foundation
foundation = ("import Foundation\n" ↝)
defTransport t = "public protocol " ⧺ t ⧺ " {"
                 ⇝ "typealias CancellationToken"
                 ⇝ "func call(method: String, arguments: " ⧺ jsonT ⧺ ","
                 ⇝ s 10 ⧺ "completion: " ⧺ jsonT ⧺ " -> Void) -> CancellationToken"
                 ↝ "}"

s ∷ Int → String -- n spaces
s = concat ∘ flip take (repeat " ")

infixr 5 ↝, ⇝, ⟿
joinLinesWithIndent n l l' = l ⧺ '\n':s n ⧺ l'
(↝) = joinLinesWithIndent 0
(⇝) = joinLinesWithIndent 4
(⟿ ) = joinLinesWithIndent 8

