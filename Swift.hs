{-# LANGUAGE UnicodeSyntax #-}
module Swift (swift, fromType) where

import Language hiding (wrapInterface, wrapEntities, generate)

import Data.Char (toLower, toUpper)
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
        passedArgs = constructDict varOrNull args
        passArg (Variable n _) = "\"" ⧺ n ⧺ "\": " ⧺ n ⧺ " as Any, "
        parseReply | t ≡ Typename "Void" = " _ in }"
                   | otherwise = "r in" ⟿  "let v = " ⧺ fromType t ⧺ "(r)"
                                 ⟿  "completion(v)" ↝ s 6 ⧺ "}"

record name vars = "public struct " ⧺ name ⧺ " : BBSerializable {" ↝ concat decls ⧺ "}\n\n"
  where decls = initDecl : statics : map varDecl vars
        initDecl = s 4 ⧺ "let asDictionary: " ⧺ jsonT
                   ⇝ "public init(_ json: " ⧺ jsonT ⧺ ") {" ⟿  "asDictionary = json"
                   ↝ list initDict ⧺ list initVar ⧺ s 4 ⧺ "}\n"
        statics = s 4 ⧺ "public static let Name = \"" ⧺ name ⧺ "\""
                  ↝ list staticName ↝ list staticPut
        staticName (Variable n _) = s 4 ⧺ "public static let " ⧺ n ⧺ " = \"" ⧺ n ⧺ "\"\n"
        staticPut (Variable n (Optional t)) = staticPut (Variable n t)
        staticPut (Variable n t) = s 4 ⧺ "public static func put" ⧺ capitalize n ⧺ "(_ " ⧺ n ⧺ ": "
                                   ⧺ fromType t ⧺ ") -> (" ⧺ jsonT ⧺ " -> " ⧺ jsonT ⧺ ") {"
                                   ⟿  "return { d in d[\"" ⧺ n ⧺ "\"] = " ⧺ n ⧺ "; return d }"
                                   ⇝ "}\n"
        varDecl (Variable n t) = s 4 ⧺ "public var " ⧺ n ≑ t ⧺ "\n"
        initDict (Variable n d@(Dictionary k t)) | primitive t = ""
                                                 | otherwise = s 8 ⧺ n ⧧ fromType d ⧺ "()\n"
        initDict (Variable n (Optional d)) = initDict (Variable n d)
        initDict _ = ""
        list = (vars ≫=)
        capitalize (c:cs) = toUpper c : cs

constructDict ∷ (Variable → String) → [Variable] → String
constructDict rule vars | null vars = "[:]"
                        | otherwise = "[" ⧺ (init ∘ init ∘ (vars ≫=)) rule ⧺ "]"
varOrNull (Variable n t) = "\"" ⧺ n ⧺ "\": " ⧺ unwrap ⧺ " as AnyObject, "
  where unwrap | Optional t' ← t = "(" ⧺ n ⧺ " ?? \"null\")"
               | otherwise = n

initVar v@(Variable n (Optional t)) | primitive t = initPrimitive (Optional t) n
                                    | otherwise = withOptionalJSON n (initNewtype n t)
initVar (Variable n d@(Dictionary k t)) | primitive t = initPrimitive (Dictionary k t) n
                                        | otherwise = s 8 ⧺ mapJSON n d ⧺ "\n" -- TODO: if let json = ..
initVar (Variable n (Array t)) | primitive t = initPrimitive (Array t) n
                               | otherwise = s 8 ⧺ n ⧧ mapJSON n (Array t) ⧺ "\n" -- TODO: check
initVar (Variable n t) | primitive t = initPrimitive t n
                       | otherwise = s 8 ⧺ n ⧧ initNewtype n t (sub n ⧺ " as! " ⧺ jsonT) ⧺ "\n"

initWithElem n = s 8 ⧺ n ⧧ sub n ⧺ " as"
initNewtype n d@(Dictionary _ _) _ = mapJSON (n ⧺ "!") d -- TODO: check
--initNewtype n d@(Array _ ) _ = mapJSON (n ⧺ "!") d -- TODO: impl
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

fromType (Array t) = "[" ⧺ fromType t ⧺ "]"
fromType (Optional t) = fromType t ⧺ "?"
fromType (Dictionary tk tv) = "[" ⧺ fromType tk ≑ tv ⧺ "]"
fromType (Typename typename) = typename

wrapInterface ∷ Typename → Typename → String → String
wrapInterface transportType interfaceType rpcs = foundation ↝ header
  where header = defTransport transportType ↝ "public class " ⧺ interfaceType
                 ⧺ " <T: " ⧺ transportType ⧺ "> {\n"
                 ⇝ "public init(" ⧺ transport ⧺ ": T) { t = " ⧺ transport ⧺ " }" ↝ rpcs
                 ⇝ "private let t: T" ↝ "}\n"
        decapitalize (c:cs) = toLower c : cs
        transport = decapitalize transportType

wrapEntities ∷ String → String
wrapEntities es = foundation ↝ es
                ↝ "public protocol BBSerializable {" ⇝ "var asDictionary: " ⧺ jsonT ⧺ " { get }"
                ⇝ "static var Name: String { get }" ↝ "}"

foundation = "import Foundation\n"
defTransport t = "public protocol " ⧺ t ⧺ " {" ⇝ "typealias CancellationToken"
                 ⇝ "func call(method: String, arguments: " ⧺ jsonT ⧺ ","
                 ⇝ s 10 ⧺ "completion: " ⧺ jsonT ⧺ " -> Void) -> CancellationToken" ↝ "}"

s ∷ Int → String -- n spaces
s = concat ∘ flip take (repeat " ")

n ⧧ v = n ⧺ " = " ⧺ v
n ≑ t = n ⧺ ": " ⧺ fromType t
jsonT = "[String: AnyObject]"
sub k = "json[\"" ⧺ k ⧺ "\"]"

infixr 5 ↝, ⇝, ⟿
joinLinesWithIndent n l l' = l ⧺ '\n' : s n ⧺ l'
(↝) = joinLinesWithIndent 0
(⇝) = joinLinesWithIndent 4
(⟿ ) = joinLinesWithIndent 8

