{-# LANGUAGE UnicodeSyntax #-}
module Swift (swift, entitiesWrap, interfaceWrap) where

import Language hiding (function, record)

import Prelude.Unicode
import Control.Monad.Unicode

swift = Language function record

function (Function name rpc args t) = "  public func " ⧺ name
                                    ⧺ "(" ⧺ argList ⧺ "completion: (" ⧺ fromType t ⧺ " -> Void))"
                                    ⧺ " -> " ⧺ "Void"
                                    ⧺ " {\n" ⧺ body ⧺ "" ⧺ "\n" ⧺ "  }\n"
  where body = s 6 ⧺ "call(\"" ⧺ rpc ⧺ "\", [" ⧺ passedArgs ⧺ "]) {" ⧺ parseReply
        argList | args ≡ [] = "" -- FIXME: keep arg list order
                | otherwise = list fromArg
        fromArg (Variable n t) = n +:+ t ⧺ ", "
        passedArgs | args ≡ [] = ":"
                   | otherwise = (init ∘ init ∘ list) passArg
        passArg (Variable n _) = "\"" ⧺ n ⧺ "\": " ⧺ n ⧺ " ,"
        parseReply | t ≡ Typename "Void" = " _ in }"
                   | otherwise = "\n" ⧺ s 8 ⧺ "let v = " ⧺ fromType t ⧺ "($0)" ⧺ "\n" -- FIXME: ""/name
                               ⧺ s 8 ⧺ "completion(v)\n" ⧺ s 6 ⧺ "}"
        list = (args ≫=)

record (Record name vars) = "public struct " ⧺ name ⧺ " {\n"
                          ⧺ concat decls ⧺ "}\n\n"
  where decls = initDecl : map varDecl vars
        initDecl = s 4 ⧺ "public init(_ json: " ⧺ jsonT ⧺ ") {\n"
                   ⧺ concatMap initDict vars
                   ⧺ concatMap initVar vars ⧺ s 4 ⧺ "}\n"
                 -- TODO: public toJSON() -> JSON
        initDict (Variable n d@(Dictionary k t)) | t ∉ primitives = s 8 ⧺ n +=+ fromType d ⧺ "()\n"
        initDict _ = ""

varDecl (Variable n t) = s 4 ⧺ "public var " ⧺ n +:+ t ⧺ "\n"

initVar v@(Variable n (Optional t)) | t ∈ primitives = initPrimitive (Optional t) n
                                    -- n = json["n"] as? T
                                    | otherwise = withOptionalJSON n (initNewtype n t)
                                    -- if let j = json["n"] as? JSON { n = N(j) } else { n = nil }
initVar (Variable n d@(Dictionary k t)) | t ∈ primitives = initPrimitive (Dictionary k t) n
                                        | otherwise = s 8 ⧺ mapJSON n d ⧺ "\n"
initVar (Variable n (Array t))    | t ∈ primitives = initPrimitive (Array t) n
                                  | otherwise = s 8 ⧺ n +=+ mapJSON n (Array t) ⧺ "\n"
initVar (Variable n t)            | t ∈ primitives = initPrimitive t n -- n = json["n"] as! T
                                  | otherwise = s 8 ⧺ n +=+ initNewtype n t (sub n ⧺ " as! " ⧺ jsonT) ⧺ "\n"
                                  -- n = T(json as! JSON)
initWithElem n = s 8 ⧺ n +=+ sub n ⧺ " as"
initNewtype n d@(Dictionary _ _) _ = mapJSON (n ⧺ "!") d -- FIXME: expl pass optional
--initNewtype n d@(Array _ ) _ = mapJSON (n ⧺ "!") d
initNewtype n t from = n +=+ fromType t ⧺ "(" ⧺ from ⧺ ")"
initPrimitive (Optional t) n = initWithElem n ⧺ "? " ⧺ fromType t ⧺ "\n"
initPrimitive t n = initWithElem n ⧺ "! " ⧺ fromType t ⧺ "\n"
withOptionalJSON n init = s 8 ⧺ "if let json" +=+ sub n ⧺ " as? " ⧺ jsonT ⧺ ""
                          ⧺ " {\n" ⧺ s 8 ⧺ init "json" ⧺ "\n" ⧺ s 8 ⧺ "} else { " ⧺ n +=+ "nil }\n"
mapJSON n t = "map(" ⧺ castArr ⧺ ") {" ⧺ closure ⧺ "}"
  where castArr | Array _ ← t = sub n ⧺ " as! [" ⧺ jsonT ⧺ "]"
                | Dictionary _ _ ← t = "json.keys"
        closure | Array t' ← t = fromType t' ⧺ "($0)"
                | Dictionary k v ← t = "(k" +:+ k ⧺ ") in self." ⧺ n ⧺ "[k]"
                                       +=+ fromType v ⧺ "(" ⧺ ("json[k] as! " ⧺ jsonT) ⧺ ")"

n +=+ v = n ⧺ " = " ⧺ v
n +:+ t = n ⧺ ": " ⧺ fromType t
jsonT = "[String: AnyObject]"
sub k = "json[\"" ⧺ k ⧺ "\"]"

primitives = foldr (=≪) atoms [opt, dict, opt, ap Array, opt] -- FIXME: more?
  where atoms = map Typename ["String", "Bool", "Int", "Float", "NSNumber"]
        ap = (take 2 ∘) ∘ iterate
        opt = ap Optional
        dict a = a : [Dictionary x a | x ← atoms]

fromType (Array t) = "[" ⧺ fromType t ⧺ "]"
fromType (Optional t) = fromType t ⧺ "?"
fromType (Dictionary tk tv) = "[" ⧺ fromType tk +:+ tv ⧺ "]"
fromType (Typename typename) = typename

interfaceWrap ∷ Bool → Typename → String → String
interfaceWrap intStub intName rpc = foundation header ⧺ "public extension " ⧺ intName ⧺ " {\n" ⧺ rpc ⧺ "}\n"
  where header | intStub = "public class " ⧺ intName ⧺ " {\n"
                         ⧺ s 4 ⧺ "public init() { }\n"
                         ⧺ s 4 ⧺ "public func call(method: String, _ args: "
                               ⧺ jsonT ⧺ ", completion: " ⧺ jsonT ⧺ " -> Void) -> " ⧺ jsonT ⧺ " {\n"
                         ⧺ s 8 ⧺ "print(\"calling \\(method) with \\(args.description)\")\n"
                         ⧺ s 8 ⧺ "return [:]\n" ⧺ s 4 ⧺ "}\n" ⧺ "}\n\n"
               | otherwise = ""

entitiesWrap ∷ String → String
entitiesWrap = foundation
foundation = ("import Foundation\n\n" ⧺)

s ∷ Int → String -- n spaces
s = concat ∘ flip take (repeat " ")

