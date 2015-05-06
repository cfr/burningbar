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
        parseReply | t ≡ (Typename "Void") = " _ in }"
                   | otherwise = "\n" ⧺ s 8 ⧺ "let v = " ⧺ initNewtype t "$0" ⧺ "\n"
                               ⧺ s 8 ⧺ "completion(v)\n" ⧺ s 6 ⧺ "}"
        list = (args ≫=)

record (Record name vars) = "public struct " ⧺ name ⧺ " {\n"
                          ⧺ concat decls ⧺ "}\n\n"
  where decls = initDecl : map varDecl vars
        initDecl = s 4 ⧺ "public init(_ json: " ⧺ jsonT ⧺ ") {\n"
                 ⧺ concatMap initVar vars ⧺ s 4 ⧺ "}\n"
                 -- TODO: public toJSON() -> JSON

varDecl (Variable n t) = s 4 ⧺ "public let " ⧺ n +:+ t ⧺ "\n"

initVar (Variable n (Optional t)) | t ∈ primitives = initPrimitive (Optional t) n
                                  -- n = json["n"] as? T
                                  | otherwise = withOptionalJSON n (initNewtype t)
                                  -- if let j = json["n"] as? JSON { n = T(j) } else { n = nil }
initVar (Variable n d@(Dictionary k t)) | t ∈ primitives = initPrimitive (Dictionary k t) n
                                        | otherwise = s 8 ⧺ "var d" +=+ fromType d ⧺ "()\n"
                                                    ⧺ s 8 ⧺ mapJSON n d
                                                    ⧺ s 8 ⧺ n +=+ "d\n"
initVar (Variable n (Array t))    | t ∈ primitives = initPrimitive (Array t) n
                                  | otherwise = s 8 ⧺ n +=+ mapJSON n (Array t)
initVar (Variable n t)            | t ∈ primitives = initPrimitive t n -- n = json["n"] as! T
                                  | otherwise = s 8 ⧺ n +=+ initNewtype t (sub n ⧺ " as! " ⧺ jsonT) ⧺ "\n"
                                  -- n = T(json as! JSON)
initWithElem n = s 8 ⧺ n +=+ sub n ⧺ " as"
initNewtype t from = fromType t ⧺ "(" ⧺ from ⧺ ")"
initPrimitive (Optional t) n = initWithElem n ⧺ "? " ⧺ fromType t ⧺ "\n"
initPrimitive t n = initWithElem n ⧺ "! " ⧺ fromType t ⧺ "\n"
withOptionalJSON n init = s 8 ⧺ "if let j" +=+ sub n ⧺ " as? " ⧺ jsonT ⧺ ""
                          ⧺ " { " ⧺ n +=+ init "j" ⧺ "} else { " ⧺ n +=+ "nil }\n"
mapJSON n t = "map(" ⧺ castArr ⧺ ") {" ⧺ closure ⧺ "}\n"
  where castArr | Array _ ← t = sub n ⧺ " as! [" ⧺ jsonT ⧺ "]"
                | otherwise = "json.keys"
        closure | Array t' ← t = initNewtype t' "$0"
                | Dictionary k v ← t = "(k" +:+ k ⧺ ") in d[k]"
                                     +=+ initNewtype v ("json[k] as! " ⧺ jsonT)

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
                         ⧺ s 4 ⧺ "public class func call(method: String, _ args: "
                               ⧺ jsonT ⧺ ") -> " ⧺ jsonT ⧺ " {\n"
                         ⧺ s 8 ⧺ "print(\"calling \\(method) with \\(args.description)\")\n"
                         ⧺ s 8 ⧺ "return [:]\n"  ⧺  s 4 ⧺ "}\n"  ⧺  "}\n\n"
               | otherwise = ""

entitiesWrap ∷ String → String
entitiesWrap = foundation
foundation = ("import Foundation\n\n" ⧺)

s ∷ Int → String -- n spaces
s = concat ∘ flip take (repeat " ")

