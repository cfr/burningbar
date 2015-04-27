{-# LANGUAGE UnicodeSyntax #-}
module Swift (swift) where

import Language hiding (function, record, header)

import Prelude.Unicode
import Control.Monad.Unicode
import Data.Bool.Unicode

swift = Language function record header

function (Function name rpc args t) = "public extension RPC {\n"
                                    ⧺ "  public class func " ⧺ name
                                    ⧺ "(" ⧺ argList ⧺ ")"
                                    ⧺ " -> " ⧺ "Void" -- FIXME: fromType t
                                    ⧺ " {\n" ⧺ body ⧺ "" ⧺ "\n" ⧺ "  }\n}\n\n"
  where body = s 6 ⧺ "call(\"" ⧺ rpc ⧺ "\", [" ⧺ passedArgs ⧺ "])"
        argList | args ≡ [] = "" -- FIXME: keep arg list order
                | otherwise = list fromArg
        fromArg (Variable n t) = n ⧺ ": " ⧺ fromType t ⧺ ", "
        passedArgs | args ≡ [] = ":"
                   | otherwise = list passArg
        passArg (Variable n _) = "\"" ⧺ n ⧺ "\": " ⧺ n ⧺ " ,"
        list = init ∘ init ∘ (args ≫=)

record (Record name vars) = "public struct " ⧺ name ⧺ " {\n"
                          ⧺ concat decls ⧺ "}\n\n"
  where decls = initDecl : map varDecl vars
        initDecl = s 4 ⧺ "public init(_ json: JSON) {\n"
                 ⧺ concatMap initVar vars ⧺ s 4 ⧺ "}\n"
                 -- TODO: public toJSON() -> JSON

varDecl (Variable n t) = s 4 ⧺ "public let " ⧺ n
                       ⧺ ": " ⧺ fromType t ⧺ "\n"

initVar (Variable n (Optional t)) | t ∈ primitives = initWithElem n ⧺ "? " ⧺ fromType t ⧺ "\n"
                                  -- n = json["n"] as? T
                                  | otherwise = s 8 ⧺ "if let j = " ⧺ sub n ⧺ " as? JSON"
                                              ⧺ " { " ⧺ n ⧺ " = " ⧺ fromType t ⧺ "(j) }"
                                              ⧺ " else { " ⧺ n ⧺ " = nil }\n"
                                  -- if let j = json["n"] as? JSON { n = T(j) } else { n = nil }
initVar (Variable n t)            | t ∈ primitives = initWithElem n ⧺ "! " ⧺ fromType t ⧺ "\n"
                                  -- n = json["n"] as! T
                                  | otherwise = s 8 ⧺ n ⧺ " = " ⧺ fromType t
                                              ⧺ "(" ⧺ sub n ⧺ " as! JSON)\n"
                                  -- n = T(json)
initWithElem n = s 8 ⧺ n ⧺ " = " ⧺ sub n ⧺ " as"
sub k = "json[\"" ⧺ k ⧺ "\"]"

primitives = atoms ≫= opt ≫= ap Array ≫= opt ≫= dict ≫= opt -- FIXME: more?
  where atoms = map Typename ["String", "Bool", "Int", "Float"]
        ap = (take 2 ∘) ∘ iterate
        opt = ap Optional
        dict a = a : [Dictionary x a | x ← atoms]

fromType (Array t) = "[" ⧺ fromType t ⧺ "]"
fromType (Optional t) = fromType t ⧺ "?"
fromType (Dictionary tk tv) = "[" ⧺ fromType tk ⧺ " : " ⧺ fromType tv ⧺ "]"
fromType (Typename typename) = typename

header = "public typealias JSON = Dictionary<String, AnyObject>\n\n"
       ⧺ "public class RPC {\n"
       ⧺ s 4 ⧺ "public class func call(method: String, _ args: JSON) -> JSON {\n"
       ⧺ s 8 ⧺ "print(\"calling \\(method) with \\(args.description)\")\n"
       ⧺ s 8 ⧺ "return [:]\n"
       ⧺ s 4 ⧺ "}\n"
       ⧺ "}\n\n"

s = concat ∘ flip take (repeat " ")

