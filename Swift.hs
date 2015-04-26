{-# LANGUAGE UnicodeSyntax #-}
module Swift (swift, primitives) where

import Prelude.Unicode
import Control.Monad.Unicode
import Data.Bool.Unicode

import Language

swift ∷ Language
swift = Language varDecl fromFunc fromType fromRecord etcSwift

fromFunc (Function name rpc args t) = "public extension RPC {\n"
                                    ⧺ "  public class func " ⧺ name
                                    ⧺ "(" ⧺ concatMap fromArg args ⧺ ")"
                                    ⧺ " -> " ⧺ fromType t ⧺ " {\n"
                                    ⧺ body ⧺ "" ⧺ "\n" ⧺ "  }\n}\n\n"
  where fromArg = const ""
        body = s 6 ⧺ "call([:], \"" ⧺ rpc ⧺ "\")"


fromRecord (Record name vars) = "public struct " ⧺ name ⧺ " {\n"
                              ⧺ concat decls ⧺ "}\n\n"
  where decls = initDecl : (map varDecl vars)
        initDecl = s 4 ⧺ "public init(_ json: JSON) {\n"
                 ⧺ concatMap initVar vars ⧺ s 4 ⧺ "}\n"
                 -- TODO: public toJSON() -> JSON


varDecl (Variable n t) = s 4 ⧺ "public let " ⧺ n
                       ⧺ ": " ⧺ fromType t ⧺ "\n"

initVar (Variable n (Optional t)) | t ∈ primitives = initWithElem n ⧺ "? " ⧺ fromType t ⧺ "\n"
                                  | otherwise = s 8 ⧺ n ⧺ " = " ⧺ accJSON n ⧺ " == nil ? nil : "
                                              ⧺ fromType t ⧺ "(" ⧺ accJSON n ⧺ " as! JSON)\n"
initVar (Variable n t)            | t ∈ primitives = initWithElem n ⧺ "! " ⧺ fromType t ⧺ "\n"
                                  | otherwise = s 8 ⧺ n ⧺ " = " ⧺ fromType t
                                              ⧺ "(" ⧺ accJSON n ⧺ " as! JSON)\n"
initWithElem n = s 8 ⧺ n ⧺ " = " ⧺ accJSON n ⧺ " as"
accJSON k = "json[\"" ⧺ k ⧺ "\"]"

primitives = atoms ≫= opt ≫= ap Array ≫= opt ≫= dict ≫= opt
  where atoms = map Typename ["String", "Bool", "Int", "Float"]
        ap = (take 2 ∘) ∘ iterate
        opt = ap Optional
        dict a = a : [Dictionary x a | x ← atoms]

fromType (Array t) = "[" ⧺ fromType t ⧺ "]"
fromType (Optional t) = fromType t ⧺ "?"
fromType (Dictionary tk tv) = "[" ⧺ fromType tk ⧺ " : " ⧺ fromType tv ⧺ "]"
fromType (Typename typename) = typename

etcSwift = "public typealias JSON = Dictionary<String, Any>\n\n"
         ⧺ "public class RPC {\n"
         ⧺ s 4 ⧺ "public class func call(args: JSON, _ method: String) -> JSON {\n"
         ⧺ s 8 ⧺ "print(\"calling \\(method) with \\(args.description)\")\n"
         ⧺ s 8 ⧺ "return [:]\n"
         ⧺ s 4 ⧺ "}\n"
         ⧺ "}\n\n"

s = concat ∘ flip take spaces
spaces = repeat " "

