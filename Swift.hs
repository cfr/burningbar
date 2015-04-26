{-# LANGUAGE UnicodeSyntax #-}
module Swift (swift) where

import Prelude.Unicode

import Language

swift ∷ Language
swift = Language (const "") fromFunc fromType fromRecord etcSwift

fromFunc (Function name rpc args t) = "public extension RPC {\n"
                                    ⧺ "  public class func " ⧺ name
                                    ⧺ "(" ⧺ concatMap fromArg args ⧺ ")"
                                    ⧺ " -> " ⧺ fromType t ⧺ " {\n"
                                    ⧺ body ⧺ "" ⧺ "\n" ⧺ "  }\n}\n\n"
  where fromArg = const ""
        body = s 4 ⧺ "call([:], \"" ⧺ rpc ⧺ "\")"


fromRecord (Record name vars) = "public struct " ⧺ name ⧺ " {\n"
                              ⧺ concat decls ⧺ "}\n\n"
  where decls = initDecl : (map varDecl vars)
        initDecl = s 4 ⧺ "public init(json: JSON) {\n"
                 ⧺ concatMap initVar vars ⧺ s 4 ⧺ "}\n"
                 -- TODO: public toJSON() -> JSON
        initVar (Variable n (Optional t)) = initWithElem n ⧺ "? "
                                          ⧺ fromType t ⧺ "\n"
        initVar (Variable n t) = initWithElem n ⧺ "! " ⧺ fromType t ⧺ "\n"
        initWithElem n = s 8 ⧺ n ⧺ " = json[\"" ⧺ n ⧺ "\"] as"
        varDecl (Variable n t) = s 4 ⧺ "public let " ⧺ n
                               ⧺ ": " ⧺ fromType t ⧺ "\n"

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

