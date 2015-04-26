{-# LANGUAGE UnicodeSyntax #-}
module Swift (swift) where

import Prelude.Unicode

import Language

swift ∷ Language
swift = Language tbd tbd fromType fromRecord etcSwift where
 tbd = const "// TBD"
 fromRecord (Record name vars) = "public struct " ⧺ name ⧺ " {\n"
                               ⧺ concat decls ⧺ "}\n"
  where decls = initDecl : (map varDecl vars)
        initDecl = s 4 ⧺ "public init(json: JSON) {\n"
                 ⧺ concatMap initVar vars ⧺ s 4 ⧺ "}\n"
        initVar (Variable n (Optional t)) = initWithElem n ⧺ "? "
                                          ⧺ fromType t ⧺ "\n"
        initVar (Variable n t) = initWithElem n ⧺ "! " ⧺ fromType t ⧺ "\n"
        initWithElem n = s 8 ⧺ n ⧺ " = json[\"" ⧺ n ⧺ "\"] as"
        varDecl (Variable n t) = s 4 ⧺ "public let " ⧺ n
                               ⧺ ": " ⧺ fromType t ⧺ "\n"
        s = concat ∘ flip take spaces
        spaces = repeat " "
 fromType (Array t) = "[" ⧺ fromType t ⧺ "]"
 fromType (Optional t) = fromType t ⧺ "?"
 fromType (Dictionary tk tv) = "[" ⧺ fromType tk ⧺ " : " ⧺ fromType tv ⧺ "]"
 fromType (Typename typename) = typename
 etcSwift = "public typealias JSON = Dictionary<String, Any>\n"

