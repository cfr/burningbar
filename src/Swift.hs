{-# LANGUAGE UnicodeSyntax #-}
module Swift where

import Data.List (intercalate)

import Static
import Language hiding (generate)
import Util

swift ∷ Bool → Typename → Typename → Language
swift shield transport interface = Language generate wrapEnts (intDefs transport interface)
  where wrapEnts = (⧺ entDefs ⧺ if shield then dynamicityShield else "")

generate ∷ Declaration → String
generate (Record name vars super) = struct name vars super
generate method = func name remoteName args rawRetType
    where (Method remoteName rawRetType name args) = method

func name rpc args t = s 4 ⧺ "public func " ⧺ name ⧺ "(" ⧺ list fromArg [] args ⧺ tf
                       ⧺ "completion: " ⧺ ret ⧺ " -> Void)" ⧺ " -> T.CancellationToken"
                       ⧺ " {\n" ⧺ body ⧺ "\n" ⧺ s 4 ⧺ "}\n"
                       ⧺ s 4 ⧺ "public let " ⧺ name ⧺ ": String = \"" ⧺ name ⧺ "\"\n\n"
  where body = s 6 ⧺ "return transport.call(\"" ⧺ rpc ⧺ "\", arguments: " ⧺ passedArgs ⧺ ") { " ⧺ mapReply
        fromArg (Variable n t _) = n ⧺ ": " ⧺ fromType t ⧺ ", "
        noRet = (t ≡ Typename "Void") ∨ (t ≡ Typename "()")
        ret = if noRet then "Void" else fromType t ⧺ "?"
        tf = if noRet then [] else "tf: ((" ⧺ ret ⧺ ", [String : AnyObject]) -> " ⧺ ret ⧺ ") = idTf, "
        passedArgs = if null args then "[:]" else "[" ⧺ list serialize ", " args ⧺ "]"
        serialize (Variable n t _) = "\"" ⧺ n ⧺ "\": " ⧺ if primitive t then n else n ⧺ ".json"
        mapReply = if noRet then " _ in }"
                   else "r in\n" ⧺ s 8 ⧺ "let v = " ⧺ fromType t ⧺ "(json: r)\n"
                        ⧺ s 8 ⧺ "completion(v)\n" ⧺ s 6 ⧺ "}"


struct name vars super = "\npublic struct " ⧺ name ⧺ conforms ⧺ " {\n" ⧺ concat decls ⧺ "}\n"
  where decls = map varDecl vars' ⧺ [statics, optInit, initDecl, create]
        conforms | (Just s) ← super = jsonProtocols ⧺ ", " ⧺ s
                 | otherwise = jsonProtocols
                 where jsonProtocols = ": JSONEncodable, JSONDecodable"
        vars' = Variable "json" (Typename "[String : AnyObject]") Nothing : vars
        create = s 4 ⧺ "static func create" ⧺ list curriedArg "" vars' ⧺ " -> " ⧺ name ⧺ " {\n"
                 ⧺ s 8 ⧺ "return " ⧺ name ⧺ "(" ⧺ list passArg ", " vars' ⧺ ")\n"
                 ⧺ s 4 ⧺ "}\n"
        passArg (Variable n _ _) = n ⧺ ": " ⧺ n
        curriedArg (Variable n t _) = "(" ⧺ n ⧺ ": " ⧺ fromType t ⧺ ")"
        optInit = s 4 ⧺ "public init?(json: [String : AnyObject]) {\n"
                  ⧺ s 8 ⧺ "let (c, json) = (" ⧺ name ⧺ ".create(json), json)\n"
                  ⧺ batchOps vars 1 ⧺ s 8 ⧺ "return nil\n" ⧺ s 4 ⧺ "}\n"
        mapVar (Variable n (Optional t) dv) = "~~? \"" ⧺ n ⧺ "\"" -- TODO: use default value
        mapVar (Variable n _ _) = "~~ \"" ⧺ n ⧺ "\""
        batchOps vars l = let ind = 6 + 2*l in if length vars > 4
             then s ind ⧺ "if let (c" ⧺ n_ l ⧺ ", json) " ⧺ "= (c" ⧺ n_ (l-1) ⧺ ", json) "
                  ⧺ " " ⧺ list mapVar " " (take 4 vars) ⧺ " {\n" -- NOTE: swiftc can't parse long op chains
                  ⧺ batchOps (drop 4 vars) (l+1) ⧺ s ind ⧺ "}\n" -- should be `ops = list mapVar " "`
             else s ind ⧺ "if let c" ⧺ n_ l ⧺ " = (c" ⧺ n_ (l-1) ++ ", json)"
                  ⧺ " " ⧺ list mapVar " " vars ⧺ " { self = c" ⧺ n_ l ⧺ " }\n"
        initDecl = s 4 ⧺ "public init(" ⧺ list arg ", " vars' ⧺ ") {\n"
                   ⧺ s 8 ⧺ list initVar "; " vars'
                   ⧺ "; self.Name = \"" ⧺ name ⧺ "\"\n" ⧺ s 4 ⧺ "}\n"
        initVar (Variable n _ _) = "self." ⧺ n ⧺ " = " ⧺ n
        arg (Variable n t _) = n ⧺ ": " ⧺ fromType t
        statics = s 4 ⧺ "public let Name: String\n"
                  ⧺ s 4 ⧺ "public static let Name = \"" ⧺ name ⧺ "\"\n"
                  ⧺ list staticName "\n" vars' ⧺ "\n"
        staticName (Variable n _ _) = s 4 ⧺ "public static let " ⧺ n ⧺ " = \"" ⧺ n ⧺ "\""
        varDecl (Variable n t dv) = s 4 ⧺ "public let " ⧺ n ⧺ ": " ⧺ fromType t ⧺ defVal ⧺ "\n"
                    where defVal = "" -- maybe "" (" = " ⧺) dv

list ∷ (α → String) → String → [α] → String
list gen separator = intercalate separator ∘ map gen

fromType (Array t) = "[" ⧺ fromType t ⧺ "]"
fromType (Optional t) = fromType t ⧺ "?"
fromType (Dictionary tk tv) = "[" ⧺ fromType tk ⧺ ": " ⧺ fromType tv ⧺ "]"
fromType (Typename typename) = typename

