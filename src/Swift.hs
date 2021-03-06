{-# LANGUAGE UnicodeSyntax, CPP #-}
module Swift where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)

import Static
import Language hiding (generate)
import Util

swift ∷ Bool → Typename → Typename → Language
swift overload transport interface = Language generate wrapEnts (intDefs transport interface)
  where wrapEnts e = "\nimport Foundation\n" ⧺ e ⧺ entDefs
                     ⧺ if overload then overloaded else []

generate ∷ Declaration → String
generate (Record idr vars super) = struct idr vars super
generate method = func name remoteName args rawRetType
    where (Method (Identifier name remoteName) args rawRetType) = method

func ∷ Name → Name → [Variable] → Type → String
func name rpc args t = s 4 ⧺ "public func " ⧺ name ⧺ "(" ⧺ list fromArg [] args ⧺ tf
                       ⧺ "completion: " ⧺ ret ⧺ " -> Void)" ⧺ " -> T.CancellationToken"
                       ⧺ " {\n" ⧺ body ⧺ "\n" ⧺ s 4 ⧺ "}\n"
                       ⧺ s 4 ⧺ "public let " ⧺ name ⧺ ": String = \"" ⧺ name ⧺ "\"\n\n"
  where body = s 6 ⧺ "return transport.call(\"" ⧺ rpc
               ⧺ "\", arguments: " ⧺ passedArgs ⧺ ") { " ⧺ mapReply
        fromArg (Variable n t _) = n ⧺ ": " ⧺ fromType t ⧺ ", "
        noRet = (t ≡ TypeName "Void") ∨ (t ≡ TypeName "()")
        ret = if noRet then "Void" else fromType t ⧺ "?"
        tf = if noRet then [] else "tf: ((" ⧺ ret ⧺ ", [String : AnyObject]) -> " ⧺ ret ⧺ ") = idTf, "
        passedArgs = if null args then "[:]" else "[" ⧺ list serialize ", " args ⧺ "]"
        serialize (Variable n t _) = "\"" ⧺ n ⧺ "\": " ⧺ if primitive t then n else n ⧺ ".json"
        mapReply = if noRet then " _ in }"
                   else "r in\n" ⧺ s 8 ⧺ "let v = tf(" ⧺ fromType t ⧺ "(json: r), r)\n"
                        ⧺ s 8 ⧺ "completion(v)\n" ⧺ s 6 ⧺ "}"


struct ∷ Identifier → [Variable] → Maybe Typename → String
struct (Identifier name remote) vars super = "\npublic struct " ⧺ name ⧺ conforms ⧺ " {\n"
                         ⧺ concat decls ⧺ "}\n" ⧺ equatable
  where decls = map varDecl vars' ⧺ [statics, optInit, initDecl, create, description]
        conforms | (Just s) ← super = jsonProtocols ⧺ ", " ⧺ s
                 | otherwise = jsonProtocols
                 where jsonProtocols = ": JSONEncodable, JSONDecodable"
        protocols = map trim (separateBy ',' (fromMaybe [] super))
        equatable = if "Equatable" ∉ protocols then [] else
                       "public func == (lhs: " ⧺ name ⧺ " , rhs: " ⧺ name ⧺ " ) -> Bool { "
                       ⧺ "return lhs.json.description == rhs.json.description }\n"
        description = if "Printable" ∉ protocols then [] else
                         s 4 ⧺ "public var description: String"
                         ⧺ " { return Name + \": \" + json.description }\n"
        vars' = Variable "json" (TypeName "[String : AnyObject]") Nothing : vars
        create = s 4 ⧺ "static func create" ⧺ list curriedArg "" vars' ⧺ " -> " ⧺ name ⧺ " {\n"
                 ⧺ s 8 ⧺ "return " ⧺ name ⧺ "(json" ⧺ (if null vars then "" else ", ")
                 ⧺ list passArg ", " vars ⧺ ")\n" ⧺ s 4 ⧺ "}\n"
        passArg (Variable n _ _) = n ⧺ ": " ⧺ n
        curriedArg (Variable n t _) = "(" ⧺ n ⧺ ": " ⧺ fromType t ⧺ ")"
        optInit = s 4 ⧺ "public init?(json: [String : AnyObject]) {\n"
                  ⧺ s 8 ⧺ "let (c, json) = (" ⧺ name ⧺ ".create(json), json)\n"
                  ⧺ batchOps vars 0 ⧺ s 4 ⧺ "}\n"
        mapVar (Variable n (Optional t) dv) = "~~? \"" ⧺ n ⧺ "\"" -- TODO: use default value
        mapVar (Variable n _ _) = "~~ \"" ⧺ n ⧺ "\""
        maxOpsInS = 3 -- NOTE: swiftc can't parse long op chains, batchOps should be list mapVar " "
        batchOps [] 0 = s 8 ⧺ "self = c; return\n"
        batchOps vars 0 = batchOps vars 1 ⧺ s 8 ⧺ "return nil\n"
        batchOps vars l = let ind = 6 + 2*l in if length vars > maxOpsInS
             then s ind ⧺ "if let (c" ⧺ n_ l ⧺ ", json) " ⧺ "= (c" ⧺ n_ (l-1) ⧺ ", json) "
                  ⧺ " " ⧺ list mapVar " " (take maxOpsInS vars) ⧺ " {\n" ⧺ dshow (take maxOpsInS vars)
                  ⧺ batchOps (drop maxOpsInS vars) (l+1) ⧺ s ind ⧺ "}\n"
             else s ind ⧺ "if let c" ⧺ n_ l ⧺ " = (c" ⧺ n_ (l-1) ⧺ ", json)"
                  ⧺ " " ⧺ list mapVar " " vars ⧺ " { self = c" ⧺ n_ l ⧺ "; return }\n"
        initDecl = s 4 ⧺ "public init(_ json: [String : AnyObject]" ⧺ (if null vars then "" else ", ")
                   ⧺ list arg ", " vars ⧺ ") {\n" ⧺ s 8 ⧺ list initVar "; " vars'
                   ⧺ "; self.Name = \"" ⧺ name ⧺ "\"\n" ⧺ s 4 ⧺ "}\n"
        initVar (Variable n _ _) = "self." ⧺ n ⧺ " = " ⧺ n
        arg (Variable n t _) = n ⧺ ": " ⧺ fromType t
        statics = s 4 ⧺ "public let Name: String\n"
                  ⧺ s 4 ⧺ "public static let Name = \"" ⧺ name ⧺ "\"\n"
                  ⧺ s 4 ⧺ "public static let remoteName = \"" ⧺ remote ⧺ "\"\n"
                  ⧺ list staticName "\n" vars' ⧺ "\n"
        staticName (Variable n _ _) = s 4 ⧺ "public static let " ⧺ n ⧺ " = \"" ⧺ n ⧺ "\""
        varDecl (Variable n t dv) = s 4 ⧺ "public let " ⧺ n ⧺ ": " ⧺ fromType t ⧺ defVal ⧺ "\n"
                    where defVal = "" -- maybe "" (" = " ⧺) dv
dshow x =
#ifdef DEBUG
    "/* " ⧺ show x ⧺ " */\n"
#else
    []
#endif

list ∷ (α → String) → String → [α] → String
list gen separator = intercalate separator ∘ map gen

fromType ∷ Type → String
fromType (Array t) = "[" ⧺ fromType t ⧺ "]"
fromType (Optional t) = fromType t ⧺ "?"
fromType (Dictionary tk tv) = "[" ⧺ fromType tk ⧺ ": " ⧺ fromType tv ⧺ "]"
fromType (TypeName typename) = typename

