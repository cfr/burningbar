{-# LANGUAGE UnicodeSyntax #-}
module Swift where

import Static
import Language hiding (generate)
import Util

swift ∷ Typename → Typename → Language
swift transport interface = Language generate (entDefs ⧺) (intDefs transport interface)

generate ∷ Declaration → String
generate (Record name vars super) = struct name vars super
generate method = func name remoteName args rawRetType
    where (Method remoteName rawRetType name args) = method

func name rpc args t = s 4 ⧺ "public func " ⧺ name ⧺ "(" ⧺ (args ≫= fromArg)
                       ⧺ "tf: (" ⧺ ret ⧺ " -> " ⧺ ret ⧺ ") = idTf, "
                       ⧺ "completion: " ⧺ ret ⧺ " -> Void)" ⧺ " -> T.CancellationToken"
                       ⧺ " {\n" ⧺  body ⧺ "\n" ⧺ s 4 ⧺ "}\n"
                       ⧺ s 4 ⧺ "public let " ⧺ name ⧺ ": String = \"" ⧺ name ⧺ "\"\n\n"
  where body = s 6 ⧺ "return transport.call(\"" ⧺ rpc ⧺ "\", arguments: " ⧺ passedArgs ⧺ ") { " ⧺ mapReply
        fromArg (Variable n t _) = n ⧺ ": " ⧺ fromType t ⧺ ", "
        ret = fromType t
        passedArgs = constructDict varOrNull args
        mapReply | t ≡ Typename "Void" = " _ in }"
                 | otherwise = "r in\n" ⧺ s 8 ⧺ "let v = " ⧺ fromType t ⧺ "(r)\n"
                                ⧺ s 8 ⧺ "completion(v)\n" ⧺ s 6 ⧺ "}"
constructDict ∷ (Variable → String) → [Variable] → String
constructDict rule vars | null vars = "[:]"
                        | otherwise = "[" ⧺ (drop2 ∘ (vars ≫=)) rule ⧺ "]"
varOrNull (Variable n t _) = "\"" ⧺ n ⧺ "\": " ⧺ unwrap ⧺ ", "
  where toObj = if primitive t then n else n ++ ".json"
        unwrap | Optional t' ← t = "(" ⧺ toObj ⧺ " ?? \"null\")"
               | otherwise = toObj

struct name vars super = "\npublic struct " ⧺ name ⧺ conforms ⧺ " {\n" ⧺ concat decls ⧺ "}\n"
  where decls = create : optInit : initDecl : statics : map varDecl vars'
        conforms | (Just s) ← super = jsonProtocols ⧺ ", " ⧺ s
                 | otherwise = jsonProtocols
                 where jsonProtocols = " : JSONEncodable, JSONDecodable"
        vars' = Variable "json" (Typename jsonT) Nothing : vars
        create = s 4 ⧺ "public static create" ⧺ list curriedArg ⧺ " {\n"
                 ⧺ s 8 ⧺ "return " ⧺ name ⧺ "(" ⧺ drop2 (list passArg) ⧺ ")\n"
                 ⧺ s 4 ⧺ "}\n"
        passArg (Variable n _ _) = n ⧺ ": " ⧺ n ⧺ ", "
        curriedArg (Variable n t _) = "(" ⧺ n ⧺ ": " ⧺ fromType t ⧺ ")"
        optInit = s 4 ⧺ "public init?(_ json: " ⧺ jsonT ⧺ ") {\n"
                  ⧺ s 8 ⧺ "if let v = (" ⧺ name ⧺ ".create(json), json) " ⧺ init (list mapVar)
                  ⧺ " { self = t } else { return nil }\n" ⧺ s 4 ⧺ "}\n"
        mapVar (Variable n (Optional t) dv) = "</? \"" ⧺ n ⧺ "\" " -- TODO: use default value
        mapVar (Variable n _ _) = "</ \"" ⧺ n ⧺ "\" "
        initDecl = s 4 ⧺ "public init(" ⧺ drop2 (list arg) ⧺ ") {\n"
                   ⧺ s 8 ⧺ drop2 (list initVar) ⧺ "\n" ⧺ s 4 ⧺ "}\n"
        arg (Variable n t _) = n ⧺ ": " ⧺ fromType t ⧺ ", "
        statics = s 4 ⧺ "public let Name = \"" ⧺ name ⧺ "\"\n"
                  ⧺ s 4 ⧺ "public static let Name = \"" ⧺ name ⧺ "\"\n"
                  ⧺ list staticName ⧺ "\n"
        staticName (Variable n _ _) = s 4 ⧺ "public static let " ⧺ n ⧺ " = \"" ⧺ n ⧺ "\"\n"
        varDecl (Variable n t dv) = s 4 ⧺ "public let " ⧺ n ⧺ ": " ⧺ fromType t ⧺ defVal ⧺ "\n"
                    where defVal = maybe "" (" = " ⧺) dv
        initVar (Variable n _ _) = "self." ⧺ n ⧺ " = " ⧺ n ⧺ "; "
        list = (vars' ≫=)

fromType (Array t) = "[" ⧺ fromType t ⧺ "]"
fromType (Optional t) = fromType t ⧺ "?"
fromType (Dictionary tk tv) = "[" ⧺ fromType tk ⧺ ": " ⧺ fromType tv ⧺ "]"
fromType (Typename typename) = typename

drop2 ∷ String → String
drop2 = init ∘ init

s ∷ Int → String -- n spaces
s = concat ∘ flip take (repeat " ")

jsonT = "[String : AnyObject]"
sub k = "json[\"" ⧺ k ⧺ "\"]"

