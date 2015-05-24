{-# LANGUAGE UnicodeSyntax #-}
module Swift where

import Data.Char (toLower, toUpper)

import Static
import Language hiding (generate)
import Util

swift ∷ Typename → Typename → Language
swift transport interface = Language generate (entDefs ↝ ) (intDefs transport interface)

generate ∷ Declaration → String
generate (Record name vars super) = struct name vars super
generate method = func name remoteName args rawRetType
    where (Method remoteName rawRetType name args) = method

func name rpc args t = "" ⇝ "public func " ⧺ name -- TODO: tf: t -> R = idTf
                       ⧺ "(" ⧺ argList ⧺ "completion: " ⧺ fromType t ⧺ " -> Void)"
                       ⧺ " -> T.CancellationToken"
                       ⧺ " {" ↝ body ⇝ "}"
                       ⇝ "public let " ⧺ name ⧺ ": String = \"" ⧺ name ⧺ "\""
  where body = s 6 ⧺ "return transport.call(\"" ⧺ rpc ⧺ "\", arguments: " ⧺ passedArgs ⧺ ") { " ⧺ mapReply
        argList = args ≫= fromArg
        fromArg (Variable n t _) = n ⧺ ": " ⧺ fromType t ⧺ ", "
        passedArgs = constructDict varOrNull args
        passArg (Variable n _ _) = "\"" ⧺ n ⧺ "\": " ⧺ n ⧺ " as Any, "
        mapReply | t ≡ Typename "Void" = " _ in }"
                 | otherwise = "r in" ⟿  "let v = " ⧺ fromType t ⧺ "(r)"
                               ⟿  "completion(v)" ↝ s 6 ⧺ "}"
constructDict ∷ (Variable → String) → [Variable] → String
constructDict rule vars | null vars = "[:]"
                        | otherwise = "[" ⧺ (init ∘ init ∘ (vars ≫=)) rule ⧺ "]"
varOrNull (Variable n t _) = "\"" ⧺ n ⧺ "\": " ⧺ unwrap ⧺ ", "
  where toObj = if primitive t then n else n ++ ".asJSON"
        unwrap | Optional t' ← t = "(" ⧺ toObj ⧺ " ?? \"null\")"
               | otherwise = toObj

struct name vars super = "public struct " ⧺ name ⧺ conforms ⧺ " {" ↝ concat decls ⧺ "}\n\n"
  where decls = initDecl : statics : map varDecl vars
        conforms | (Just s) ← super = jsonProtocols ⧺ ", " ⧺ s
                 | otherwise = jsonProtocols
                 where jsonProtocols = " : JSONEncodable, JSONDecodable"
        initDecl = s 4 ⧺ "public let asJSON: " ⧺ jsonT
                   ⇝ "public init(_ json: " ⧺ jsonT ⧺ ") {" ⟿  "asJSON = json"
                   ↝ list initVar ⧺ s 4 ⧺ "}\n"
        statics = s 4 ⧺ "public static let Name = \"" ⧺ name ⧺ "\""
                  ⇝ "public let Name = \"" ⧺ name ⧺ "\""
                  ↝ list staticName
        staticName (Variable n _ _) = s 4 ⧺ "public static let " ⧺ n ⧺ " = \"" ⧺ n ⧺ "\"\n"
        varDecl (Variable n t dv) = s 4 ⧺ "public let " ⧺ n ⧺ ": " ⧺ fromType t ⧺ defVal ⧺ "\n"
                    where defVal = maybe "" (" = " ⧺) dv
        initVar (Variable n (Optional t) _) = ""
        initVar _ = ""
        list = (vars ≫=)
        capitalize (c:cs) = toUpper c : cs

fromType (Array t) = "[" ⧺ fromType t ⧺ "]"
fromType (Optional t) = fromType t ⧺ "?"
fromType (Dictionary tk tv) = "[" ⧺ fromType tk ⧺ ": " ⧺ fromType tv ⧺ "]"
fromType (Typename typename) = typename

s ∷ Int → String -- n spaces
s = concat ∘ flip take (repeat " ")

jsonT = "[String : AnyObject]"
sub k = "json[\"" ⧺ k ⧺ "\"]"

infixr 5 ↝, ⇝, ⟿
joinLinesWithIndent n l l' = l ⧺ '\n' : s n ⧺ l'
(↝) = joinLinesWithIndent 0
(⇝) = joinLinesWithIndent 4
(⟿ ) = joinLinesWithIndent 8

