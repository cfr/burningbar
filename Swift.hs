{-# LANGUAGE UnicodeSyntax #-}
module Swift (swift, dataWrap, rpcWrap) where
                     -- TODO: put wrappers in Lang

import Language hiding (function, record)

import Prelude.Unicode
import Control.Monad.Unicode

swift = Language function record

rpcWrap genRPCStub rpcName rpc = header genRPCStub ⧺ "public extension "
                               ⧺ rpcName ⧺ " {\n" ⧺ rpc ⧺ "}\n"
dataWrap d = d

function (Function name rpc args t) = "  public func " ⧺ name
                                    ⧺ "(" ⧺ argList ⧺ "completion: (" ⧺ fromType t ⧺ " -> Void))"
                                    ⧺ " -> " ⧺ "Void"
                                    ⧺ " {\n" ⧺ body ⧺ "" ⧺ "\n" ⧺ "  }\n"
  where body = s 6 ⧺ "call(\"" ⧺ rpc ⧺ "\", [" ⧺ passedArgs ⧺ "]) {" ⧺ parseReply
        argList | args ≡ [] = "" -- FIXME: keep arg list order
                | otherwise = list fromArg
        fromArg (Variable n t) = n ⧺ ": " ⧺ fromType t ⧺ ", "
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
        initDecl = s 4 ⧺ "public init(_ json: " ⧺ json ⧺ ") {\n"
                 ⧺ concatMap initVar vars ⧺ s 4 ⧺ "}\n"
                 -- TODO: public toJSON() -> JSON

varDecl (Variable n t) = s 4 ⧺ "public let " ⧺ n
                       ⧺ ": " ⧺ fromType t ⧺ "\n"

initVar (Variable n (Optional t)) | t ∈ primitives = initPrimitive (Optional t) n
                                  -- n = json["n"] as? T
                                  | otherwise = withOptionalJSON n (initNewtype t)
                                  -- if let j = json["n"] as? JSON { n = T(j) } else { n = nil }
initVar (Variable n (Array t))    | t ∈ primitives = initPrimitive (Array t) n
                                  | otherwise = s 8 ⧺ n +=+ "map(" ⧺ sub n ⧺ " as! [" ⧺ json ⧺ "]) {"
                                              ⧺ initNewtype t "$0" ⧺ "}\n"
                                  -- if let j = json["n"] as? JSON { n = T(j) } else { n = nil }
initVar (Variable n t)            | t ∈ primitives = initPrimitive t n
                                  -- n = json["n"] as! T
                                  | otherwise = s 8 ⧺ n +=+ initNewtype t (sub n ⧺ "as! " ⧺ json) ⧺ "\n"
                                  -- n = T(json as! JSON)
initWithElem n = s 8 ⧺ n +=+ sub n ⧺ " as"
initNewtype t from = fromType t ⧺ "(" ⧺ from ⧺ ")"
initPrimitive (Optional t) n = initWithElem n ⧺ "? " ⧺ fromType t ⧺ "\n"
initPrimitive t n = initWithElem n ⧺ "! " ⧺ fromType t ⧺ "\n"
withOptionalJSON n init = s 8 ⧺ "if let j" +=+ sub n ⧺ " as? " ⧺ json ⧺ ""
                          ⧺ " { " ⧺ n +=+ init "j" ⧺ "} else { " ⧺ n +=+ "nil }\n"

n +=+ v = n ⧺ " = " ⧺ v
json = "[String: AnyObject]"
sub k = "json[\"" ⧺ k ⧺ "\"]"

primitives = foldr (=≪) atoms [opt, dict, opt, ap Array, opt] -- FIXME: more?
  where atoms = map Typename ["String", "Bool", "Int", "Float"]
        ap = (take 2 ∘) ∘ iterate
        opt = ap Optional
        dict a = a : [Dictionary x a | x ← atoms]

fromType (Array t) = "[" ⧺ fromType t ⧺ "]"
fromType (Optional t) = fromType t ⧺ "?"
fromType (Dictionary tk tv) = "[" ⧺ fromType tk ⧺ " : " ⧺ fromType tv ⧺ "]"
fromType (Typename typename) = typename

header False = ""
header True  = "public class RPC {\n"
             ⧺ s 4 ⧺ "public class func call(method: String, _ args: " ⧺ json ⧺ ") -> " ⧺ json ⧺ " {\n"
             ⧺ s 8 ⧺ "print(\"calling \\(method) with \\(args.description)\")\n"
             ⧺ s 8 ⧺ "return [:]\n"
             ⧺ s 4 ⧺ "}\n"
             ⧺ "}\n\n"

s = concat ∘ flip take (repeat " ")

