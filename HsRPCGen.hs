{-# LANGUAGE ViewPatterns, UnicodeSyntax #-}

module Main where

import Control.Monad (join)
import Data.Map (Map, fromList)
import System.Environment (getArgs)
import System.Console.GetOpt (OptDescr(..), getOpt, ArgOrder(..), ArgDescr(..))

import Translate
import Swift

import qualified Text.JSON (decode)
import Text.JSON hiding (decode)

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Arrow.Unicode
import Control.Applicative.Unicode

hsRPCGenURL = "http://j.mp/HsRPCGen"

main = do
  args ← getArgs
  let (actions, _, _) = getOpt RequireOrder options args --lb
  let o = foldr ($) (defaults) actions
  let copy = (("// Generated with " ⧺ hsRPCGenURL ⧺ "\n\n") ⧺)
  let writeRPC = rpcWriter o ∘ copy ∘ rpcWrap (genRPCStub o) (rpcTypename o)
  let writeData = dataWriter o ∘ copy ∘ dataWrap
  let proc = decode ⋙ translate ⋙ writeData ⁂ writeRPC ⋙ uncurry (≫)
  spec o ≫= proc

data Options = Options { genRPCStub :: Bool, rpcTypename :: Typename, spec :: IO String
                       , rpcWriter :: String → IO (), dataWriter :: String → IO () }

defaults ∷ Options
defaults = Options True "RPC" (readFile "spec.json")
            (writeFile "RPCGen.swift") (writeFile "DataGen.swift")

options ∷ [OptDescr (Options->Options)]
options =
  [ Option ['v'] ["version"] (NoArg version) "show version number"
  , Option ['h'] ["help"] (NoArg usage)  "show help"
  , Option ['g'] ["gen-rpc-stub"] (NoArg genRPC) "output file to write"
  , Option ['t'] ["rpc-typename"] (ReqArg rpcT "RPCType") "input file to read"
  , Option ['r'] ["rpc-filename"] (ReqArg rpcW "FILE") "input file to read"
  , Option ['d'] ["data-filename"] (ReqArg dataW "FILE") "output file to write"
  , Option ['s'] ["spec-filename"] (ReqArg specR "FILE") "output file to write"
  ]

genRPC o = o { genRPCStub = True }
rpcT arg o = o { rpcTypename = arg }
rpcW arg o = o { rpcWriter = writeFile arg }
dataW arg o = o { dataWriter = writeFile arg }
specR arg o = o { spec = readFile arg }

translate ∷ JSValue → (String, String)
translate = translator swift ∘ toSpec ∘ processJSON

decode ∷ String → JSValue
decode = either error id ∘ resultToEither ∘ Text.JSON.decode

processJSON ∷ JSValue → [Map String String]
processJSON (JSArray a) = map (fromList ∘ map unpack ∘ fromJSObj) a where
  unpack (k, JSString s) = (k, fromJSString s)
  unpack _ = errType
  fromJSObj (JSObject obj) = fromJSObject obj
  fromJSObj _ = errType
  errType = error "Spec item should be map of type String: String"
processJSON _ = error $ "Root object should be array, see " ⧺ hsRPCGenURL

usage _ = error "Usage: hsrpcgen [-vhn] spec.json"
version _ = error "http://j.mp/HsRPCGen v0.1"

