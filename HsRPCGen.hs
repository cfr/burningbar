{-# LANGUAGE ScopedTypeVariables, UnicodeSyntax #-}

module Main where

import Control.Monad (join)
import Data.Map (Map, fromList)
import Control.Exception (catch, SomeException)
import Prelude hiding (catch)
import System.Environment (getArgs)
import System.Console.GetOpt (OptDescr(..), getOpt, ArgOrder(..), ArgDescr(..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix ((</>), (<.>))

import Translate
import Swift

import qualified Text.JSON (decode)
import Text.JSON hiding (decode)

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Arrow.Unicode

hsRPCGenURL = "http://j.mp/HsRPCGen"

main = do
  args ← getArgs
  let (actions, _, _) = getOpt RequireOrder options args
  let o = foldr ($) defaults actions

  let copy = (("// Generated with " ⧺ hsRPCGenURL ⧺ "\n\n") ⧺)
  let rpcPath = rootPath o </> rpcFn o
  let entPath = rootPath o </> entFn o
  let writeRPC = writeFile rpcPath ∘ copy ∘ rpcWrap (genRPCStub o) (rpcTypename o)
  let writeData = writeFile entPath ∘ copy ∘ dataWrap

  let proc = decode ⋙ translate ⋙ writeData ⁂ writeRPC ⋙ uncurry (≫)
  createDir (rootPath o) ≫ spec o ≫= proc

translate ∷ JSValue → (String, String)
translate = translator swift ∘ toSpec ∘ processJSON

data Options = Options { genRPCStub ∷ Bool, rpcTypename ∷ Typename, spec ∷ IO String
                       , rootPath ∷ FilePath, rpcFn ∷ FilePath, entFn ∷ FilePath}
defaults ∷ Options
defaults = Options False "RPC" (readFile "spec.json") "./" interfaceFn entitiesFn
  where interfaceFn = "Interface" <.> ext
        entitiesFn = "Entities" <.> ext
        ext = "swift"

options ∷ [OptDescr (Options → Options)]
options =
  [ Option "v" ["version"] (NoArg version) "show version number"
  , Option "h" ["help"] (NoArg usage)  "show help"
  , Option "g" ["gen-rpc-stub"] (NoArg genRPC) "output file to write"
  , Option "t" ["rpc-typename"] (ReqArg rpcT "RPCType") "input file to read"
  , Option "r" ["rpc-filename"] (ReqArg rpcF "FILE") "input file to read"
  , Option "d" ["data-filename"] (ReqArg entF "FILE") "output file to write"
  , Option "s" ["spec-filename"] (ReqArg spcF "FILE") "output file to write"
  , Option "p" ["gen-path"] (ReqArg path "DIR") "path to put generated files"
  ]

genRPC o = o { genRPCStub = True }
rpcT arg o = o { rpcTypename = arg }
rpcF arg o = o { rpcFn = arg }
entF arg o = o { entFn = arg }
spcF arg o = o { spec = readFile arg }
path arg o = o { rootPath = arg }

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

usage _ = error "Usage: hsrpcgen [-vhgtrds]"
version _ = error $ hsRPCGenURL ⧺ "v0.1"

createDir name = (createDirectoryIfMissing True name) `catch` handleEx
  where handleEx (e∷SomeException) = error "Can't create dir."
