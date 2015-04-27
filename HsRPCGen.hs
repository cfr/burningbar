{-# LANGUAGE ViewPatterns, UnicodeSyntax #-}

module Main where

import Data.Map (Map, fromList)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.Console.GetOpt (OptDescr(..), getOpt, ArgOrder(..), ArgDescr(..))

import Translate
import Swift

import qualified Text.JSON (decode)
import Text.JSON hiding (decode)

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Arrow.Unicode
import Control.Monad (join)

hsRPCGenURL = "http://j.mp/HsRPCGen"

-- TODO: accept multiple specs
-- TODO: put compiled version or install deps automatically

main = do
  args ← getArgs
  let (actions, _, _) = getOpt RequireOrder options args --lb
  o ← foldl (≫=) (return defaults) actions
  let copy = (⧺ "// Generated with " ⧺ hsRPCGenURL ⧺ "\n")
  let proc = decode ⋙ translate o ⋙ join (⁂) copy
             ⋙ rpcWriter o ⁂ dataWriter o ⋙ const exit
  spec o ≫= proc

data Options = Options { genRPCStub :: Bool, rpcTypename :: Typename
                       , rpcWriter :: String → IO (), dataWriter :: String → IO ()
                       , spec :: IO String }

defaults :: Options
defaults = Options True "RPC" (writeFile "RPCGen") (writeFile "DataGen") (readFile "spec.json")

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option ['v'] ["version"] (NoArg version) "show version number"
  , Option ['h'] ["help"]    (NoArg usage)  "show help"
  , Option ['g'] ["gen-rpc-stub"]  (NoArg genRPC) "output file to write"
  , Option ['t'] ["rpc-typename"]  (ReqArg rpcT "RPCType")   "input file to read"
  , Option ['r'] ["rpc-filename"]  (ReqArg rpcW "FILE")   "input file to read"
  , Option ['d'] ["data-filename"] (ReqArg dataW "FILE") "output file to write"
  , Option ['s'] ["spec-filename"] (ReqArg specR "FILE") "output file to write"
  ]

genRPC o = return o { genRPCStub = True } --lb
rpcT arg o = return o { rpcTypename = arg }
rpcW arg o = return o { rpcWriter = writeFile arg }
dataW arg o = return o { dataWriter = writeFile arg }
specR arg o = return o { spec = readFile arg }

translate ∷ Options → JSValue → (String, String)
translate (Options genRPCStub rpcName _ _ _)
    = translator (swift genRPCStub rpcName) ∘ toSpec ∘ processJSON

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

usage _ = putStrLn "Usage: hsrpcgen [-vhn] spec.json" ≫ exit
version _ = putStrLn "http://j.mp/HsRPCGen v0.1" ≫ exit
exit = exitWith ExitSuccess

