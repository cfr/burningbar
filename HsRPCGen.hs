{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Data.Map (Map, fromList)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))

import Translate
import Swift

import qualified Text.JSON (decode)
import Text.JSON hiding (decode)

import Prelude.Unicode
import Control.Monad.Unicode

hsRPCGenURL = "http://j.mp/HsRPCGen"

main = do genRPCStub ← getArgs ≫= parse
          putStrLn ("// Generated with " ⧺ hsRPCGenURL ⧺ "\n") ≫
            interact (translate genRPCStub ∘ decode)

rpcName = "Singularity" -- FIXME: pass via arg
-- TODO: pass "call" function via argument for RPC

translate ∷ Bool → JSValue → String
translate genRPCStub = translator (swift genRPCStub rpcName) ∘ toSpec ∘ processJSON

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

parse ["-h"] = usage
parse ["-v"] = version
parse ["--no-rpc"] = return False
parse ["-n"] = return False
parse _ = return True

usage   = putStrLn "Usage: hsrpcgen [-vhn] spec.json" ≫ exit
version = putStrLn "http://j.mp/HsRPCGen v0.1" ≫ exit
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)

