{-# LANGUAGE UnicodeSyntax #-}
-- $ git clone https://github.com/cfr/HsRPCGen.git
-- $ cd HsRPCGen
-- $ make deps
-- $ ./rpcgen spec.json

module Main where

import Data.Map (Map, fromList)

import Translate
import Swift

import qualified Text.JSON (decode)
import Text.JSON hiding (decode)

import Prelude.Unicode
import Control.Monad.Unicode

hsRPCGenURL = "http://j.mp/HsRPCGen"

main = putStrLn ("// Generated with " ⧺ hsRPCGenURL ⧺ "\n") ≫
       interact (translate ∘ decode)

translate ∷ JSValue → String
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

