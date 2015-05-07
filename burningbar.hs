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

bbURL = "http://j.mp/burningbar"

main = do
  args ← getArgs
  let (actions, _, _) = getOpt RequireOrder options args
  let o = foldr ($) defaults actions
  let copy = (("// Generated with " ⧺ bbURL ⧺ "\n\n") ⧺)
  let intPath = root o </> intFn o
  let entPath = root o </> entFn o
  let writeInt = writeFile intPath ∘ copy ∘ interfaceWrap (intStub o) (intType o)
  let writeEnt = writeFile entPath ∘ copy ∘ entitiesWrap
  json ← spec o
  createDir (root o)
  let proc = decode ⋙ translate ⋙ writeEnt ⁂ writeInt ⋙ uncurry (≫)
  proc json `catch` handleEx "Syntax error ¬ ¬"

translate ∷ [Map String String] → (String, String)
translate = translator swift ∘ toSpec

data Options = Options { intStub ∷ Bool, intType ∷ Typename, spec ∷ IO String
                       , root ∷ FilePath, intFn ∷ FilePath, entFn ∷ FilePath }
defaults ∷ Options
defaults = Options False "Interface" (readFile "spec.json") "./" intFn entFn
  where { intFn = "Interface" <.> ext; entFn = "Entities" <.> ext; ext = "swift" }

options ∷ [OptDescr (Options → Options)]
options =
  [ Option "v" ["version"]        (NoArg  version) "show version number"
  , Option "h" ["help"]           (NoArg  usage)  "show help"
  , Option "g" ["int-stub"]       (NoArg  (\o → o {intStub = True})) "generate interface class stub"
  , Option "t" ["interface-type"] (ReqArg (\a o → o {intType = a}) "T") "interface class name"
  , Option "r" ["interface-file"] (ReqArg (\a o → o {intFn = a}) "f") "interface output file"
  , Option "d" ["entities-file"]  (ReqArg (\a o → o {entFn = a}) "f") "entities outout file"
  , Option "s" ["spec-file"]      (ReqArg (\a o → o {spec = readFile a}) "f") "input spec file"
  , Option "p" ["root-path"]      (ReqArg (\a o → o {root = a}) "p") "path to put generated files" ]

decode ∷ String → [Map String String]
decode = unpackJSON ∘ either error id ∘ resultToEither ∘ Text.JSON.decode

unpackJSON ∷ JSValue → [Map String String]
unpackJSON (JSArray a) = map (fromList ∘ map unpack ∘ fromJSObj) a where
  unpack (k, JSString s) = (k, fromJSString s)
  unpack _ = errType
  fromJSObj (JSObject obj) = fromJSObject obj
  fromJSObj _ = errType
  errType = error "Spec item should be map of type String: String"
unpackJSON _ = error $ "Root object should be array, see " ⧺ bbURL

usage _ = error $ "Usage: hsrpcgen [-vhgtrdsp]\n" ⧺ bbURL
version _ = error $ bbURL ⧺ "v0.2"

createDir name = createDirectoryIfMissing True name `catch` handleEx "Can't create dir."
handleEx err (e ∷ SomeException) = print e ≫  error err
-- TODO: catch specific exs and provide info, i.e. Map.! — no item with prefix

