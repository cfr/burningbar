{-# LANGUAGE ScopedTypeVariables, UnicodeSyntax, CPP, RecordWildCards #-}
module Main where

import Control.Exception (catch, SomeException)
import Control.Monad (when)
import Prelude hiding (catch)
import System.Environment (getArgs)
import System.Console.GetOpt (getOpt, OptDescr(..), ArgOrder(..), ArgDescr(..), usageInfo)
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix ((</>))

import Language
import Parse
import Swift
import Util
import Checker

bbURL = "http://j.mp/burnbar"
version = " v0.6.7-α"

main = do
  args ← getArgs
  let (actions, _, _) = getOpt RequireOrder options args
  let (Options {..}) = foldr ($) defaults actions
  let copy = (("// 📏🔥 Generated with " ⧺ bbURL ⧺ version ⧺ "\n") ⧺)
  let write = (∘ copy) ∘ writeFile ∘ (root </>)
  spec ← readFile spec
  let errors = check spec in when (errors ≠ []) (error errors)
  let (ent, int) = translator (swift shield transport interface) (parse spec)
  (createDir root ≫ write entFn ent ≫ write intFn int)
      `catch` handleEx
#ifdef DEBUG
  print (spec, ent, int)
#endif

data Options = Options { transport ∷ Typename, interface ∷ Typename , spec ∷ String
                       , root ∷ FilePath, entFn ∷ FilePath, intFn ∷ FilePath, shield ∷ Bool}

defaults = Options "Transport" "Interface" "spec.burnbar" "./" entFn intFn False
  where { intFn = "Interface.swift"; entFn = "Entities.swift" }

options ∷ [OptDescr (Options → Options)]
options = let opt (k, f, a, h) = Option k f a h in map opt
  [ ("v", ["version"], NoArg ver, "print version number"), ("h", ["help"], NoArg use, "print help")
  , ("t", ["transport"], ReqArg (\a o → o {transport = a}) "Transport", "transport protocol name")
  , ("n", ["interface"], ReqArg (\a o → o {interface = a}) "Iterface", "interface class name")
  , ("i", ["interface-file"], ReqArg (\a o → o {intFn = a}) "Interface.swift", "interface out filename")
  , ("r", ["entities-file"], ReqArg (\a o → o {entFn = a}) "Entities.swift", "entities out filename")
  , ("s", ["spec-file"], ReqArg (\a o → o {spec = a}) "spec.burnbar", "input spec file")
  , ("b", ["dynamicity-shield"], NoArg (\o → o {shield = True}), "accept weak-typed json")
  , ("f", ["fucking-string"], NoArg (\o → o {shield = True}), "accept weak-typed json")
  , ("p", ["path"], ReqArg (\a o → o {root = a}) ".", "output path prefix") ]

use _ = error $ usageInfo ("Usage: burningbar [-vtnirsbfpcd]\n" ⧺ bbURL ⧺ version) options
ver _ = error $ bbURL ⧺ version

createDir = createDirectoryIfMissing True
handleEx (e ∷ SomeException) = error (show e)

