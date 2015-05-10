{-# LANGUAGE ScopedTypeVariables, UnicodeSyntax, CPP, RecordWildCards #-}

module Main where

import Control.Monad (join)
import Control.Exception (catch, SomeException)
import Prelude hiding (catch)
import System.Environment (getArgs)
import System.Console.GetOpt (OptDescr(..), getOpt, ArgOrder(..), ArgDescr(..), usageInfo)
import System.Directory (createDirectoryIfMissing)

import Language
import Parse
import Swift

import Unicode

bbURL = "http://j.mp/burnbar"
version = " v0.5.10"

main = do
  args ← getArgs
  let (actions, _, _) = getOpt RequireOrder options args
  let (Options {..}) = foldr ($) defaults actions
  let copy = (("// Generated with " ⧺ bbURL ⧺ version ⧺ "\n\n") ⧺)
  let write = (∘ copy) ∘ writeFile ∘ (root ⧄)
  spec ← spec ≫= return ∘ parse
  let (ent, int) = translator (swift transport interface) spec
  (createDir root ≫ write entFn ent ≫ write intFn int)
      `catch` handleEx "Error ¬ ¬: "
#ifdef DEBUG
  print (spec, ent, int)
#endif


data Options = Options { transport ∷ Typename, interface ∷ Typename
                       , spec ∷ IO String, root ∷ FilePath, entFn ∷ FilePath, intFn ∷ FilePath }
defaults ∷ Options
defaults = Options "Transport" "Interface" (readFile "spec.burbar") "./" entFn intFn
  where { intFn = "Interface" ⊡ ext; entFn = "Entities" ⊡ ext; ext = "swift" }

options ∷ [OptDescr (Options → Options)]
options = let opt (k, f, a, h) = Option k f a h in map opt
  [ ("v", ["version"], NoArg ver, "print version number")
  , ("h", ["help"], NoArg use, "print help")
  , ("t", ["transport"], ReqArg (\a o → o {transport = a}) "Transport", "transport protocol name")
  , ("i", ["interface"], ReqArg (\a o → o {interface = a}) "Iterface", "interface class name")
  , ("r", ["interface-file"], ReqArg (\a o → o {intFn = a}) "Interface.swift", "interface out filename")
  , ("d", ["entities-file"], ReqArg (\a o → o {entFn = a}) "Entities.swift", "entities out filename")
  , ("s", ["spec-file"], ReqArg (\a o → o {spec = readFile a}) "spec.burbar", "input spec file")
  , ("p", ["path"], ReqArg (\a o → o {root = a}) ".", "output path prefix") ]


use _ = error $ usageInfo ("Usage: burningbar [-vhtirdsp]\n" ⧺ bbURL ⧺ version) options
ver _ = error $ bbURL ⧺ version

createDir name = createDirectoryIfMissing True name `catch` handleEx "Can't create dir."
handleEx err (e ∷ SomeException) = error (err ++ show e)

