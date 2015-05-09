{-# LANGUAGE ScopedTypeVariables, UnicodeSyntax, CPP #-}

module Main where

import Control.Monad (join)
import Data.Map (Map, fromList)
import Control.Exception (catch, SomeException)
import Prelude hiding (catch)
import System.Environment (getArgs)
import System.Console.GetOpt (OptDescr(..), getOpt, ArgOrder(..), ArgDescr(..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix ((</>), (<.>))

import Language
import Control.Arrow
import Parse
import Swift

import Prelude.Unicode
import Control.Monad.Unicode
import Control.Arrow.Unicode

bbURL = "http://j.mp/burningbar"
version = " v0.5.9"

main = do
  args ← getArgs
  let (actions, _, _) = getOpt RequireOrder options args
  let (Options cancel transport interface spec root iFn eFn) = foldr ($) defaults actions
  let copy = (("// Generated with " ⧺ bbURL ⧺ version ⧺ "\n\n") ⧺)
  let write = (∘ copy) ∘ writeFile ∘ (root </>)
  spec ← spec ≫= return ∘ parse
  let (ent, int) = translator (swift cancel transport interface) spec
  (createDir root ≫ write eFn ent ≫ write iFn int)
      `catch` handleEx "Syntax error ¬ ¬"
#ifdef DEBUG
  print (spec, ent, int)
#endif


data Options = Options { cancel ∷ Typename, transport ∷ Typename, interface ∷ Typename
                       , spec ∷ IO String, root ∷ FilePath, entFn ∷ FilePath, intFn ∷ FilePath }
defaults ∷ Options
defaults = Options "Void" "Transport" "Interface" (readFile "spec.bb") "./" intFn entFn
  where { intFn = "Interface" <.> ext; entFn = "Entities" <.> ext; ext = "swift" }

options ∷ [OptDescr (Options → Options)]
options =
  [ Option "v" ["version"]        (NoArg  ver) "show version number"
  , Option "h" ["help"]           (NoArg  use) "show help"
  , Option "a" ["transport"]      (ReqArg (\a o → o {transport = a}) "T") "transport type name"
  , Option "c" ["cancel"]         (ReqArg (\a o → o {cancel= a}) "C") "cancellation token type name"
  , Option "t" ["interface"]      (ReqArg (\a o → o {interface = a}) "I") "interface class name"
  , Option "r" ["interface-file"] (ReqArg (\a o → o {intFn = a}) "i") "interface output file"
  , Option "d" ["entities-file"]  (ReqArg (\a o → o {entFn = a}) "e") "entities outout file"
  , Option "s" ["spec-file"]      (ReqArg (\a o → o {spec = readFile a}) "s") "input spec file"
  , Option "p" ["root-path"]      (ReqArg (\a o → o {root = a}) "p") "path to put generated files" ]

use _ = error $ "Usage: hsrpcgen [-vhgtrdsp]\n" ⧺ bbURL ⧺ version
ver _ = error $ bbURL ⧺ version

createDir name = createDirectoryIfMissing True name `catch` handleEx "Can't create dir."
handleEx err (e ∷ SomeException) = print e ≫ error err

-- (⩕) = (&&&)
