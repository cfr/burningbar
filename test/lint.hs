{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure)
import Control.Monad (unless)

main = do o ← hlint ["src", "test"]
          unless (null o) exitFailure

