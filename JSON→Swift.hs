{-# LANGUAGE UnicodeSyntax #-}
-- $ brew install ghc cabal-install
-- $ cabal install json base-unicode-symbols
-- $ ghc --make JSON→Swift.hs
-- $ ./JSON→Swift <file.json

module Main where

import Control.Monad.State
import qualified Text.JSON (decode)
import Text.JSON hiding (decode)
import Prelude.Unicode
import Control.Monad.Unicode

jsSwiftURL = "https://gist.github.com/cfr/a7ce3793cdf8f17c6412#file-json-swift-hs"

data TranslatorState = TranslatorState
    { json  ∷ JSValue
    , swift ∷ String
    }

toSwift ∷ TranslatorState → String
toSwift = evalState $ translate
 where
  translate = get ≫= return ∘ swift

main = interact $
          toSwift
        ∘ flip TranslatorState ("// Generated with " ⧺ jsSwiftURL ⧺ "\n")
        ∘ decode

decode ∷ String → JSValue
decode = either error id ∘ resultToEither ∘ Text.JSON.decode

