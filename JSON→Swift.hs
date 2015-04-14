{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables #-}
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
import Control.Arrow.Unicode
import Data.List (intercalate)

jsSwiftURL = "https://gist.github.com/cfr/a7ce3793cdf8f17c6412#file-json-swift-hs"

data TranslatorState = TranslatorState
    { json  ∷ JSValue
    , swift ∷ String
    }

type Pair = (String, JSValue)

toSwift ∷ TranslatorState → String
toSwift = evalState $ translate
 where
  --translate = get ≫= return ∘ show ∘ swift
  toS v = toSwift (TranslatorState v "")
  translate = do
    j ← get ≫= return ∘ json
    return $ case j of
      JSObject jso → let os ∷ [Pair] = fromJSObject jso
                         po (k, v) = k ⧺ ": {" ⧺ toS v ⧺ "}, "
                     in concatMap po os

      JSArray a    → let l = intercalate ", " (map toS a)
                     in "[" ⧺ l ⧺ "]"
      p            → pv p
  pv ∷ JSValue -> String
  pv (JSBool True)    = "true"
  pv (JSBool False)   = "false"
  pv (JSNull)         = "null"
  pv (JSString s)     = fromJSString s
  pv (JSRational f r) = show $ fromRational r
  pv u                = show u

main = interact $
          toSwift
        ∘ flip TranslatorState ("// Generated with " ⧺ jsSwiftURL ⧺ "\n")
        ∘ decode

decode ∷ String → JSValue
decode = either error id ∘ resultToEither ∘ Text.JSON.decode

