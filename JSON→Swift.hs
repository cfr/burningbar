{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables #-}
-- $ brew install ghc cabal-install
-- $ cabal install json base-unicode-symbols
-- $ ghc --make JSON→Swift.hs
-- $ ./JSON→Swift <file.json

module Main where

import Control.Monad.State
import Data.List (intercalate)
import qualified Text.JSON (decode)
import Text.JSON hiding (decode)
import Prelude.Unicode
import Control.Monad.Unicode
import Control.Arrow.Unicode
import Control.Applicative.Unicode

jsonToSwiftURL = "https://gist.github.com/cfr/a7ce3793cdf8f17c6412"

data TState = TState { swift ∷ String
                     , json  ∷ JSValue}

type Pair = (String, JSValue)

toSwift ∷ JSValue → String
toSwift = evalState translate ∘ TState [] where
  translate ∷ State TState String
  translate = do
    j ← get ≫= return ∘ json
    return $ case j of
      JSObject jso → let os ∷ [Pair] = fromJSObject jso
                         po (k, v) = k ⧺ ": {" ⧺ toSwift v ⧺ "}, "
                     in concatMap po os

      JSArray a    → let l = intercalate ", " (map toSwift a)
                     in "[" ⧺ l ⧺ "]"
      p            → pv p

  pv ∷ JSValue → String
  pv (JSBool True)    = "true"
  pv (JSBool False)   = "false"
  pv (JSNull)         = "null"
  pv (JSString s)     = fromJSString s
  pv (JSRational f r) = show $ fromRational r
  pv u                = show u


main = putStrLn ("// Generated with " ⧺ jsonToSwiftURL) >>
       interact (toSwift ∘ decode) >> putStr "\n"

decode ∷ String → JSValue
decode = either error id ∘ resultToEither ∘ Text.JSON.decode

