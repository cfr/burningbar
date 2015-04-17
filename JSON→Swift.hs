{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables #-}
-- $ brew install ghc cabal-install
-- $ cabal install json base-unicode-symbols
-- $ curl -O https://gist.github.com/cfr/a7ce3793cdf8f17c6412/raw/JSON→Swift.hs
-- $ ghc --make JSON→Swift.hs
-- $ ./JSON→Swift <file.json

module Main where

import Control.Monad.State
import Data.List (intercalate)
import Data.Char (toUpper)
import qualified Text.JSON (decode)
import Text.JSON hiding (decode)
import Prelude.Unicode
import Control.Monad.Unicode
import Control.Arrow.Unicode
import Control.Applicative.Unicode

jsonToSwiftURL = "http://j.mp/JSON-Swift_hs"

data TranslatorState = TState { extensions ∷ String
                              , keyname ∷ Maybe String
                              , json ∷ JSValue }
stateWithJSON = TState [] Nothing

type Pair = (String, JSValue)

toSwift ∷ JSValue → String
toSwift = evalState translate ∘ stateWithJSON where
  translate ∷ State TranslatorState String
  translate = do
    j ← gets json
    return $ case j of
      JSObject jso   →  let os ∷ [Pair] = fromJSObject jso
                            po (k, v) = k ⧺ ": {" ⧺ toSwift v ⧺ "}, "
                        in concatMap po os
      JSArray (o:os) → let l = intercalate ", " (map toSwift (o:os))
                       in "[" ⧺ l ⧺ "]"
--                      -- takes first object only
--                      let (sw, st) = runState translate $ stateWithJSON a
--                      in do modify (\s → s { extesions = st ⧺ extensions s})
--                            return 
      p              → pv p

  pv ∷ JSValue → String
  pv (JSBool _)       = "Bool"
  pv (JSNull)         = "T?"
  pv (JSString s)     = "String"
  pv (JSRational t _) = "Double"
  pv u                = "N"

  capitalize (h:hs) = toUpper h : hs


-- NOTE: it seems it is not possible to extend [String: Any] in Swift
alias name = "typealias " ⧺ name ⧺ " = NSDictionary\n"
extension vars = "extension NSDictionary {\n  "
               ⧺ intercalate "\n  " vars ⧺ "\n}\n"

var name typeName isSafe = "var " ⧺ name ⧺ typed ⧺ "{ get { "
                         ⧺ "return self[\"" ⧺ name ⧺"\"] "
                         ⧺ as ⧺ " " ⧺ typeName ⧺ " } }"
                   where typed = ": " ⧺ typeName
                               ⧺ if isSafe then "? " else " "
                         as = "as" ⧺ if isSafe then "?" else "!"


main = putStrLn ("// Generated with " ⧺ jsonToSwiftURL) ≫
       interact (toSwift ∘ decode) ≫ putStr "\n"

decode ∷ String → JSValue
decode = either error id ∘ resultToEither ∘ Text.JSON.decode

