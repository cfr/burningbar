#!/usr/bin/env runhaskell
{-# LANGUAGE UnicodeSyntax, ScopedTypeVariables #-}
-- $ brew install ghc cabal-install
-- $ cabal install json base-unicode-symbols
-- $ curl -O https://gist.github.com/cfr/a7ce3793cdf8f17c6412/raw/JSON→Swift.hs
-- $ chmod +x JSON→Swift.hs
-- $ ./JSON→Swift.hs <file.json

module Main where







import Control.Monad.Reader
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

type Name = String
type Typename = String
data Type = Array Typename | Dictionary Typename Typename
          | Optional Typename | Typename
data Variable = Variable Name Type
data Function = Function Name [Variable] Type
data Record = Record Name [Variable]

type Def a = a → String
data Language = Language
    { var  ∷ Def Variable
    , fun  ∷ Def Function
    , typ  ∷ Def Type
    , rec  ∷ Def Record
    , etc  ∷ String -- FIXME: all this defs should work in Reader Lang?
    }

type Spec = ([Record], [Function])

translator ∷ Language → Spec → String
translator (Language var fun typ rec etc) spec = etc ⧺ tr spec where
  tr (recs, funs) = concatMap rec recs ⧺ concatMap fun funs

type Pair = (String, String) -- element of json record
toSpec ∷ [[Pair]] → Spec
toSpec = (map parseRec ⁂ map parseFun) . span isRec where
  isRec ∷ [Pair] → Bool
  isRec = undefined
  parseRec ∷ [Pair] → Record
  parseRec = undefined
  parseFun = undefined

toSwift = undefined

{-
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
      p              → pv p

  pv ∷ JSValue → String
  pv (JSBool _)       = "Bool"
  pv (JSNull)         = "T?"
  pv (JSString s)     = "String"
  pv (JSRational t _) = "Double"
  pv u                = "N"

  capitalize (h:hs) = toUpper h : hs-}
-- TODO: swiftTranslator = read "swift.jsto"
-- NOTE: it seems it is not possible to extend [String: Any] in Swift
{-alias name = "typealias " ⧺ name ⧺ " = NSDictionary\n"
extension vars = "extension NSDictionary {\n  "
               ⧺ intercalate "\n  " vars ⧺ "\n}\n"

var name typeName isSafe = "var " ⧺ name ⧺ typed ⧺ "{ get { "
                         ⧺ "return self[\"" ⧺ name ⧺"\"] "
                         ⧺ as ⧺ " " ⧺ typeName ⧺ " } }"
                   where typed = ": " ⧺ typeName
                               ⧺ if isSafe then "? " else " "
                         as = "as" ⧺ if isSafe then "?" else "!"
-}


main = putStrLn ("// Generated with " ⧺ jsonToSwiftURL) ≫
       interact (toSwift ∘ decode) ≫ putStr "\n"

decode ∷ String → JSValue
decode = either error id ∘ resultToEither ∘ Text.JSON.decode

