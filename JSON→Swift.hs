#!/usr/bin/env runhaskell
{-# LANGUAGE ViewPatterns, UnicodeSyntax, ScopedTypeVariables #-}
-- $ brew install ghc cabal-install
-- $ cabal install json base-unicode-symbols
-- $ curl -O https://gist.github.com/cfr/a7ce3793cdf8f17c6412/raw/JSON→Swift.hs
-- $ chmod +x JSON→Swift.hs
-- $ ./JSON→Swift.hs <spec.json

module Main where

import Data.List (intercalate, stripPrefix)
import Data.Char (toUpper, isSpace)
import Data.Map (Map, mapKeys, member, fromList, (!), toList)
import Control.Monad (join)
import Control.Arrow (second)
import qualified Data.Map.Strict as Map
import Data.Text (pack)
import qualified Data.Text as T (stripSuffix, unpack)
import qualified Text.JSON (decode)
import Text.JSON hiding (decode)
import Prelude.Unicode
import Control.Monad.Unicode
import Control.Arrow.Unicode
import Control.Applicative.Unicode

jsonToSwiftURL = "http://j.mp/JSON-Swift_hs"

type Name = String
type Typename = String
data Type = Array Type | Dictionary Type Type
          | Optional Type | Typename String deriving Show
data Variable = Variable Name Type deriving Show
data Function = Function Name [Variable] Type deriving Show
data Record = Record Name [Variable] deriving Show

type Def a = a → String
data Language = Language
    { var  ∷ Def Variable
    , fun  ∷ Def Function
    , typ  ∷ Def Type
    , rec  ∷ Def Record
    , etc  ∷ String
    }

swift ∷ Language
swift = Language tbd tbd fromType fromRecord etcSwift where
 tbd = const "// TBD"
 fromRecord (Record name vars) = "struct " ⧺ name ⧺ " {\n" ⧺ concat decls ⧺ "}\n"
  where decls = initDecl : (map varDecl vars)
        initDecl = s 4 ⧺ "init(json: JSON) {\n" ⧺ concatMap initVar vars ⧺ s 4 ⧺ "}\n"
        initVar (Variable n (Optional t)) = initWithElem n ⧺ "? " ⧺ fromType t ⧺ "\n"
        initVar (Variable n t) = initWithElem n ⧺ "! " ⧺ fromType t ⧺ "\n"
        initWithElem n = s 8 ⧺ n ⧺ " = json[" ⧺ n ⧺ "] as"
        varDecl (Variable n t) = s 4 ⧺ "let " ⧺ n ⧺ ": " ⧺ fromType t ⧺ "\n"
        s = concat ∘ flip take spaces
        spaces = repeat " "
 fromType (Array t) = "[" ⧺ fromType t ⧺ "]"
 fromType (Optional t) = fromType t ⧺ "?"
 fromType (Dictionary tk tv) = "[" ⧺ fromType tk ⧺ " : " ⧺ fromType tv ⧺ "]"
 fromType (Typename typename) = typename
 etcSwift = "typealias JSON = Dictionary<String, String>\n"

type Spec = ([Record], [Function])

translator ∷ Language → Spec → String
translator (Language var fun typ rec etc) = (etc ⧺) ∘ tr where
  tr (recs, funs) = concatMap rec recs ⧺ concatMap fun funs


toSpec ∷ [Map String String] → Spec
toSpec = (map parseRec ⁂ map parseFun) ∘ span isRec where
  isRec = member '_' ∘ mapKeys head

parseRec ∷ Map String String → Record
parseRec r = Record name (map parseVar $ toList vars) where
  (underscored, vars) = Map.partitionWithKey (const ∘ (≡ '_') ∘ head) r
  name = underscored ! "_name"

parseVar = uncurry ((∘ parseType) ∘ Variable)

parseType (stripSuffix "?" → Just type_) = Optional (parseType type_)
parseType (stripPrefix "[" → Just type_) = Array ((parseType ∘ init) type_) -- TODO: check "]"
parseType (stripPrefix "{" → Just type_) = parseDictType type_
parseType u = Typename u
parseDictType (stripSuffix "}" → Just type_) = Dictionary (keyType) (valType)
  where (keyType, valType) = join (⁂) parseType (splitAtColon type_)
        splitAtColon = second (tail ∘ filter (not ∘ isSpace)) ∘ break (≡ ':')

parseFun = const $ Function "TBD" [] (Typename "TBD")

translate ∷ JSValue → String
translate = translator swift ∘ toSpec ∘ processJSON

main = putStrLn ("// Generated with " ⧺ jsonToSwiftURL) ≫
       interact (translate ∘ decode) ≫ putStr "\n"

decode ∷ String → JSValue
decode = either error id ∘ resultToEither ∘ Text.JSON.decode

processJSON ∷ JSValue → [Map String String]
processJSON (JSArray a) = map (fromList ∘ map unpack ∘ fromJSObj) a where
  unpack (k, (JSString s)) = (k, fromJSString s)
  unpack _ = errType
  fromJSObj (JSObject obj) = fromJSObject obj
  fromJSObj _ = errType
  errType = error "Spec item should be map of type String: String"
processJSON _ = error $ "Root object should be array, see " ⧺ jsonToSwiftURL

stripSuffix = (fmap T.unpack ∘) ∘ (∘ pack) ∘ T.stripSuffix ∘ pack
