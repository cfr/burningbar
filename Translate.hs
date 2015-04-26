{-# LANGUAGE ViewPatterns, UnicodeSyntax #-}
module Translate (Spec(..), toSpec, translator) where

import Data.List (stripPrefix)
import Data.Map (Map, partitionWithKey, mapKeys, member, (!), toList)
import Data.Char (isSpace)
import Control.Monad (join)
import Control.Arrow (second)

import Data.Text (pack, unpack)
import qualified Data.Text (stripSuffix)

import Prelude.Unicode
import Control.Arrow.Unicode

import Language

type Spec = ([Record], [Function])

translator ∷ Language → Spec → String
translator (Language var fun typ rec etc) = (etc ⧺) ∘ tr where
  tr (recs, funs) = concatMap rec recs ⧺ concatMap fun funs

toSpec ∷ [Map String String] → Spec
toSpec = (map parseRec ⁂ map parseFun) ∘ span isRec where
  isRec = member '_' ∘ mapKeys head

parseRec ∷ Map String String → Record
parseRec r = Record name (map parseVar $ toList vars) where
  (underscored, vars) = partitionWithKey (const ∘ (≡ '_') ∘ head) r
  name = underscored ! "_name"

parseVar = uncurry ((∘ parseType) ∘ Variable)

parseType (stripSuffix "?" → Just type_) = Optional (parseType type_)
parseType (stripPrefix "[" → Just type_) = Array ((parseType ∘ init) type_) -- TODO: check "]"
parseType (stripPrefix "{" → Just type_) = parseDictType type_
parseType u = Typename u
parseDictType (stripSuffix "}" → Just type_) = Dictionary (keyType) (valType)
  where (keyType, valType) = join (⁂) parseType (splitAtColon type_)
        splitAtColon = (filter notSpace ⁂ tail ∘ filter notSpace) ∘ break (≡ ':')
        notSpace = not ∘ isSpace

parseFun = const $ Function "tbd" "user.tbd" [] (Typename "Void")

stripSuffix = (fmap unpack ∘) ∘ (∘ pack) ∘ Data.Text.stripSuffix ∘ pack

