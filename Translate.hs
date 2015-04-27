{-# LANGUAGE ViewPatterns, UnicodeSyntax #-}
module Translate (Spec(..), toSpec, translator) where

import Data.List (stripPrefix)
import Data.Map (Map, partitionWithKey, mapKeys
                , lookup, member, (!), toList)
import Data.Maybe
import Data.Char (isSpace)
import Control.Monad (join)
import Control.Arrow (second)

import Data.Text (pack, unpack)
import qualified Data.Text (stripSuffix)

import Prelude.Unicode
import Prelude hiding (lookup)
import Control.Arrow.Unicode

import Language

type Spec = ([Record], [Function])

translator ∷ Language → Spec → String
translator (Language function record etc) = (etc ⧺) ∘ tr where
  tr (recs, funs) = concatMap record recs ⧺ concatMap function funs

toSpec ∷ [Map String String] → Spec
toSpec = (map parseRec ⁂ map parseFun) ∘ span isRec where
  isRec = member '_' ∘ mapKeys head

parseRec ∷ Map String String → Record
parseRec m = Record name (parseVars vars) where
  (underscored, vars) = partitionWithPrefix '_' m
  name = underscored ! "_name"

parseVar = uncurry ((∘ parseType) ∘ Variable)
parseVars = map parseVar ∘ toList

parseType (stripSuffix "?" → Just type_) = Optional (parseType type_)
parseType (stripPrefix "[" → Just type_) = Array ((parseType ∘ init) type_) -- TODO: check "]"
parseType (stripPrefix "{" → Just type_) = parseDictType type_
parseType u = Typename u
parseDictType (stripSuffix "}" → Just type_) = Dictionary keyType valType
  where (keyType, valType) = join (⁂) parseType (splitAtColon type_)
        splitAtColon = (filter notSpace ⁂ tail ∘ filter notSpace) ∘ break (≡ ':')
        notSpace = not ∘ isSpace


parseFun m = Function name rpc (parseVars args) t where
  (dashed, args) = partitionWithPrefix '-' m
  name = fromMaybe rpc (lookup "-pretty" dashed)
  rpc = dashed ! "-method"
  t = parseType (dashed ! "-returns")

partitionWithPrefix prefix = partitionWithKey (const ∘ (≡ prefix) ∘ head)

stripSuffix = (fmap unpack ∘) ∘ (∘ pack) ∘ Data.Text.stripSuffix ∘ pack

