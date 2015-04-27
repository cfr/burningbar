{-# LANGUAGE ViewPatterns, UnicodeSyntax #-}
module Translate (Spec(..), toSpec, translator
                 , Language(..), Record(..), Function(..)) where

import Data.List (stripPrefix)
import Data.Map (Map, partitionWithKey, mapKeys
                , lookup, member, (!), toList)
import Data.Maybe (fromMaybe)
import Data.Char (isSpace)
import Control.Monad (join)
import Control.Arrow (second)

import Data.Text (pack, unpack)
import qualified Data.Text (stripSuffix)

import Prelude.Unicode
import Prelude hiding (lookup)
import Control.Arrow.Unicode

import Language

translator ∷ Language → Spec → String
translator (Language function record header) = (header ⧺) ∘ tr where
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

-- FIXME: strip spaces
parseType (stripSuffix "?" → Just t) = Optional (parseType t)
parseType (stripPrefix "[" → Just t) = Array ((parseType ∘ init) t) -- TODO: check "]"
parseType (stripPrefix "{" → Just t) = parseDictType t
parseType u = Typename u
parseDictType (stripSuffix "}" → Just t) = Dictionary keyType valType
  where (keyType, valType) = join (⁂) parseType (splitAtColon t)
        splitAtColon = (filter notSpace ⁂ tail ∘ filter notSpace) ∘ break (≡ ':')
        notSpace = not ∘ isSpace

parseFun m = Function name rpc (parseVars args) t where
  (dashed, args) = partitionWithPrefix '-' m
  name = fromMaybe rpc (lookup "-pretty" dashed)
  rpc = dashed ! "-method"
  t = parseType (dashed ! "-returns")

partitionWithPrefix prefix = partitionWithKey (const ∘ (≡ prefix) ∘ head)

stripSuffix = (fmap unpack ∘) ∘ (∘ pack) ∘ Data.Text.stripSuffix ∘ pack

