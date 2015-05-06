{-# LANGUAGE ViewPatterns, UnicodeSyntax #-}
module Translate (Spec(..), toSpec, translator
                 , Language(..), Record(..)
                 , Function(..), Typename) where

import Prelude hiding (lookup)
import Data.List (stripPrefix)
import Data.Map (Map, partitionWithKey, mapKeys
                , lookup, member, (!), toList)
import Data.Maybe (fromMaybe)
import Data.Char (isSpace)
import Control.Monad (join)
import Control.Arrow (second)

import Data.Text (pack, unpack)
import qualified Data.Text (stripSuffix, strip)

import Prelude.Unicode
import Control.Arrow.Unicode

import Language

toSpec ∷ [Map String String] → Spec
toSpec = (map parseRec ⁂ map parseFun) ∘ span isRec where
  isRec = member '_' ∘ mapKeys head

parseRec ∷ Map String String → Record
parseRec m = Record name (parseVars vars) where
  (underscored, vars) = partitionWithPrefix '_' m
  name = underscored ! "_name"

parseVar ∷ (Name, RawType) → Variable
parseVar = uncurry ((∘ parseType ∘ strip) ∘ Variable)

parseVars ∷ Map String String → [Variable]
parseVars = map parseVar ∘ toList

parseType ∷ RawType → Type
parseType (stripSuffix "?" → Just t) = Optional (parseType t)
parseType (stripPrefix "[" → Just t) = (Array ∘ parseType ∘ init) t -- TODO: check "]"
parseType (stripPrefix "{" → Just t) = parseDictType t
parseType u = Typename (strip u)
parseDictType (stripSuffix "}" → Just t) = Dictionary keyType valType
  where (keyType, valType) = join (⁂) parseType (splitAtColon t)
        splitAtColon = (filter notSpace ⁂ tail ∘ filter notSpace) ∘ break (≡ ':')
        notSpace = not ∘ isSpace

parseFun ∷ Map String String → Function
parseFun m = Function name rpc (parseVars args) t where
  (dashed, args) = partitionWithPrefix '-' m
  name = fromMaybe rpc (lookup "-pretty" dashed)
  rpc = dashed ! "-method"
  t = parseType (dashed ! "-returns")

partitionWithPrefix ∷ Eq β ⇒ β → Map [β] a → (Map [β] a, Map [β] a)
partitionWithPrefix prefix = partitionWithKey (const ∘ (≡ prefix) ∘ head)

stripSuffix ∷ String → String → Maybe String
stripSuffix = (fmap unpack ∘) ∘ (∘ pack) ∘ Data.Text.stripSuffix ∘ pack

strip ∷ String → String
strip = unpack ∘ Data.Text.strip ∘ pack

