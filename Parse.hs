{-# LANGUAGE ViewPatterns, UnicodeSyntax #-}
module Parse (Spec, parse, Language(..), Typename, translator) where

import Prelude hiding (lookup)
import Data.List (stripPrefix)
import Data.Map (Map, partitionWithKey, mapKeys
                , lookup, member, (!), toList)
import Data.Maybe (fromMaybe, maybe, catMaybes)
import Data.Char (isSpace)
import Control.Monad (join)
import Control.Arrow (second)

import Data.Text (pack, unpack)
import qualified Data.Text (stripSuffix, strip)

import Control.Applicative
import Prelude.Unicode
import Control.Arrow.Unicode

import Language

-- met remoteName returnType [name]
--  name type
--  ...
--
-- rec name
--  name type
--  ...
--
-- rec(ord) / met(hod) / rem(ark)
parseDeclaration ∷ String → Maybe Declaration
parseDeclaration (lines → ls) = parseMethod ls ⦶ parseRecord ls

-- (+) `fmap` pR pM ==> pure (+) <*> 
-- liftM2 (+) parseRec parseM
-- last = liftM2 mod ps [10]
-- last = mod `fmap` ps <*> [10] --fmap==<$>
-- (pure fromMaybe <*> Comment) == fromMaybe `fmap` Comment
--  'fmap' f x = 'pure' f '<*>' x
(⊚) = (<$>)
(⦶) = (<|>)

type Lines = [String]
type Words = [String]

type RawType = String

parseMethod ∷ Lines → Maybe Declaration
parseMethod = parseDeclarationAs cm proto
  where proto ∷ Words → Maybe (RemoteName, RawType, Name)
        proto ["met", rn, rrt] = Just (rn, rrt, rn)
        proto ["met", rn, rrt, n] = Just (rn, rrt, n)
        proto _ = Nothing
        cm vars (rn, rrt, n) = Method rn (parseType rrt) n vars

parseRecord ∷ Lines → Maybe Declaration
parseRecord = parseDeclarationAs (flip Record) name
  where name ["rec", nm] = Just nm
        name _ = Nothing

parseDeclarationAs ∷ ([Variable] → a → Declaration) → (Words → Maybe a) → Lines → Maybe Declaration
parseDeclarationAs con parseHeader (head:vars) = con' ⊚ header
  where header = parseHeader (words head)
        con' = con (map parseVar vars)
parseDeclarationAs con parseHeader [] = Nothing

parseVar ∷ String → Variable
parseVar (words → (n:rt)) = parseVar' (n, join rt)
parseVar s = error $ s ⧺ "expecting variable declaration."

parseVar' ∷ (Name, RawType) → Variable
parseVar' (n, rt) = Variable n (parseType rt)

parseType ∷ RawType → Type
parseType (stripSuffix "?" → Just t) = Optional (parseType t)
parseType (stripPrefix "[" → Just t) = (Array ∘ parseType ∘ init) t -- TODO: check "]"
parseType (stripPrefix "{" → Just t) = parseDictType t
parseType u = Typename (strip u)
parseDictType (stripSuffix "}" → Just t) = Dictionary keyType valType
  where (keyType, valType) = join (⁂) parseType (splitAtColon t)
        splitAtColon = (filter notSpace ⁂ tail ∘ filter notSpace) ∘ break (≡ ':')
        notSpace = not ∘ isSpace

paragraphs ∷ Lines → [String]
paragraphs [] = []
paragraphs lines = let (p, rest) = (break null ∘ dropWhile null ∘ map strip) lines
                   in unlines p : paragraphs rest

parse ∷ String → Spec
parse = Spec ∘ catMaybes ∘ map parseDeclaration ∘ paragraphs ∘ map stripComments ∘ lines
  where stripComments = takeWhile (≠ '-')

stripSuffix ∷ String → String → Maybe String
stripSuffix = (fmap unpack ∘) ∘ (∘ pack) ∘ Data.Text.stripSuffix ∘ pack

strip ∷ String → String
strip = unpack ∘ Data.Text.strip ∘ pack

