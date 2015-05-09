{-# LANGUAGE ViewPatterns, UnicodeSyntax #-}
module Parse (Spec, parse, Language(..), Typename, translator) where

import Prelude hiding (lookup)
import Data.List (stripPrefix)
import Data.Map (Map, partitionWithKey, mapKeys
                , lookup, member, (!), toList)
import Data.Maybe (maybe, catMaybes)
import Data.Char (isSpace)
import Control.Monad (join)
import Control.Arrow (second)

import Unicode

import Language

-- met remoteName returnType [name]
--  name type
--  ...
--
-- rec name
--  name type
--  ...

parseDeclaration ∷ String → Maybe Declaration
parseDeclaration (lines → ls) = parseMethod ls ⦶ parseRecord ls

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

parseDeclarationAs ∷ ([Variable] → α → Declaration) → (Words → Maybe α) → Lines → Maybe Declaration
parseDeclarationAs construct parseHeader (head:vars) = construct' ⊚ header
  where header = parseHeader (words head)
        construct' = construct (map parseVar vars)
parseDeclarationAs con parseHeader [] = Nothing

parseVar ∷ String → Variable
parseVar (words → (n:rt)) {-| not null rt-} = parseVar' (n, join rt)
parseVar s = error $ s ⧺ "expecting variable declaration."

parseVar' ∷ (Name, RawType) → Variable
parseVar' (n, rt) = Variable n (parseType rt)

parseType ∷ RawType → Type
parseType (stripSuffix "?" → Just t) = Optional (parseType t)
parseType (stripPrefix "[" → Just t) = (Array ∘ parseType ∘ init) t -- TODO: check "]"
parseType (stripPrefix "{" → Just t) = parseDictType t
parseType u = Typename (trim u)
parseDictType (stripSuffix "}" → Just t) = Dictionary keyType valType
  where (keyType, valType) = join (⁂) parseType (splitAtColon t)
        splitAtColon = (filter notSpace ⁂ tail ∘ filter notSpace) ∘ break (≡ ':')
        notSpace = not ∘ isSpace

paragraphs ∷ Lines → [String]
paragraphs [] = []
paragraphs ls = let (p, rest) = (break null ∘ dropWhile null ∘ map trim) ls
                in unlines p : paragraphs rest

parse ∷ String → Spec
parse = catMaybes ∘ map parseDeclaration ∘ paragraphs ∘ map stripComments ∘ lines
  where stripComments = takeWhile (≠ '-')

stripSuffix ∷ Eq α ⇒ [α] → [α] → Maybe [α]
stripSuffix xs ys = reverse ⊚ stripPrefix (reverse xs) (reverse ys)

trim ∷ String → String
trim = f ∘ f
   where f = reverse ∘ dropWhile isSpace

