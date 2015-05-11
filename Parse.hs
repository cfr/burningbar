{-# LANGUAGE ViewPatterns, UnicodeSyntax, ExistentialQuantification #-}
module Parse (Spec, parse, Language(..), Typename, translator, parseType) where

import Data.List (stripPrefix)
import Data.Maybe (maybe, catMaybes)
import Data.Char (isSpace)
import Control.Monad (join)

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
parseMethod = parseDeclarationAs met proto
  where proto ∷ Words → Maybe (Name, RawType, Name)
        proto ["met", rn, rrt] = Just (rn, rrt, rn)
        proto ["met", rn, rrt, n] = Just (rn, rrt, n)
        proto _ = Nothing
        met vars (rn, rrt, n) = Method rn (parseType rrt) n vars

parseRecord ∷ Lines → Maybe Declaration
parseRecord = parseDeclarationAs (flip Record) named
  where named ["rec", nm] = Just nm
        named _ = Nothing

parseDeclarationAs ∷ ([Variable] → α → Declaration) → (Words → Maybe α) → Lines → Maybe Declaration
parseDeclarationAs _ _ [] = Nothing
parseDeclarationAs construct parseHeader (head:vars) = construct' ⊚ header
  where header = parseHeader (words head)
        construct' = construct (map parseVar vars)

parseVar ∷ String → Variable
parseVar (words → (n:rt)) | (not ∘ null) rt = parseVar' (n, join rt)
parseVar s = error $ s ⧺ " — expecting variable declaration."

parseVar' ∷ (Name, RawType) → Variable
parseVar' (n, rt) = Variable n (parseType rt)

parseType ∷ RawType → Type
parseType (stripSuffix "?" → Just t) = Optional (parseType t)
parseType (stripPrefix "[" → Just t) = (Array ∘ parseType ∘ init) t -- TODO: check "]"
parseType (stripPrefix "{" → Just t) = parseDictType t
parseType u = Typename (trim u) -- TODO: only prims and introduced?
parseDictType (stripSuffix "}" → Just t) = Dictionary keyType valType
  where (keyType, valType) = join (⁂) parseType (splitAtColon t)
        splitAtColon = (filter notSpace ⁂ tail ∘ filter notSpace) ∘ break (≡ ':')
        notSpace = not ∘ isSpace

paragraphs ∷ Lines → [String]
paragraphs [] = []
paragraphs ls = let (p, rest) = (break null ∘ dropWhile null ∘ map trim) ls
                in unlines p : paragraphs rest

parse ∷ String → Spec
parse = catMaybes ∘ map parseDeclaration ∘ paragraphs ∘ map stripComment ∘ lines
  where stripComment = takeWhile (≠ '-')

stripSuffix ∷ Eq α ⇒ [α] → [α] → Maybe [α]
stripSuffix xs ys = reverse ⊚ stripPrefix (reverse xs) (reverse ys)

--(⊑) ∷ [a] → Maybe a → Maybe a
--(⊑) = lift stripPrefix
--(⊒) = lift stripSuffix
--parseType ("[" ⊑ rt ⊒ "]") | Just t = (Array ∘ parseType) t

stripPrefAndSuff ∷ Eq α ⇒ ([α], [α]) → [α] → Maybe [α]
stripPrefAndSuff = undefined

data SubstringOf = ∀ α. Eq α ⇒  SubstringOf ([α], [α])
data IsSubstring = ∀ α. Eq α ⇒  IsPrefix { stripP ∷ SubstringOf → Maybe [α] }
                 | ∀ α. Eq α ⇒  IsSuffix { stripS ∷ SubstringOf → Maybe [α] }
                 | ∀ α. Eq α ⇒  IsRoot { stripR ∷ SubstringOf → Maybe ([α], [α]) } -- PS | PR | SR | PSR

stripSubstring ∷ IsSubstring
stripSubstring = (⊥)

trim ∷ String → String
trim = let f = reverse ∘ dropWhile isSpace in f ∘ f


