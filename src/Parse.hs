{-# LANGUAGE ViewPatterns, UnicodeSyntax, ExistentialQuantification #-}
module Parse (Spec, parse, Language(..), Typename, translator,
              parseType, parseVar, parseRecord, parseMethod) where

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

type Words = [String]
type RawType = String

parseMethod ∷ [String] → Maybe Declaration
parseMethod = parseDeclarationAs met proto
  where proto ∷ Words → Maybe (Name, RawType, Name)
        proto ["met", rn, rrt] = Just (rn, rrt, rn)
        proto ["met", rn, rrt, n] = Just (rn, rrt, n)
        proto _ = Nothing
        met vars (rn, rrt, n) = Method rn (parseType rrt) n vars

parseRecord ∷ [String] → Maybe Declaration
parseRecord = parseDeclarationAs rec named
  where named ["rec", nm, super] = Just (nm, Just super)
        named ["rec", nm] = Just (nm, Nothing)
        named _ = Nothing
        rec vars (nm, super) = Record nm vars super

parseDeclarationAs ∷ ([Variable] → α → Declaration) → (Words → Maybe α) → [String] → Maybe Declaration
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
parseType (stripPrefix "[" → Just t) = (Array ∘ parseType ∘ init) t -- FIXME: check "]"
parseType (stripPrefix "{" → Just t) = parseDictType t
parseType u = Typename (trim u) -- TODO: only prims and introduced?
parseDictType (stripSuffix "}" → Just t) = Dictionary keyType valType
  where (keyType, valType) = join (⁂) parseType (splitAtColon t)
        splitAtColon = (filter notSpace ⁂ tail ∘ filter notSpace) ∘ break (≡ ':')
        notSpace = not ∘ isSpace

paragraphs ∷ [String] → [String]
paragraphs [] = []
paragraphs ls = let (p, rest) = (break null ∘ dropWhile null ∘ map trim) ls
                in unlines p : paragraphs rest

parse ∷ String → Spec
parse = catMaybes ∘ map parseDeclaration ∘ paragraphs ∘ map stripComment ∘ lines
  where stripComment = takeWhile (≠ '-')

stripSuffix ∷ Eq α ⇒ [α] → [α] → Maybe [α]
stripSuffix xs ys = reverse ⊚ stripPrefix (reverse xs) (reverse ys)

trim ∷ String → String
trim = let f = reverse ∘ dropWhile isSpace in f ∘ f

