{-# LANGUAGE ViewPatterns, UnicodeSyntax, ExistentialQuantification #-}
module Parse where

import Data.List (stripPrefix)
import Data.Maybe (maybe, catMaybes)
import Data.Char (isSpace)
import Control.Monad (join, mplus)

import Util
import Language

parseDeclaration ∷ String → Maybe Declaration
parseDeclaration (lines → ls) = parseMethod ls `mplus` parseRecord ls

type Words = [String]
type RawType = String

parseMethod ∷ [String] → Maybe Declaration
parseMethod = parseDeclarationAs method proto
  where method vars (rn, rrt, n) = Method rn (parseType rrt) n vars
        proto ∷ Words → Maybe (Name, RawType, Name)
        proto ["met", rn, rrt] = Just (rn, rrt, rn)
        proto ["met", rn, rrt, n] = Just (rn, rrt, n)
        proto _ = Nothing

parseRecord ∷ [String] → Maybe Declaration
parseRecord = parseDeclarationAs record named
  where record vars (nm, super) = Record nm vars super
        named ["rec", nm, super] = Just (nm, Just super)
        named ["rec", nm] = Just (nm, Nothing)
        named _ = Nothing

parseDeclarationAs ∷ ([Variable] → α → Declaration) → (Words → Maybe α) → [String] → Maybe Declaration
parseDeclarationAs _ _ [] = Nothing
parseDeclarationAs construct parseHeader (head:vars) = construct' `fmap` header
  where header = parseHeader (words head)
        construct' = construct (map parseVar vars)

parseVar ∷ String → Variable
parseVar (words → (n:rtdv)) | (not ∘ null) rtdv = parseVar' (n, join rtdv)
parseVar s = error $ s ⧺ " — expecting variable declaration."

parseVar' ∷ (Name, String) → Variable
parseVar' (n, rtdv) = Variable n (parseType t) dv
  where dv | null rdv = Nothing
           | otherwise = Just (tail rdv)
        (t, rdv) = break (≡ '=') rtdv

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

