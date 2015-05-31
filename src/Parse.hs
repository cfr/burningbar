{-# LANGUAGE ViewPatterns, UnicodeSyntax, ExistentialQuantification #-}
module Parse where

import Data.List (stripPrefix)
import Data.Maybe (catMaybes)
import Data.Char (isSpace)
import Control.Monad (join, mplus)

import Util
import Language

parseDeclaration ∷ String → Maybe Declaration
parseDeclaration (lines → ls) = parseMethod ls `mplus` parseRecord ls

type Words = [String]
type RawType = String

parseMethod ∷ [String] → Maybe Declaration
parseMethod = parseDeclarationAs method parseMetProto
  where method vars (rn, rrt, n) = Method rn (parseType rrt) n vars

parseMetProto ∷ Words → Maybe (Name, RawType, Name)
parseMetProto ["met", rn, rrt] = Just (rn, rrt, rn)
parseMetProto ["met", rn, rrt, n] = Just (rn, rrt, n)
parseMetProto _ = Nothing

parseRecord ∷ [String] → Maybe Declaration
parseRecord = parseDeclarationAs record parseRecHeader
  where record vars (nm, super) = Record nm vars super

parseRecHeader ∷ Words → Maybe (Name, Maybe Typename)
parseRecHeader ["rec", nm, super] = Just (nm, Just super)
parseRecHeader ["rec", nm] = Just (nm, Nothing)
parseRecHeader _ = Nothing

parseDeclarationAs ∷ ([Variable] → α → Declaration) → (Words → Maybe α) → [String] → Maybe Declaration
parseDeclarationAs _ _ [] = Nothing
parseDeclarationAs construct parseHeader (head:vars) = construct' `fmap` header
  where header = parseHeader (words head)
        construct' = construct ∘ catMaybes $ map parseVar vars

parseVar ∷ String → Maybe Variable
parseVar (words → (n:rtdv)) | (not ∘ null) rtdv = parseVar' (n, join rtdv)
parseVar _ = Nothing -- TODO: move not null to checker

parseVar' ∷ (Name, String) → Maybe Variable
parseVar' (n, rtdv) = Just (Variable n (parseType t) dv)
  where dv = if null rdv then Nothing else Just (tail rdv)
        (t, rdv) = break (≡ '=') rtdv

parseType ∷ RawType → Type
parseType (stripSuffix "?" → Just t) = Optional (parseType t)
parseType (stripPrefix "[" → Just t) = (Array ∘ parseType ∘ init) t -- FIXME: check "]"
parseType (stripPrefix "{" → Just t) = parseDictType t
parseType u = TypeName (trim u) -- TODO: only prims and introduced?
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

