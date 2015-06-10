{-# LANGUAGE ViewPatterns, UnicodeSyntax, ExistentialQuantification #-}
module Parse where

import Data.List (stripPrefix, splitAt)
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
  where method args (ln, rn, rrt) = Method (Identifier ln rn) args (parseType rrt)

parseIdentifier ∷ Words → Maybe (Name, Name)
parseIdentifier [nm] = Just (nm, nm)
parseIdentifier [rn, "as", ln] = Just (ln, rn)
parseIdentifier _ = Nothing

parseMetProto ∷ Words → Maybe (Name, Name, RawType)
parseMetProto ["met", nm, rrt] = Just (nm, nm, rrt)
parseMetProto ["met", rn, "as", ln, rrt] = Just (ln, rn, rrt)
parseMetProto _ = Nothing

parseRecord ∷ [String] → Maybe Declaration
parseRecord = parseDeclarationAs record parseRecHeader
  where record vars (ln, rn, super) = Record (Identifier ln rn) vars super

parseRecHeader ∷ Words → Maybe (Name, Name, Maybe Typename)
parseRecHeader (splitAt 2 → (("rec":nm), super)) = header (nm, super)
  where header (parseIdAndSup → (Just (ln, rn), s)) = Just (ln, rn, s)
        header _ = Nothing
        parseIdAndSup = parseIdentifier ⁂ parseSuper
        parseSuper [] = Nothing
        parseSuprt ws = Just (unwords ws)

parseDeclarationAs ∷ ([Variable] → α → Declaration) → (Words → Maybe α) → [String] → Maybe Declaration
parseDeclarationAs _ _ [] = Nothing
parseDeclarationAs construct parseHeader (head:vars) = construct' `fmap` header
  where header = parseHeader (words head)
        construct' = construct ∘ catMaybes $ map (parseVar ∘ words) vars

parseVar ∷ Words → Maybe Variable
parseVar (n:rtdv) | (not ∘ null) rtdv = parseVar' (n, join rtdv)
parseVar _ = Nothing

parseVar' ∷ (Name, String) → Maybe Variable
parseVar' (n, rtdv) = Just (Variable n (parseType t) dv)
  where dv = if null rdv then Nothing else Just (tail rdv)
        (t, rdv) = break (≡ '=') rtdv

parseType ∷ RawType → Type
parseType (stripSuffix "?" → Just t) = Optional (parseType t)
parseType (stripPrefix "[" → Just t) = (Array ∘ parseType ∘ init) t
parseType (stripPrefix "{" → Just t) = parseDictType t -- TODO: allow [:]
parseType u = TypeName (trim u)
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

stripComment = takeWhile (≠ '-')

