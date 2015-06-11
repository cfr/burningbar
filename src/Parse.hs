{-# LANGUAGE ViewPatterns, UnicodeSyntax, ExistentialQuantification #-}
module Parse where

import Data.List (stripPrefix, splitAt)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Char (isSpace)
import Control.Monad (join, mplus)

import Util
import Language

paragraphs ∷ [String] → [String]
paragraphs [] = []
paragraphs ls = let (p, rest) = (break null ∘ dropWhile null ∘ map trim) ls
                in unlines p : paragraphs rest

parse ∷ String → Spec
parse = catMaybes ∘ map parseDeclaration ∘ paragraphs ∘ map stripComment ∘ lines

stripComment ∷ String → String
stripComment = takeWhile (≠ '-')

parseDeclaration ∷ String → Maybe Declaration
parseDeclaration (lines → ls) = parseMethod ls `mplus` parseRecord ls

type Words = [String]
type RawType = String

parseMethod ∷ [String] → Maybe Declaration
parseMethod = parseDeclarationAs Method parseRetType "met"
  where parseRetType [] = TypeName "Void"
        parseRetType rrt = parseType rrt

parseRecord ∷ [String] → Maybe Declaration
parseRecord = parseDeclarationAs Record readSup "rec"
  where readSup [] = Nothing
        readSup s = Just s

parseDeclarationAs ∷ (Identifier → [Variable] → α → Declaration) → (String → α)
                     → String → [String] → Maybe Declaration
parseDeclarationAs _ _ _ [] = Nothing
parseDeclarationAs make parseRetOrSuper key (head:vars) = parsedDecl
  where make' = flip make (catMaybes parsedVars)
        parsedVars = map parseVar vars
        parsedDecl = uncurry make' `fmap` (parseHeader ∘ words) head
        parseHeader (k:ws) | key ≡ k = parseIdAndRS $ (splitAtColon ∘ unwords) ws
                           | otherwise = Nothing
        parseIdAndRS (parseId → Just idr, parseRetOrSuper → rs) = Just (idr, rs)
        parseIdAndRS _ = Nothing

parseId ∷ String → Maybe Identifier
parseId (words → [nm]) = Just (Identifier nm nm)
parseId (words → [rn, "as", ln]) = Just (Identifier ln rn)
parseId _ = Nothing

--parseHeaderAliased ∷ [String] → Words → Maybe Declaration
--parseHeaderAliased keys (key:decl) = foldr mplus Nothing ∘ map parseHeader keys
--  where parseHeader k | key ≡ k = parseDecl decl
--                      | otherwise = Nothing
--        parseDecl _ = Nothing

parseVar ∷ String → Maybe Variable
parseVar (words → (n:rtdv)) | (not ∘ null) rtdv = Just var
  where dv = if null rdv then Nothing else Just (tail rdv)
        (rt, rdv) = break (≡ '=') (join rtdv)
        var = Variable n (parseType rt) dv
parseVar _ = Nothing

parseType ∷ RawType → Type
parseType (stripSuffix "?" → Just t) = Optional (parseType t)
parseType (stripPrefix "[" → Just t) = (Array ∘ parseType ∘ init) t
parseType (stripPrefix "{" → Just t) = parseDictType t -- TODO: allow [:]
parseType u = TypeName (trim u)
parseDictType (stripSuffix "}" → Just t) = Dictionary keyType valType
  where (keyType, valType) = join (⁂) parseType (splitAtColon t)

