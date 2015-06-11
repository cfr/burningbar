{-# LANGUAGE UnicodeSyntax, TypeSynonymInstances, FlexibleInstances #-}

module Checker where

import Control.Monad (mplus, join)
import Control.Arrow (second)
import Data.Maybe (fromMaybe, fromJust)
import Data.Char (isAlphaNum)
import Text.Printf (printf)

import Util
import Language
import Parse

class Checkable α where
  check ∷ α → Maybe String

instance Checkable Declaration where
  check (Record (Identifier ln rn) _ s) = validName ln `mplus` validSuper s
  check (Method (Identifier ln rn) _ t) = validName ln `mplus` validType t

instance Checkable Variable where
  check (Variable nm t dv) = validName nm `mplus` validType t

instance Checkable (Maybe α) where
  check (Just _) = Nothing
  check Nothing = Just "failed to parse"

instance Checkable String where
  check line = if null clean then Nothing
               else foldr mplus Nothing [checkVar, checkDecl, parsed]
    where clean = (stripComment ∘ trim) line
          checkVar = if checkDecl ≡ Nothing -- don't parse if var if line is valid decl
                     then Nothing else check =≪ var
          checkDecl = check =≪ decl
          parsed | (decl ≡ Nothing) ∧ (var ≡ Nothing) = Just "failed to parse"
                 | otherwise = Nothing
          var = parseVar clean
          decl = parseDeclaration clean


validName nm = if all isSwiftId nm then Nothing
               else Just ("invalid name " ⧺ nm)
isSwiftId c = isAlphaNum c ∨ (c ≡ '_')
validSuper sup = if all isSwiftId names then Nothing
                 else Just ("invalid proto " ⧺ fromJust sup)
  where names = separateBy ',' (fromMaybe [] sup) ≫= trim

validType (Dictionary k _) = if k ≠ TypeName "String"
                             then Just ("not String key found (" ⧺ show k ⧺ ")")
                             else Nothing
validType (TypeName t) = validName t
validType _ = Nothing


checkSpec ∷ String → String
checkSpec = (≫= valid) ∘ enumerateLines
  where valid = second check ⋙ printError

enumerateLines ∷ String → [(Integer, String)]
enumerateLines = zip [1..] ∘ lines

printError ∷ (Integer, Maybe String) → String
printError (n, Just error) = printf "spec line %03d: %s.\n" n error
printError _ = ""

-- TODO:
-- no optionals in met args
-- validate paragraphs or ignore?
-- check matching parens
-- only prims and newtypes in types?

