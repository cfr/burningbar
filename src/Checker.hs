{-# LANGUAGE ViewPatterns, UnicodeSyntax #-}

module Checker where

import Control.Monad (mplus)
import Control.Arrow (second)
import Data.Maybe (fromMaybe, fromJust)
import Data.Char (isAlphaNum)
import Text.Printf (printf)

import Util
import Language
import Parse

data SpecLine = Decl Declaration | VarOrArg Variable | EmptyLine

{- TODO:
 - Checkable class
 - instances for EmptyLine, Decl
 -}

parseLine ∷ String → Maybe SpecLine
parseLine line  = if null clean then Just EmptyLine else parseDeclOrVar
  where clean = (stripComment ∘ trim) line
        decl = Decl `fmap` (parseDeclaration clean)
        var = VarOrArg `fmap` (parseVar clean)
        parseDeclOrVar = decl `mplus` var

validSpecLine ∷ Maybe SpecLine → Maybe String
validSpecLine (Just (Decl decl)) = validHeadOrProto decl
validSpecLine (Just (VarOrArg v)) = validVar v
validSpecLine (Just EmptyLine) = Nothing
validSpecLine Nothing = Just "failed to parse"

validVar (Variable nm t dv) = validName nm `mplus` validType t
validHeadOrProto (Record (Identifier ln rn) _ s) = validName ln `mplus` validSuper s
validHeadOrProto (Method (Identifier ln rn) _ t) = validName ln `mplus` validType t

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


check ∷ String → String
check = (≫= valid) ∘ enumerateLines
  where valid = second (validSpecLine ∘ parseLine) ⋙ printError

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
-- ensure equatable?
--

