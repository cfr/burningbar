{-# LANGUAGE ViewPatterns, UnicodeSyntax #-}

module Checker where

import Control.Monad (mplus)
import Control.Arrow (second)
import Data.Char (isAlphaNum)
import Data.Maybe (fromJust)
import Text.Printf (printf)

import Util
import Language
import Parse

data SpecLine = Proto (Name, RawType, Name) | Header (Name, Maybe Typename)
              | VarOrArg Variable | EmptyLine

parseLine ∷ String → Maybe SpecLine
parseLine (words ∘ stripComment ∘ trim → l) = if null l then Just EmptyLine
                                                        else parseDecl
  where parseDecl = header l `mplus` proto l `mplus` var l
        header = (Header `fmap`) ∘ parseRecHeader
        proto = (Proto `fmap`) ∘ parseMetProto
        var = (VarOrArg `fmap`) ∘ parseVar

validSpecLine ∷ Maybe SpecLine → Maybe String
validSpecLine Nothing = Just "failed to parse"
validSpecLine (Just (VarOrArg v)) = validVar v
validSpecLine (Just EmptyLine) = Nothing
validSpecLine (Just hp) = validHeadOrProto hp

validVar (Variable nm t dv) = validName nm `mplus` validType t
validHeadOrProto (Header (nm, sup)) = validName nm `mplus` validSuper sup
validHeadOrProto (Proto (n, rrt, nm)) = validName nm
                                `mplus` validType (parseType rrt)

validName nm = if all isAlphaNum nm then Nothing
               else Just ("invalid name " ⧺ nm)
validSuper (fromJust → sup) = if all isAlphaNum names then Nothing
                              else Just ("invalid proto " ⧺ sup)
  where names = separateBy ',' sup ≫= trim
validType (Dictionary k _) = if k ≠ TypeName "String"
                             then Just ("no-String key (" ⧺ show k ⧺ ")")
                             else Nothing
validType _ = Nothing

check ∷ String → String
check = (≫= valid) ∘ enumerateLines
  where valid = second (validSpecLine ∘ parseLine) ⋙ printError

enumerateLines ∷ String → [(Integer, String)]
enumerateLines = zip [1..] ∘ lines

printError ∷ (Integer, Maybe String) → String
printError (n, Just error) = printf "spec line %03d: %v.\n" n error
printError _ = ""

-- TODO:
-- only optional newtypes in recs
-- no optionals in met args
-- validate paragraphs or ignore?
-- check matching parens
-- only prims and newtypes in types?
-- ensure equatable?
--

