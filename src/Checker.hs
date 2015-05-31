{-# LANGUAGE ViewPatterns, UnicodeSyntax #-}

module Checker where

import Control.Monad (mplus)
import Control.Arrow (second)
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

validateSpecLine ∷ Maybe SpecLine → Either String SpecLine
validateSpecLine Nothing = Left "failed to parse"
validateSpecLine (Just (VarOrArg v)) = validateVar v
validateSpecLine (Just EmptyLine) = Right EmptyLine
validateSpecLine (Just hp) = validateHeaderOrProto hp

validateVar v = Right (VarOrArg v)
validateHeaderOrProto = Right

check ∷ String → String
check = (≫= validate) ∘ enumerateLines
  where validate = second (validateSpecLine ∘ parseLine) ⋙ printError

enumerateLines ∷ String → [(Integer, String)]
enumerateLines = zip [1..] ∘ lines

printError ∷ (Integer, Either String SpecLine) → String
printError (n, Left error) = printf "spec line %03d: %v.\n" n error
printError _ = ""

-- TODO:
-- only optional newtypes in recs
-- no optionals in met args
-- check chars in names
-- validate paragraphs or ignore?
-- check matching parens
-- only prims and newtypes in types?
-- ensure equatable?
--

