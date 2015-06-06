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

validName nm = if all isSwiftId nm then Nothing
               else Just ("invalid name " ⧺ nm)
isSwiftId c = isAlphaNum c ∨ (c ≡ '_')
validSuper sup = if all isAlphaNum names then Nothing
                 else Just ("invalid proto " ⧺ fromJust sup)
  where names = separateBy ',' (fromMaybe [] sup) ≫= trim
validType (Dictionary k _) = if k ≠ TypeName "String"
                             then Just ("not String key found (" ⧺ show k ⧺ ")")
                             else Nothing
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
-- only optional newtypes in recs
-- no optionals in met args
-- validate paragraphs or ignore?
-- check matching parens
-- only prims and newtypes in types?
-- ensure equatable?
--

