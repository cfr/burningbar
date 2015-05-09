{-# LANGUAGE UnicodeSyntax, RecordWildCards #-}
module Language where

import Control.Monad (join)
import Data.List (partition)

import Unicode

type Name = String
type RemoteName = Name
type Typename = String
data Type = Array Type | Dictionary Type Type
          | Optional Type | Typename String deriving (Show, Eq)
type ReturnType = Type
data Variable = Variable Name Type deriving (Show, Eq)
data Declaration = Record Name [Variable]
                 | Method RemoteName ReturnType Name [Variable] deriving Show

data Language = Language
    { generate ∷ Declaration → String
    , wrapEntities ∷ String → String
    , wrapInterface ∷ String → String }

type Spec = [Declaration]

translator ∷ Language → Spec → (String, String)
translator (Language {..}) = partition isRec ⋙ gen' ⋙ wrapEntities ⁂ wrapInterface
  where gen' = join (⁂) (generate =≪)
        isRec (Record _ _) = True
        isRec _ = False

