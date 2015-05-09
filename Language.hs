{-# LANGUAGE UnicodeSyntax #-}
module Language where

import Control.Monad.Unicode
import Control.Arrow.Unicode
import Control.Monad (join)
import Data.List (partition)

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

data Spec = Spec { fromSpec ∷ [Declaration] } deriving Show

translator ∷ Language → Spec → (String, String)
translator (Language gen we wi) = fromSpec ⋙ partition isRec ⋙ gen' ⋙ we ⁂ wi
  where gen' = join (⁂) (gen =≪)
        isRec (Record _ _) = True
        isRec _ = False

