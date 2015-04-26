{-# LANGUAGE UnicodeSyntax #-}
module Language where

type Name = String
type Typename = String
data Type = Array Type | Dictionary Type Type
          | Optional Type | Typename String deriving (Show, Eq)
data Variable = Variable Name Type deriving Show
data Function = Function Name Name [Variable] Type deriving Show
data Record = Record Name [Variable] deriving Show

type Def a = a → String
data Language = Language
    { var  ∷ Def Variable
    , fun  ∷ Def Function
    , typ  ∷ Def Type
    , rec  ∷ Def Record
    , etc  ∷ String
    }

