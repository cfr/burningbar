{-# LANGUAGE UnicodeSyntax #-}
module Language where

type Name = String
type Typename = String
data Type = Array Type | Dictionary Type Type
          | Optional Type | Typename String deriving (Show, Eq)
data Variable = Variable Name Type deriving (Show, Eq)
data Function = Function Name Name [Variable] Type deriving Show
data Record = Record Name [Variable] deriving Show

type Define a = a → String
data Language = Language
    { function  ∷ Define Function
    , record  ∷ Define Record
    , header ∷ String
    }

type Spec = ([Record], [Function])

