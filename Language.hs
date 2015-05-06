{-# LANGUAGE UnicodeSyntax #-}
module Language where

import Control.Monad.Unicode
import Control.Arrow.Unicode

type Name = String
type PrettyName = String -- interface name alias for remote call
type Typename = String
data Type = Array Type | Dictionary Type Type
          | Optional Type | Typename String deriving (Show, Eq)
type RawType = String -- unparsed type
data Variable = Variable Name Type deriving (Show, Eq)
data Function = Function PrettyName Name [Variable] Type deriving Show
data Record = Record Name [Variable] deriving Show

data Language = Language
    { function ∷ Function → String
    , record ∷ Record → String }

type Spec = ([Record], [Function])

translator ∷ Language → Spec → (String, String)
translator (Language f r) = (r =≪) ⁂ (f =≪)

