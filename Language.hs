{-# LANGUAGE UnicodeSyntax, RecordWildCards #-}
module Language where

import Control.Monad (join)
import Data.List (partition)

import Unicode

type Name = String
type Typename = String
data Type = Array Type | Dictionary { key ∷ Type, value ∷ Type }
          | Optional Type | Typename String deriving (Show, Eq)

data Variable = Variable Name Type deriving Show
data Declaration = Record { name ∷ Name, vars ∷ [Variable] }
                 | Method { remote ∷ Name, returns ∷ Type,
                            name ∷ Name, args ∷ [Variable] } deriving Show

data Language = Language { generate ∷ Declaration → String
                         , wrapEntities ∷ String → String
                         , wrapInterface ∷ String → String }

type Spec = [Declaration]

translator ∷ Language → Spec → (String, String)
translator (Language {..}) = partition isRec ⋙ gen ⋙ wrapEntities ⁂ wrapInterface
  where gen = join (⁂) (generate =≪)
        isRec Record {} = True
        isRec _ = False

primitives ∷ [Type]
primitives = foldr (=≪) atoms [opt, dict, opt, ap Array, opt]
  where atoms = map Typename ["String", "NSNumber"] -- TODO: "Bool", "Int", "Float", "URL", "IntString"
        ap = (take 2 ∘) ∘ iterate
        opt = ap Optional
        dict a = a : [Dictionary x a | x ← atoms]

primitive ∷ Type → Bool
primitive = (∈ primitives)

