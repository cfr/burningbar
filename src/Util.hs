{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax, ViewPatterns #-}
module Util where

-- ripped from https://github.com/roelvandijk/base-unicode-symbols

import Prelude (Int, concat, flip, replicate
               , filter, tail, otherwise)
import Data.Bool (Bool, (||), (&&), not)
import Data.Eq (Eq, (==), (/=))
import Control.Category (Category, (.))
import Control.Arrow (Arrow, (>>>), (***), (&&&))
import Control.Monad (Monad, (>>=), (>>), (=<<))
import Data.List ((++), elem, notElem, reverse, dropWhile,
                  stripPrefix, unfoldr, drop, break)
import Data.Char (isSpace)
import Data.Functor (fmap)
import Data.Maybe (Maybe(..))
import Data.String (String)

infix 4 ≡, ≠
(≡) ∷ Eq α ⇒ α → α → Bool
(≡) = (==)
(≠) ∷ Eq α ⇒ α → α → Bool
(≠) = (/=)
(∨) ∷ Bool → Bool → Bool
(∨) = (||)
(∧) ∷ Bool → Bool → Bool
(∧) = (&&)

infixr 9 ∘
(∘) ∷ Category c ⇒ c β γ → c α β → c α γ
(∘) = (.)

infixr 3 ⁂
infixr 1 ⋙
(⁂) ∷ Arrow a ⇒ a α β → a α' β' → a (α, α') (β, β')
(⁂) = (***)
(⋙) ∷ Arrow a ⇒ a α β → a β γ → a α γ
(⋙) = (>>>)

infixl 1 ≫=, ≫, =≪
(≫=) ∷ Monad m ⇒ m α → (α → m β) → m β
(≫=) = (>>=)
(≫) ∷ Monad m ⇒ m α → m β → m β
(≫) = (>>)
(=≪) ∷ Monad m ⇒ (α → m β) → m α → m β
(=≪) = (=<<)

infixr 5 ⧺
(⧺) ∷ [α] → [α] → [α]
(⧺) = (++)
(∈) ∷ Eq α ⇒ α → [α] → Bool
(∈) = elem
(∉) ∷ Eq α ⇒ α → [α] → Bool
(∉) = notElem

-- | remove spaces from the string ends
-- >>> trim " \tsdf   "
-- "sdf"
trim ∷ String → String
trim = let f = reverse ∘ dropWhile isSpace in f ∘ f

stripSuffix ∷ Eq α ⇒ [α] → [α] → Maybe [α]
stripSuffix xs ys = reverse `fmap` stripPrefix (reverse xs) (reverse ys)

separateBy ∷ Eq α ⇒ α → [α] → [[α]]
separateBy c = unfoldr sep
  where sep [] = Nothing
        sep (break (≡ c) → l) = Just (fmap (drop 1) l)

splitAtColon ∷ String → (String, String)
splitAtColon s | ':' ∉ s = (s, [])
               | otherwise = clear (break (≡ ':') s)
  where clear = trim ⁂ trim ∘ tail

-- | repeat string n times
repeatN ∷ Int → String → String
repeatN n s = concat (replicate n s)

-- | n underscores
n_ ∷ Int → String
n_ = flip repeatN "_"

-- | n spaces
s ∷ Int → String
s = flip repeatN " "

