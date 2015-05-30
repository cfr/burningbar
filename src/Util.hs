{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}
module Util where

-- ripped from https://github.com/roelvandijk/base-unicode-symbols

import Prelude (Int, concat, flip, replicate)
import Data.Bool (Bool, (||))
import Data.Eq (Eq, (==), (/=))
import Control.Category (Category, (.))
import Control.Arrow (Arrow, (>>>), (***), (&&&))
import Control.Monad (Monad, (>>=), (>>), (=<<))
import Data.List ((++), elem, reverse, dropWhile, stripPrefix)
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

-- | remove spaces from the string ends
-- >>> trim " \tsdf   "
-- "sdf"
trim ∷ String → String
trim = let f = reverse ∘ dropWhile isSpace in f ∘ f

stripSuffix ∷ Eq α ⇒ [α] → [α] → Maybe [α]
stripSuffix xs ys = reverse `fmap` stripPrefix (reverse xs) (reverse ys)

-- | repeat string n times
repeatN ∷ Int → String → String
repeatN n s = concat (replicate n s)

-- | n underscores
n_ ∷ Int → String
n_ = flip repeatN "_"

-- | n spaces
s ∷ Int → String
s = flip repeatN " "

