{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}
module Unicode where

-- ripped from https://github.com/roelvandijk/base-unicode-symbols

import Prelude (undefined)
import Data.Bool (Bool, (&&), (||), not)
import Data.Eq (Eq, (==), (/=))
import Data.Ord  (Ord, (<=), (>=))
import Data.Function (flip)
import Control.Category (Category, (.))
import Control.Applicative (Applicative, Alternative, (<*>), (<|>), (<$>))
import Data.Functor (Functor)
import Control.Arrow (Arrow, (>>>), (***), (&&&))
import Control.Monad (Monad, (>>=), (>>), (=<<))
import Data.List ((++), elem, notElem)
import System.FilePath.Posix ((</>), (<.>))

(⊥) ∷ α
(⊥) = undefined

-- Bool
infixr 2 ∨
infixr 3 ∧
(¬) ∷ Bool → Bool
(¬) = not
(∧) ∷ Bool → Bool → Bool
(∧) = (&&)
(∨) ∷ Bool → Bool → Bool
(∨) = (||)

-- Eq and Ord
infix  4 ≡, ≠, ≤, ≥
(≡) ∷ Eq α ⇒ α → α → Bool
(≡) = (==)
(≠) ∷ Eq α ⇒ α → α → Bool
(≠) = (/=)
(≥) ∷ Ord α ⇒ α → α → Bool
(≥) = (>=)
(≤) ∷ Ord α ⇒ α → α → Bool
(≤) = (<=)

-- Category, Applicative
infixr 9 ∘
(∘) ∷ Category c ⇒ c β γ → c α β → c α γ
(∘) = (.)
infixl 4 ⊛, ⦶, ⊚
(⊚) ∷ Functor f ⇒ (α → β) → f α → f β
(⊚) = (<$>)
(⦶) ∷ Alternative f ⇒ f α → f α → f α
(⦶) = (<|>)
(⊛) ∷ Applicative f ⇒ f (α → β) → f α → f β
(⊛) = (<*>)

-- Arrow
infixr 3 ⁂, ⩕
infixr 1 ⋙
(⩕) ∷ Arrow a ⇒ a α β → a α β' → a α (β, β')
(⩕) = (&&&)
(⁂) ∷ Arrow a ⇒ a α β → a α' β' → a (α, α') (β, β')
(⁂) = (***)
(⋙) ∷ Arrow a ⇒ a α β → a β γ → a α γ
(⋙) = (>>>)

-- Monad
infixl 1 ≫=, ≫, =≪
(≫=) ∷ Monad m ⇒ m α → (α → m β) → m β
(≫=) = (>>=)
(≫) ∷ Monad m ⇒ m α → m β → m β
(≫) = (>>)
(=≪) ∷ Monad m ⇒ (α → m β) → m α → m β
(=≪) = (=<<)

-- List
infixr 5 ⧺
(⧺) ∷ [α] → [α] → [α]
(⧺) = (++)
(∈) ∷ Eq α ⇒ α → [α] → Bool
(∈) = elem
(∉) ∷ Eq α ⇒ α → [α] → Bool
(∉) = notElem

-- Filepath
(⧄) = (</>)
(⊡) = (<.>)

