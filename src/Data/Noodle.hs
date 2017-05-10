{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Data.Noodle (Noodle, insertL, insertR) where

import Data.FingerTree as FT
import Data.AdditiveGroup
import Data.AffineSpace
import Data.List (foldl')

instance AdditiveGroup p => Measured (Sum p) (Item p a) where
  measure (Item d _) = Sum d

data Item p a = Item !p !a deriving (Eq, Ord)

data Noodle p a = Noodle !p !(FingerTree (Sum (Diff p)) (Item (Diff p) a)) !p
  -- Invariant: ∀(Noodle p s q). q .-. p == measure s
  | Empty

deriving instance (Eq p, Eq (Diff p), Eq a) => Eq (Noodle p a)
deriving instance (Ord p, Ord (Diff p), Ord a) => Ord (Noodle p a)

instance (AffineSpace p, Show p, Show a) => Show (Noodle p a) where
  showsPrec p xs = showParen (p > 10)
    $ showString "fromList " . shows (toList xs)

insertL :: AffineSpace p => p -> a -> Noodle p a -> Noodle p a
insertL p a Empty = Noodle p (FT.singleton (Item zeroV a)) p
insertL p a (Noodle p0 s p1) = Noodle p (Item (p0 .-. p) a <| s) p1

insertR :: AffineSpace p => Noodle p a -> p -> a -> Noodle p a
insertR Empty p a = Noodle p (FT.singleton (Item zeroV a)) p
insertR (Noodle p0 s p1) p a = Noodle p0 (s |> Item (p .-. p1) a) p

fromList :: AffineSpace p => [(p, a)] -> Noodle p a
fromList = foldl' (uncurry . insertR) Empty

toList :: AffineSpace p => Noodle p a -> [(p, a)]
toList (Noodle p0 s _) = foldr
  (\(Item d a) k p -> (p, a) : k (p .+^ d)) (const []) s p0

break :: AffineSpace p => (p -> Bool) -> Noodle p a -> (Noodle p a, Noodle p a)
break cond (Noodle p0 s p1) = (Noodle p0 l p, Noodle p r p1) where
  (l, r) = split (cond . (p0 .+^) . getSum) s
  p = p0 .+^ getSum (measure l)
break _ Empty = (Empty, Empty)
