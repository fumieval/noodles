{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Data.Noodle (Noodle
    , insert
    , fromAscList
    , Data.Noodle.toList
    , minViewWithKey
    , maxViewWithKey
    , splitL
    , splitR
    , lookupLE
    , lookupGT) where

import Data.Foldable as F
import Data.FingerTree as FT
import Data.List (foldl')

newtype MaxMaybe a = MaxMaybe { getMaxMaybe :: Maybe a }

instance Ord i => Monoid (MaxMaybe i) where
  mempty = MaxMaybe Nothing
  mappend (MaxMaybe (Just a)) (MaxMaybe (Just b)) = MaxMaybe $ Just (max a b)
  mappend (MaxMaybe Nothing) (MaxMaybe b) = MaxMaybe b
  mappend (MaxMaybe a) (MaxMaybe Nothing) = MaxMaybe a

instance Ord k => Measured (MaxMaybe k) (Item k a) where
  measure (Item p _) = MaxMaybe $ Just p

data Item p a = Item !p !a deriving (Eq, Ord, Functor)

newtype Noodle k a = Noodle { unNoodle :: FingerTree (MaxMaybe k) (Item k a) }

instance Functor (Noodle k) where
  fmap f = Noodle . unsafeFmap (fmap f) . unNoodle

instance (Show k, Show a) => Show (Noodle k a) where
  showsPrec p xs = showParen (p > 10)
    $ showString "fromList " . shows (Data.Noodle.toList xs)

instance Ord k => Foldable (Noodle k) where
  foldMap f (Noodle ft) = foldMap (\(Item _ a) -> f a) ft
  null (Noodle ft) = FT.null ft

instance Ord k => Monoid (Noodle k a) where
  mempty = Noodle mempty
  mappend (Noodle l) (Noodle r)
    | _ :> Item x0 _ <- FT.viewr l
    , Item x1 _ :< _ <- FT.viewl r
    , x0 <= x1 = Noodle (mappend l r)
    | FT.null l = Noodle r
    | FT.null r = Noodle l
    | otherwise = error "TODO"

insert :: Ord k => k -> a -> Noodle k a -> Noodle k a
insert k a (Noodle ft)
  | _ :> Item k' _ <- FT.viewr ft, k >= k' = Noodle (ft |> Item k a)
  | FT.null ft = Noodle $ FT.singleton (Item k a)
  | otherwise = error "TODO"

fromAscList :: Ord k => [(k, a)] -> Noodle k a
fromAscList xs = Noodle $ FT.fromList [ Item k v | (k, v) <- xs ]

toList :: Noodle k a -> [(k, a)]
toList (Noodle ft) = [(k, v) | Item k v <- F.toList ft]

minViewWithKey :: Ord k => Noodle k a -> Maybe ((k, a), Noodle k a)
minViewWithKey (Noodle ft) = case FT.viewl ft of
  EmptyL -> Nothing
  Item k a :< ft' -> Just ((k, a), Noodle ft')

maxViewWithKey :: Ord k => Noodle k a -> Maybe ((k, a), Noodle k a)
maxViewWithKey (Noodle ft) = case FT.viewr ft of
  EmptyR -> Nothing
  ft' :> Item k a -> Just ((k, a), Noodle ft')

splitL :: Ord k => k -> Noodle k a -> (Noodle k a, Noodle k a)
splitL k (Noodle ft) = (Noodle l, Noodle r)
  where (l, r) = FT.split (maybe False (>k) . getMaxMaybe) ft

splitR :: Ord k => k -> Noodle k a -> (Noodle k a, Noodle k a)
splitR k (Noodle ft) = (Noodle l, Noodle r)
  where (l, r) = FT.split (maybe False (>=k) . getMaxMaybe) ft

lookupLE :: Ord k => k -> Noodle k a -> Maybe (k, a)
lookupLE k = fmap fst . maxViewWithKey . fst . splitL k

lookupGT :: Ord k => k -> Noodle k a -> Maybe (k, a)
lookupGT k = fmap fst . minViewWithKey . snd . splitL k
