{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Monoid where

import Prelude (Show, Eq, Ord, min, max, ($), Bool(..), foldl, (&&), (||), (.), Num(..), Foldable, id, (<$>), Functor(..), (<$>), const, Int, zip, (==), otherwise, fst, length, Float, String)
import Data.Bifunctor (bimap)
import Data.List.NonEmpty (NonEmpty(..), fromList)
import qualified Data.List.NonEmpty as NonEmpty

import Semigroup (Semigroup(..))

class Semigroup a => Monoid a where
  mempty :: a

sconcat :: Semigroup a => NonEmpty a -> a
sconcat (x :| xs) = foldl (<>) x xs

instance Monoid [a] where
  mempty = []

newtype Dual a = Dual { getDual :: a } deriving (Show)

instance Semigroup a => Semigroup (Dual a) where
  (Dual x) <> (Dual y) = Dual $ y <> x

instance Monoid a => Monoid (Dual a) where
  mempty = Dual mempty

-- Ex. Try out Dual, e.g. Dual [1] <> Dual [2] <> Dual [3]

newtype Sum a = Sum { getSum :: a } deriving (Show)

instance (Num a) => Semigroup (Sum a) where
  Sum x <> Sum y = Sum $ x + y

instance (Num a) => Monoid (Sum a) where
  mempty = Sum 0

newtype Product a = Product { getProduct :: a } deriving (Show)

instance (Num a) => Semigroup (Product a) where
  Product x <> Product y = Product $ x * y

instance (Num a) => Monoid (Product a) where
  mempty = Product 1

foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f xs = foldl (\acc x -> acc <> f x) mempty xs

-- Ex. sum via foldMap
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

-- Ex. length via foldMap
length' :: Foldable t => t a -> Int
length' = getSum . foldMap (Sum . const 1)

type Predicate a = a -> Bool

-- Ex. any and all via All/Any and foldMap
all :: Predicate a -> [a] -> Bool
all p = foldl (\acc x -> acc && p x) True

any :: Predicate a -> [a] -> Bool
any p = foldl (\acc x -> acc || p x) False

newtype All = All { getAll :: Bool }

instance Semigroup All where
  All x <> All y = All $ x && y

instance Monoid All where
  mempty = All True

newtype Any = Any { getAny :: Bool }

instance Semigroup Any where
  Any x <> Any y = Any $ x || y

instance Monoid Any where
  mempty = Any False

-- Ex. any and all via All/Any and foldMap
all' :: Predicate a -> [a] -> Bool
all' p = getAll . foldMap (All . p)

any' :: Predicate a -> [a] -> Bool
any' p = getAny . foldMap (Any . p)

newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
  Endo f <> Endo g = Endo $ f . g

instance Monoid (Endo a) where
  mempty = Endo id

-- Ex. Endo composition
-- let f = getEndo $ fold $ Endo <$> [(+3), (*2), \x -> x * x]

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

-- Ex. reverse Endo composition via Dual
-- let f = getEndo $ fold $ Dual . Endo <$> [(+3), (*2), \x -> x * x]

data Maybe a = Nothing | Just a deriving (Show, Functor)

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing  = x
fromMaybe _ (Just x) = x

instance Semigroup m => Semigroup (Maybe m) where
  x <> Nothing = x
  Nothing <> x = x
  Just x <> Just y = Just $ x <> y

instance Monoid m => Monoid (Maybe m) where
  mempty = Nothing

data Gift = Gift { description :: String, price :: Maybe Float }

gifts :: [Gift]
gifts =
  [ Gift { description = "Wine",          price = Just 12.50 }
  , Gift { description = "Origami crane", price = Nothing }
  , Gift { description = "Opera tickets", price = Just 75.0 }
  , Gift { description = "Icicle",        price = Nothing }
  , Gift { description = "Ugly sweater",  price = Just 49.99 }
  ]

-- Ex.  get total cost of gifts via Sum and Maybe
-- fold $ (Sum <$>) . price <$> gifts

data Func a b = Func { getFunc :: (a -> b) }

instance Semigroup b => Semigroup (Func a b) where
  Func f <> Func g = Func $ \x -> (f x) <> (g x)

instance Monoid b => Monoid (Func a b) where
  mempty = Func $ const mempty

newtype First a = First { getFirst :: Maybe a }

instance Semigroup (First a) where
  First Nothing <> x = x
  x             <> _ = x

instance Monoid (First a) where
  mempty = First Nothing

-- Ex. findIndex via First
findIndex :: Eq a => a -> [a] -> Maybe Int
findIndex x xs = getFirst $ foldMap (First . f) (xs `zip` [0..])
  where
    f (e, i) | e == x    = Just i
             | otherwise = Nothing

newtype Max a = Max { getMax :: a } deriving Show

instance Ord a => Semigroup (Max a) where
  Max x <> Max y = Max $ max x y

-- Ex. max via Max
max' :: Ord a => NonEmpty a -> a
max' = getMax . sconcat . (Max <$>)

newtype Min a = Min { getMin :: a } deriving Show

instance Ord a => Semigroup (Min a) where
  Min x <> Min y = Min $ min x y

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (x, y) <> (x', y') = (x <> x', y <> y')

-- Ex. minmax via sconcat
minmax :: Ord a => NonEmpty a -> (a, a)
minmax xs = bimap getMin getMax $ sconcat $ (Min <$> xs) `NonEmpty.zip` (Max <$> xs)

head :: [a] -> Maybe a
head []     = Nothing
head (x:xs) = Just x

type Predicate' a = Func a Bool

instance Functor (Func b) where
  fmap f (Func g) = Func $ f . g

startsWith :: Eq a => a -> Predicate' [a]
startsWith x = Func $ \xs -> fromMaybe False $ (x ==) <$> head xs

hasLength :: Int -> Predicate' [a]
hasLength n = Func $ \xs -> length xs == n

allP :: Foldable t => t (Predicate' a) -> Predicate' a
allP = (getAll <$>) . foldMap (All <$>)

anyP :: Foldable t => t (Predicate' a) -> Predicate' a
anyP = (getAny <$>) . foldMap (Any <$>)

