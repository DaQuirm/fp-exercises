{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Recursion where

newtype Fix f = Fix { unfix :: f (Fix f) }

-- https://hackage.haskell.org/package/data-fix-0.2.0/docs/Data-Fix.html
instance Show (f (Fix f)) => Show (Fix f) where
  showsPrec n x = showParen (n > 10) $ \s ->
    "Fix " ++ showsPrec 11 (unfix x) s

data TreeF a f = Node a [f] deriving (Show, Functor) -- a is "content", f is "recursion point"

type Tree a = Fix (TreeF a)

node :: a -> [Tree a] -> Tree a -- "smart" constructor
node x xs = Fix $ Node x xs

et :: Tree Int
et = node 0 [node 1 [], node 7 []]

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg f = alg $ (cata alg) <$> unfix f

type Coalgebra f a = a -> f a

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coalg x = Fix $ (ana coalg) <$> coalg x

nested :: Int -> Tree Int
nested = ana coalg
  where
    coalg 0 = Node 0 []
    coalg n = Node n (replicate n (n - 1))

data Attr f a = Attr
  { attribute :: a
  , hole      :: f (Attr f a)
  }

type CVAlgebra f a = f (Attr f a) -> a

histo :: Functor f => CVAlgebra f a -> Fix f -> a
histo cvalg (Fix f) = cvalg $ worker <$> f
  where
    worker f' = Attr (histo cvalg f') (worker <$> unfix f')

sumAlg :: Algebra (TreeF Int) Int
sumAlg (Node x xs) = x + sum xs

-- sumCVAlg :: CVAlgebra (TreeF Int) [Int]
-- sumCVAlg (Node x as) = x : (_h <$> as)

