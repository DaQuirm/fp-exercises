{-# LANGUAGE NoImplicitPrelude #-}

module Semigroup where

import Prelude ((++))

class Semigroup a where
  (<>) :: a -> a -> a

instance Semigroup [a] where
  (<>) xs ys = xs  ++ ys

