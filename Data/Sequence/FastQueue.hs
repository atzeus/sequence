{-# LANGUAGE GADTs, ViewPatterns, TypeOperators #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Sequence.FastQueue
-- Copyright   :  (c) Atze van der Ploeg 2014
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A sequence, a queue, with worst case constant time: '|>', and 'tviewl'.
--
-- Based on: "Simple and Efficient Purely Functional Queues and Deques", Chris Okasaki,
-- Journal of Functional Programming 1995
--
-----------------------------------------------------------------------------

module Data.Sequence.FastQueue(module Data.SequenceClass, FastQueue) where
import Control.Applicative (pure, (<$>), (<*>))
import Control.Applicative.Backwards
import Data.SequenceClass
import Data.Foldable
import Data.Traversable
import Prelude hiding (foldr,foldl)

data Nat = Z | S Nat

len :: [a] -> Nat
len [] = Z
len (_:xs) = S $ len xs

revAppend l r = rotate l r []
-- precondtion : |a| = |f| - (|r| - 1)
-- postcondition: |a| = |f| - |r|
rotate :: [a] -> [a]-> [a] -> [a]
rotate []  [y] r = y : r
rotate (x : f) (y : r) a = x : rotate f r (y : a)
rotate f        a     r  = error "Invariant |a| = |f| - (|r| - 1) broken"

data FastQueue a where
  RQ :: ![a] -> ![a] -> !Nat -> FastQueue a

queue :: [a] -> [a] -> Nat -> FastQueue a
queue f r Z = let f' = revAppend f r
                 in RQ f' [] $ len f'
queue f r (S n) = RQ f r n

instance Functor FastQueue where
  fmap phi (RQ a b n) = RQ (fmap phi a) (fmap phi b) n

instance Foldable FastQueue where
  foldl f = loop where
    loop i s = case viewl s of
          EmptyL -> i
          h :< t -> loop (f i h) t
  foldr f i s = foldr f i (reverse $ toRevList s)
    where toRevList s = case viewl s of
           EmptyL -> []
           h :< t -> h : toRevList t

instance Traversable FastQueue where
  traverse f (RQ a b n) =
    (\a b -> RQ a b n)
      <$> traverse f a
      <*> forwards (traverse (Backwards . f) b)

instance Sequence FastQueue where
 empty = RQ [] [] Z
 singleton x = queue [x] [] (S Z)
 (RQ f r a) |> x = queue f (x : r) a

 viewl (RQ [] [] Z) = EmptyL
 viewl (RQ (h : t) f a) = h :< queue t f a


