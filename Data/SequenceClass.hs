{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances, GADTs,TypeSynonymInstances,FlexibleInstances,Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints #-}
#endif



-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SequenceClass
-- Copyright   :  (c) Atze van der Ploeg 2014
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
-- A type class for sequences.
--
-- See the package type-aligned for a generalization of this type class sequences.
-- 
-----------------------------------------------------------------------------
module Data.SequenceClass(Sequence(..), ViewL(..), ViewR(..)) where

import Data.Monoid
import Data.Foldable (foldl')
import qualified Data.Traversable as T
import qualified Data.Sequence as S

infixr 5 <|
infixl 5 |>
infix 5 ><
infixl 9 :<
infixr 9 :>
{- | A type class for (finite) sequences
 

Instances should be /free monoids/
(<http://comonad.com/reader/2015/free-monoids-in-haskell/ ignoring issues with infinite and partially defined structures>),
just like lists, with @singleton@ as the canonical injection and @foldMap@
factoring functions.  In particular, they should satisfy the following laws:

@Semigroup@ and @Monoid@ laws:

> (><) == (Data.Semigroup.<>)
> empty == mempty

In particular, this requires that

> empty >< x == x
> x >< empty == x
> (x >< y) >< z = x >< (y >< z)

@FoldMap@/@singleton@ laws:

For any 'Monoid' @m@ and any function @f :: c -> m@,

1. @'foldMap' f@ is a monoid morphism:

    * @'foldMap' f 'mempty' = 'mempty'@
    * @'foldMap' f (m '<>' n) = 'foldMap' f m <> 'foldMap' f n@

2. 'foldMap' undoes 'singleton':

    @'foldMap' f . 'singleton' = f@

Observation laws:

> viewl (singleton e >< s) == e :< s
> viewl empty == EmptyL

The behaviour of '<|','|>', and 'viewr' is implied by the above laws and their
default definitions.

Warning: the default definitions are typically awful. Check them carefully
before relying on them. In particular, they may well work in @O(n^2)@ time (or
worse?) when even definitions that convert to and from lists would work in
@O(n)@ time. Exceptions: for sequences with constant time concatenation, the
defaults for '<|' and '|>' are okay. For sequences with constant time '|>',
the default for 'fromList' is okay.
-}
#if __GLASGOW_HASKELL__ >= 806
class (T.Traversable s, forall c. Monoid (s c)) => Sequence s where
#else
class T.Traversable s => Sequence s where
#endif

  {-# MINIMAL
    empty,
    singleton,
    (viewl | viewr),
    ((><) | (|>) | (<|))
    #-}

  empty     :: s c 
  singleton :: c  -> s c 
  -- | Append two sequences
  (><)       :: s c  -> s c   -> s c 
  -- | View a sequence from the left
  viewl     :: s c  -> ViewL s c 
  -- | View a sequence from the right
  --          
  -- Default definition:
  --
  -- > viewr q = case viewl q of 
  -- >    EmptyL -> EmptyR
  -- >    h :< t -> case viewr t of
  -- >        EmptyR -> empty   :> h
  -- >        p :> l   -> (h <| p) :> l
  --
  viewr     :: s c -> ViewR s c 
  -- | Append a single element to the right
  -- 
  -- Default definition:
  --
  -- > l |> r = l >< singleton r
  -- 
  (|>)       :: s c -> c  -> s c 
  -- | Append a single element to the left
  -- 
  -- Default definition:
  --
  -- > l <| r = singleton l >< r
  --
  (<|)       :: c  -> s c -> s c
  
  -- | Convert a list to a sequence
  --
  -- Default definition:
  --
  -- > fromList = foldl' (|>) empty
  fromList :: [c] -> s c

  l |> r = l >< singleton r
  l <| r = singleton l >< r
  l >< r = case viewl l of
    EmptyL -> r
    h :< t  -> h <| (t >< r)

  viewl q = case viewr q of 
    EmptyR -> EmptyL
    p :> l -> case viewl p of
        EmptyL -> l :< empty
        h :< t   -> h :< (t |> l)

  viewr q = case viewl q of 
    EmptyL -> EmptyR
    h :< t -> case viewr t of
        EmptyR -> empty   :> h
        p :> l   -> (h <| p) :> l

  fromList = foldl' (|>) empty

-- | A view of the left end of a 'Sequence'.
data ViewL s c where
   EmptyL  :: ViewL s c 
   (:<)    :: c -> s c -> ViewL s c

deriving instance (Show c, Show (s c)) => Show (ViewL s c)

-- | A view of the right end of a 'Sequence'.
data ViewR s c where
   EmptyR  :: ViewR s c 
   (:>)    :: s c -> c -> ViewR s c

deriving instance (Show c, Show (s c)) => Show (ViewR s c)

 
instance Sequence S.Seq where
 empty = S.empty
 singleton = S.singleton
 (<|) = (S.<|)
 (|>) = (S.|>)
 (><) = (S.><)
 viewl s = case S.viewl s of
   S.EmptyL -> EmptyL
   h S.:< t -> h :< t
 viewr s = case S.viewr s of
   S.EmptyR -> EmptyR
   t S.:> h -> t :> h
 fromList = S.fromList

instance Sequence [] where
  empty = []
  singleton x = [x]
  (<|) = (:)
  xs |> x = xs ++ [x]
  (><) = (++)
  viewl [] = EmptyL
  viewl (h : t) = h :< t 

  -- This definition is entirely strict. I'm not sure whether there's
  -- a real benefit to making it lazy or not.
  -- NOTE: if we *do* make it lazy, then the definition of viewr
  -- for FastQueue will have to be adjusted to keep its bounds
  -- worst case.
  viewr [] = EmptyR
  viewr (x : xs) = case go x xs of (start, end) -> start :> end
    where
      go y [] = ([], y)
      go y (z : zs) = case go z zs of (start, end) -> (y : start, end)
  fromList = id
