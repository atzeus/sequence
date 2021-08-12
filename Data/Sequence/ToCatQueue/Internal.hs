{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
#endif


-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Sequence.ToCatQueue.Internal
-- Copyright   :  (c) Atze van der Ploeg 2013
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A purely functional catenable queue representation with
-- that turns takes a purely functional queue and turns in it into
-- a catenable queue, i.e. with the same complexity for '><' as for '|>'
-- Based on Purely functional data structures by Chris Okasaki 
-- section 7.2: Catenable lists
--
-----------------------------------------------------------------------------

module Data.Sequence.ToCatQueue.Internal
  ( ToCatQueue (..)
  ) where
import Control.Applicative hiding (empty)
import Data.Foldable
import Data.Traversable
import Data.Monoid (Monoid (..))
import qualified Text.Read as TR
#if MIN_VERSION_base(4,9,0)
import qualified Data.Semigroup as Semigroup
import Data.Functor.Classes (Show1 (..))
#endif
import Data.Function (on)
import Prelude hiding (foldr,foldl)
import Data.SequenceClass

-- | The catenable queue type. The first type argument is the 
-- type of the queue we use (|>)
data ToCatQueue q a where
  -- Invariant: no element of the queue of queues may
  -- be empty.
  C0 :: ToCatQueue q a
  CN :: a -> !(q (ToCatQueue q a)) -> ToCatQueue q a

deriving instance Functor q => Functor (ToCatQueue q)
deriving instance Foldable q => Foldable (ToCatQueue q)
deriving instance Traversable q => Traversable (ToCatQueue q)

instance (Show a, Foldable q) => Show (ToCatQueue q a) where
    showsPrec p xs = showParen (p > 10) $
        showString "fromList " . shows (toList xs)

#if MIN_VERSION_base(4,9,0)
instance Foldable q => Show1 (ToCatQueue q) where
  liftShowsPrec _shwsPrc shwList p xs = showParen (p > 10) $
        showString "fromList " . shwList (toList xs)
#endif

instance (Sequence q, Read a) => Read (ToCatQueue q a) where
    readPrec = TR.parens $ TR.prec 10 $ do
        TR.Ident "fromList" <- TR.lexP
        xs <- TR.readPrec
        return (fromList xs)

    readListPrec = TR.readListPrecDefault

instance (Foldable q, Eq a) => Eq (ToCatQueue q a) where
  (==) = (==) `on` toList

instance (Foldable q, Ord a) => Ord (ToCatQueue q a) where
  compare = compare `on` toList

instance Sequence q => Sequence (ToCatQueue q) where
 empty       = C0
 singleton a = CN a empty
 C0        >< ys  = ys
 xs        >< C0  = xs
 (CN x q)  >< ys  = CN x (q |> ys)

 viewl C0        = EmptyL
 viewl (CN x q0)  = x :< case viewl q0 of
   EmptyL -> C0
   t :< q'  -> linkAll t q'
   where
   linkAll :: ToCatQueue q a -> q (ToCatQueue q a) -> ToCatQueue q a
   linkAll t@(CN x q) q' = case viewl q' of
     EmptyL -> t
     h :< t' -> CN x (q |> linkAll h t')
   linkAll C0 _ = error "Invariant failure"

 viewr = foldl' go EmptyR
   where
     go EmptyR y = empty :> y
     go (xs :> x) y = xs' :> y
       where
         !xs' = xs |> x

#if MIN_VERSION_base(4,9,0)
instance Sequence q => Semigroup.Semigroup (ToCatQueue q a) where
  (<>) = (><)
#endif
instance Sequence q => Monoid (ToCatQueue q a) where
  mempty = empty
#if MIN_VERSION_base(4,9,0)
  mappend = (Semigroup.<>)
#else
  mappend = (><)
#endif
