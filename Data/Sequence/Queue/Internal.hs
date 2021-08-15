{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types,GADTs, DataKinds, TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Sequence.Queue
-- Copyright   :  (c) Atze van der Ploeg 2014
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A sequence, a queue, with amortized constant time: '|>', and 'viewl'.
-- It also supports '|>' and 'viewr' in amortized logarithmic time.
--
-- A simplified version of Okasaki's implicit recursive
-- slowdown queues. 
-- See purely functional data structures by Chris Okasaki 
-- section 8.4: Queues based on implicit recursive slowdown
--
-----------------------------------------------------------------------------
module Data.Sequence.Queue.Internal
  ( Queue (..)
  , P (..)
  , B (..)
  )  where
import Control.Applicative (pure, (<*>), (<$>))
import Data.Foldable
import Data.Monoid (Monoid (..), (<>))
import Data.Traversable
import qualified Text.Read as TR
import Data.Function (on)
#if MIN_VERSION_base(4,9,0)
import qualified Data.Semigroup as Semigroup
import Data.Functor.Classes (Show1 (..))
#endif

import Prelude hiding (foldr,foldl)
import Data.SequenceClass

data P a = a :* a 
  deriving (Functor, Foldable, Traversable)

data B a where
  B1 :: a    -> B a
  B2 :: !(P a)  -> B a

deriving instance Functor B
deriving instance Foldable B
deriving instance Traversable B

-- | A queue.
data Queue a  where
  Q0 :: Queue a 
  Q1 :: a  -> Queue a
  QN :: !(B a) -> Queue (P a) -> !(B a) -> Queue a

deriving instance Functor Queue
-- The derived Foldable instance has an optimal null
-- with a good unfolding. No need to fuss around with it.
deriving instance Foldable Queue
deriving instance Traversable Queue

#if MIN_VERSION_base(4,9,0)
instance Semigroup.Semigroup (Queue a) where
  (<>) = (><)
#endif
instance Monoid (Queue a) where
  mempty = empty
#if MIN_VERSION_base(4,9,0)
  mappend = (Semigroup.<>)
#else
  mappend = (><)
#endif

instance Sequence Queue where
  empty = Q0
  singleton = Q1 
  q |> b = case q of
    Q0             -> Q1 b
    Q1 a           -> QN (B1 a) Q0 (B1 b)
    QN l m (B1 a)  -> QN l m (B2 (a :* b))
    QN l m (B2 r)  -> QN l (m |> r) (B1 b)

  a <| q = case q of
    Q0 -> Q1 a
    Q1 b -> QN (B1 a) Q0 (B1 b)
    QN (B1 b) m r -> QN (B2 (a :* b)) m r
    QN (B2 l) m r -> QN (B1 a) (l <| m) r

  (><) = foldl' (|>)

  viewl q0 = case q0 of
    Q0                    -> EmptyL
    Q1 a                  -> a :< Q0
    QN (B2 (a :* b)) m r  -> a :< QN (B1 b) m r
    QN (B1 a) m r         -> a :< shiftLeft m r
    where  
           shiftLeft q r = case viewl q of
               EmptyL -> buf2queue r
               l :< m -> QN (B2 l) m r

  viewr q0 = case q0 of
    Q0 -> EmptyR
    Q1 a -> Q0 :> a
    QN l m (B2 (a :* b)) -> QN l m (B1 a) :> b
    QN l m (B1 a) -> shiftRight l m :> a
    where
      shiftRight l q = case viewr q of
        EmptyR -> buf2queue l
        m :> r -> QN l m (B2 r)

buf2queue :: B a -> Queue a
buf2queue (B1 a)        = Q1 a
buf2queue (B2 (a :* b))  = QN (B1 a) Q0 (B1 b)
{-# INLINE buf2queue #-}

instance Show a => Show (Queue a) where
    showsPrec p xs = showParen (p > 10) $
        showString "fromList " . shows (toList xs)

#if MIN_VERSION_base(4,9,0)
instance Show1 Queue where
  liftShowsPrec _shwsPrc shwList p xs = showParen (p > 10) $
        showString "fromList " . shwList (toList xs)
#endif

instance Read a => Read (Queue a) where
    readPrec = TR.parens $ TR.prec 10 $ do
        TR.Ident "fromList" <- TR.lexP
        xs <- TR.readPrec
        return (fromList xs)

    readListPrec = TR.readListPrecDefault

instance Eq a => Eq (Queue a) where
  (==) = (==) `on` toList

instance Ord a => Ord (Queue a) where
  compare = compare `on` toList

