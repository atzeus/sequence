-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Sequence.BSeq
-- Copyright   :  (c) Atze van der Ploeg 2014
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A catenable qeueue, implemented as a binary tree,
-- with good amortized performance when used ephemerally.
--
--
-----------------------------------------------------------------------------
module Data.Sequence.BSeq
  ( module Data.SequenceClass
  , BSeq
  ) where
import Data.Sequence.BSeq.Internal
import Data.SequenceClass
