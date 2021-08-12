-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Sequence.ToCatQueue
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

module Data.Sequence.ToCatQueue
  ( module Data.SequenceClass
  , ToCatQueue
  ) where
import Data.Sequence.ToCatQueue.Internal
import Data.SequenceClass
