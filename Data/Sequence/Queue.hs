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
--
-- A simplified version of Okasaki's implicit recursive
-- slowdown queues. 
-- See purely functional data structures by Chris Okasaki 
-- section 8.4: Queues based on implicit recursive slowdown
--
-----------------------------------------------------------------------------
module Data.Sequence.Queue(module Data.SequenceClass,Queue)  where
import Data.Sequence.Queue.Internal
import Data.SequenceClass
