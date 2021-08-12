-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Sequence.FastQueue
-- Copyright   :  (c) Atze van der Ploeg 2014
--                (c) David Feuer 2021
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A queue (actually an output-restricted deque), with worst case constant time:
-- '|>', '<|', and 'viewl'. It has worst case linear time 'viewr' and '><'.
--
-- Based on: "Simple and Efficient Purely Functional Queues and Deques", Chris Okasaki,
-- Journal of Functional Programming 1995
--
-----------------------------------------------------------------------------

module Data.Sequence.FastQueue 
  ( module Data.SequenceClass
  , FastQueue
  ) where
import Data.SequenceClass
import Data.Sequence.FastQueue.Internal
