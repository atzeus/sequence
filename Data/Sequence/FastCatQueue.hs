

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Sequence.FastCatQueue
-- Copyright   :  (c) Atze van der Ploeg 2014
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A sequence, a catenable queue, with worst case constant time: '><', '|>', '<|' and 'viewl'.
--
-----------------------------------------------------------------------------
module Data.Sequence.FastCatQueue(module Data.SequenceClass, FastTCQueue) where

import Data.SequenceClass
import Data.Sequence.FastQueue
import Data.Sequence.ToCatQueue

-- | A catenable queue.
type FastTCQueue =  ToCatQueue FastQueue
