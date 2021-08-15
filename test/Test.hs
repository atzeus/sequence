{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

import Test.Tasty
import Test.Tasty.QuickCheck as QC hiding ((><))
import Data.SequenceClass
import Valid
import Data.Proxy
import Data.Foldable

import FastQueue ()
import Data.Sequence.FastQueue
import BSeq ()
import Data.Sequence.BSeq
import Queue ()
import Data.Sequence.Queue
import ToCatQueue ()
import Data.Sequence.ToCatQueue

type Usable s = (Arbitrary (s Int), Show (s Int), Eq (s Int), Valid s, Sequence s)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testsFor "FastQueue" (Proxy :: Proxy FastQueue)
  , testsFor "BSeq" (Proxy :: Proxy BSeq)
  , testsFor "Queue" (Proxy :: Proxy Queue)
  , testsFor "ToCatQueue FastQueue" (Proxy :: Proxy (ToCatQueue FastQueue))
  , testsFor "ToCatQueue Queue" (Proxy :: Proxy (ToCatQueue Queue))
  ]

testsFor :: Usable s => String -> Proxy s -> TestTree
testsFor s p = testGroup (s ++ " Tests") [properties p]

properties :: Usable s => Proxy s -> TestTree
properties p = testGroup "Properties" [qcProps p]

qcProps :: Usable s => Proxy s -> TestTree
qcProps (_ :: Proxy s) = testGroup "(checked by QuickCheck)" $
  [ QC.testProperty "generator valid" $
     -- We avoid trying to show an invalid generated sequence
     -- because doing so may raise an exception.
     forAllBlind (arbitrary :: Gen (s Int)) valid
  , QC.testProperty "viewl works" $
      \(s :: s Int) -> case viewl s of
        EmptyL -> null s .&&. s === empty
        a :< as -> valid as .&&. toList s === a : toList as .&&. a <| as === s
  , QC.testProperty "viewr works" $
      \(s :: s Int) -> case viewr s of
        EmptyR -> null s .&&. s === empty
        as :> a -> valid as .&&. toList s === toList as ++ [a] .&&.
                      as |> a === s
  , QC.testProperty ">< works" $
      \(s :: s Int) t -> let st = s >< t in
        valid st .&&. toList st === toList s ++ toList t
  , QC.testProperty ">< is associative" $
      \(s :: s Int) t u -> (s >< t) >< u === s >< (t >< u)
  , QC.testProperty "empty is identity" $
      \(s :: s Int) -> s >< empty === s .&&. empty >< s === s
  , QC.testProperty "foldMap/singleton" $
      \(x :: Int) ->
        let s = singleton x :: s Int
        in valid s .&&. foldMap (:[]) s === [x]
  , QC.testProperty "fromList works" $
      \(l :: [Int]) -> let s = fromList l :: s Int in
        valid s .&&. toList s === l .&&. s === foldl' (|>) empty l
  ]
