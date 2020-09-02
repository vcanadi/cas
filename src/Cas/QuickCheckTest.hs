module Cas.QuickCheckTest where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Control.Monad
import Data.Ratio
import PreludeCustom
import Cas.Interface
import Data.Complex.Generic

instance Arbitrary C where
    arbitrary = oneof [return Pi, return E]

instance Arbitrary T where
    arbitrary = sized genT

chooseQ :: Gen Rational
chooseQ = liftM2 (%) (choose (-1,1))  (choose (1,2))


maxTestCnt = 20000
maxDepth =13
scaleFactor = 2 -- maxTestCnt `div` 2^maxDepth + 1

genT, scaledGenT :: Int -> Gen T
genT n = scaledGenT $ n `div` scaleFactor

scaledGenT 0 = oneof [
     liftM f $ liftM2 (:+) chooseQ chooseQ
   , liftM x $ choose (0,1)
   , liftM TC arbitrary
   ]
scaledGenT n = do
    k <- choose (2,5)
    oneof [
          liftM add $ vectorOf k rec
        , liftM mul $ vectorOf k rec
        , liftM2 pow rec rec
        , liftM ln rec
        ]
   where
   rec =  scaledGenT =<< choose (0, n `div` 2)

