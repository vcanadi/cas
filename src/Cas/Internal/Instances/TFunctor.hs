{-# LANGUAGE TypeFamilies #-}
module Cas.Internal.Instances.TFunctor where


import           PreludeCustom
import           Cas.Internal.Def.Interface
import           Data.MonoTraversable

tmap :: (T -> T) -> T -> T
tmap = tmapDepthCond (const True) (-1)

tmapToDepth, tmapOnDepth, tmapFromDepth :: Int -> (T -> T) -> T -> T
[tmapToDepth,tmapOnDepth, tmapFromDepth] =
    fmap tmapDepthCond [(>=0), (==0), (<=0)]

tmapDepthCond :: (Int -> Bool) -> Int -> (T -> T) -> T -> T
tmapDepthCond p d f =
    bool id f (p d)
  . (   isAdd ?>>> add . fmap rec . tAddTs
    ||> isMul ?>>> mul . fmap rec . tMulTs
    ||> isPow ?>>> (pow <$> rec . tPowB
                        <*> rec . tPowE )
    ||> isLn  ?>>> ln . rec . tLnT
    ||>            id
    )
    where
    rec = tmapDepthCond p (pred d) f


-- tmapReverseDepthCond :: (Int -> Bool) -> Int -> (T -> T) -> T -> T
-- tmapReverseDepthCond p d f =
--                 bool id f (p d)
--                 >>>
--                        isAdd ?>>> add . fmap (tmapDepthCond p (d-1) f) . tAddTs
--                    ||> isMul ?>>> mul . fmap (tmapDepthCond p (d-1) f) . tMulTs
--                    ||> isPow ?>>> (pow <$> tmapDepthCond p (d-1) f . tPowB
--                                        <*> tmapDepthCond p (d-1) f . tPowE )
--                    ||> isLn  ?>>> ln  . tmapDepthCond p (d-1) f . tLnT
--                    ||>            id



tmapRoot :: (T -> T) -> T -> T
tmapRoot = tmapOnDepth 0

tmapChilds :: (T -> T) -> T -> T
tmapChilds = tmapOnDepth 1

