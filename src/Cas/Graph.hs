{-# LANGUAGE StandaloneDeriving #-}
module Cas.Graph
    ( maxCompleteSubList )
    where

import           Data.Foldable
import           Data.Char
import           PreludeCustom
import           Debug.Trace

-- it is assumed that edge function (a -> a -> Bool) is
-- and symmetric (it concludes p x y && p y x from p x y)
type E a = a -> a -> Bool
data G a = G {
      gE  :: E a
    , gVs :: [a]
    }

gEs :: G a -> [(a,a)]
gEs G {..} = [(x,y) | x<-gVs, y<-gVs, gE x y]

showEs :: (Show a) => G a -> String
showEs g = show $ gEs g

instance (Show a) => Show (G a) where
    show g = concat (zipWith (\i x -> i:":" <> show x <> "\n"  ) ['A'..] (gVs g))
          <> take (gLength g) ['A'..] <> "\n"
          <> showEs g

gEToAll  = (\x G {..} -> all (gE x) gVs        ):: a -> G a -> Bool
gEToAny  = (\x G {..} -> any (gE x) gVs        ):: a -> G a -> Bool
gEToNone = (\x        -> not . gEToAny x       ):: a -> G a -> Bool
(|:)     = (\x G {..} -> G gE (x:gVs)          ):: a -> G a -> G a;   infixr 5 |:
gEmpty   = (\gE       -> G gE []               ):: E a -> G a
fromVs   = (\gE       -> G gE                  ):: E a -> [a] -> G a
isEmpty  = (null . gVs                         ):: G a -> Bool
isSingle = ((==1) . length  . gVs              ):: G a -> Bool
gFilter  = (\p G {..} -> G gE (filter p gVs)   ):: (a -> Bool) -> G a -> G a
gHead    = (head . gVs                         ):: G a -> a
gIndex   = (\i        -> (!!i) . gVs           ):: Int -> G a -> a
gTail    = (\G {..}   -> G gE (tail gVs)       ):: G a -> G a
gDrop    = (\i G {..} -> G gE (drop i gVs)     ):: Int -> G a -> G a
gLength  = (length . gVs                       ):: G a -> Int



neighborhood :: a -> G a -> [a]
neighborhood x G {..} = filter (gE x) gVs

-- Graph is complete if each vertex is connected to all other vertices (EVEN TO ITSELF)
isComplete :: G a -> Bool
isComplete G {..} = all (flip all gVs . gE) gVs

-- Completeness closure of x in g
complete :: G a -> G a
complete g | isEmpty g || isSingle g = g
           | gEToAll u rec           = gHead rec |: u |: gTail rec
           | otherwise               = rec
    where
    v   = gHead g
    u   = gIndex 1 g
    rec = complete (v |: gDrop 2 g)

completeComplement ::  G a -> G a
completeComplement g = gFilter (not . flip gEToAll (complete g)) g

maxCompleteSubG :: G a -> G a
maxCompleteSubG g | isEmpty g = g
                  -- | gLength g == gLength (completeComplement g) = error "cmp loop"
                  | otherwise =  maximumBy (compare `on` gLength)
                              [ complete g
                              , maxCompleteSubG $ completeComplement g
                              ]

-- Safer version. Relation doesn't asume reflexive clusure
-- Completeness closure of x in g
-- complete :: (Eq a) => a -> G a -> G a
-- complete x g | isEmpty g || isSingle g = g
--              | gEToAll u rec           = gHead rec |: u |: gTail rec
--              | otherwise               = rec
--     where
--     v   = gHead g
--     u   = gIndex 1 g
--     rec = complete x (gTail g)


-- symmetric and reflexive closure of gE
gEClosed :: (Eq a) => E a -> E a
gEClosed gE  = \x y -> gE x y || gE y x || x == y

-- Use without wrapping in G.
-- This closes relation p with reflexive and symmetric closure on relation using Eq constraing.
maxCompleteSubList :: (Eq a) => (a -> a -> Bool) -> [a] -> [a]
maxCompleteSubList p xs = gVs $ maxCompleteSubG $ G (gEClosed p) xs
