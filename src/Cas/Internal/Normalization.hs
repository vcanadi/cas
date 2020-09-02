module Cas.Internal.Normalization where

import           PreludeCustom
import           Cas.Misc
import           Cas.Internal.Utils
import qualified Data.List as L(sort)
import           Data.List hiding (sort)
import           Data.Bool.HT (implies)

flattenNode, flatten, sortNode, sort, collectNode, collect, normalizeStep, normalize :: T -> T
flattenNode = isAdd ?>>> add . concatMap addElems . tAddTs
          ||> isMul ?>>> mul . concatMap mulElems . tMulTs
                     ||> id

collectNode TAdd {..}   = collectAddNode tAdd
collectNode TMul {..}   = collectMulNode tMul
collectNode t@TPow {..} = collectMulNode (Mul [t])
collectNode t            = t

collectAddNode :: Add -> T
collectAddNode =
     addCollapse
   . fmap (uncurry mulBinCollapse)
   . fmap (bimap TF id)
   . fmap (foldlFst addFBin zeroF)
   . stableGroupOn snd
   . fmap mulParts
   . addTs

collectMulNode :: Mul -> T
collectMulNode =
     mulCollapse
   . fmap (uncurry $ flip powCollapse)
   . fmap (foldlFst addBinCollapse zero)
   . stableGroupOn snd
   . fmap powParts
   . mulTs

sortNode = isAdd ?>>> add . L.sort . tAddTs
       ||> isMul ?>>> mul . L.sort . tMulTs
                  ||> id



flatten = tmap flattenNode
collect = tmap collectNode
sort    = tmap sortNode

normalizeStep = collect . flatten

newtype Norm = Norm (Int,Int,Int) deriving (Eq,Ord,Show)
instance Num Norm where
    (Norm (x,y,z)) + (Norm (w,v,q)) = Norm (x+w,y+v,z+q)
    fromInteger i = Norm (fromIntegral i,fromIntegral i,fromIntegral i)
norm :: T -> Norm
norm t = Norm ( lengthNonFLeafs t
              , lengthFs t
              , lengthNonFlats t)

infix 4 |>.|, |>|
(|>.|),(|>|) :: T -> T -> Bool
decNorm, nonDecNorm, isFixT, isNormalFor, isCumulativeFor :: (T -> T) -> T -> Bool
isQuasiDecFor :: (T -> T) -> T -> T -> Bool

t |>| r = norm t > norm r
t |>.| r = sort t == sort r || t |>| r
decNorm f t = t |>| f t
nonDecNorm f = not . decNorm f
isFixT f t = sort t == sort (f t)
f `isNormalFor` t = isFixT f t || decNorm f t
f `isCumulativeFor` t =
       isLeaf t
    || sum (fmap norm $ selectAllChilds t) >= norm (f t)
isQuasiDecFor f t r =
                eqType t r
            &&  (not . isLeaf) t
            &&  tLengthAllChilds t == tLengthAllChilds r
            &&  (((and .) . zipWith (|>.|)) `on` selectAllChilds)  t r
      `implies` t |>.| f r


normalize = sort . normalize'
    where
    normalize' t
        | isFixT normalizeStep t     = t
        | nonDecNorm normalizeStep t = error $ "non-monotone norm"
        | otherwise                  = normalize' (normalizeStep t)



fixTIndex, nonDecNormIndex :: (T -> T) -> T -> Int
fixTIndex f = fromMaybe (error "no fixT")
            . findIndex (isFixT f) . iterate f

nonDecNormIndex f = fromMaybe (error "no nonDecNorm")
                  . findIndex (nonDecNorm f) . iterate f

converges :: (T -> T) -> T -> Bool
converges f = (==) <$> fixTIndex f <*> nonDecNormIndex f


normalizeIter :: Int -> T -> T
normalizeIter n = endoIter n normalizeStep

endoIter :: Int -> (a -> a) -> (a -> a)
endoIter n f = foldr (.) id (replicate n f)

normOfIter ::  Int -> T -> Norm
normOfIter = (norm .) . normalizeIter

-- expand
expandNode, expand :: T -> T
expandNode TMul {..} = expandMulNode tMul
    where

    expandMulNode :: Mul -> T
    expandMulNode = addCollapse
                . foldr1 (\acc t2s -> [mulBinCollapse x y | x <- acc, y <- t2s])
                . (\case [] -> error "empty mul in expandNode"; ts -> ts  )
                . subTsMul
expandNode t = t

expand = tmap (normalizeStep . expandNode)

