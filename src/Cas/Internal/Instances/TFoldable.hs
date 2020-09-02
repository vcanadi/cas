{-# LANGUAGE TypeFamilies #-}
module Cas.Internal.Instances.TFoldable where


import           PreludeCustom
import           Cas.Internal.Def.Interface
import           Data.List (nub)
import           Data.MonoTraversable


-- pseudo Foldable functionality
tfoldl       :: (a -> T -> a) -> a  -> T -> a
tfoldl f a t = appEndo (getDual $ tfoldMap (Dual . Endo . flip f) t) a

-- depth limited version for foldl
tfoldlToDepth, tfoldlOnDepth :: Int -> (a -> T -> a) -> a  -> T -> a
tfoldlToDepth d f a t  = appEndo (getDual $ tfoldMapToDepth d (Dual . Endo . flip f) t ) a
tfoldlOnDepth d f a t  = appEndo (getDual $ tfoldMapOnDepth d (Dual . Endo . flip f) t ) a

-- ... foldr ...
tfoldr       :: (T -> a -> a) -> a  -> T -> a
tfoldr f a t = appEndo (tfoldMap (Endo . f) t) a

tfoldrToDepth, tfoldrOnDepth :: Int -> (T -> a -> a) -> a  -> T -> a
tfoldrToDepth d f a t  = appEndo (tfoldMapToDepth d (Endo . f) t ) a
tfoldrOnDepth d f a t  = appEndo (tfoldMapOnDepth d (Endo . f) t ) a

-- used for extracting usefull information from term
tToList, tToListRoot, tToListChilds :: T -> [T]
tToList = select $ const True
tToListRoot = selectRoot $ const True
tToListChilds = selectChilds $ const True
tToListToDepth,tToListOnDepth,tToListFromDepth :: Int -> T -> [T]
tToListToDepth d = selectToDepth d $ const True
tToListOnDepth d = selectOnDepth d $ const True
tToListFromDepth d = selectFromDepth d $ const True


select, selectRoot, selectChilds :: (T -> Bool) -> T -> [T]
select p = tfoldMap $ bool [] <$> return <*> p
selectRoot = selectOnDepth 0
selectChilds = selectOnDepth 1
selectAllChilds = selectOnDepth 1 (const True)

selectToDepth, selectOnDepth, selectFromDepth :: Int -> (T -> Bool) -> T -> [T]
selectToDepth d p = tfoldMapToDepth d $ bool [] <$> return <*> p
selectOnDepth d p = tfoldMapOnDepth d $ bool [] <$> return <*> p
selectFromDepth d p = tfoldMapFromDepth d $ bool [] <$> return <*> p

tLength, tLengthNub  :: (T -> Bool) -> T -> Int
tLength     = (length .)  .  select
tLengthNub  = ((length . nub) . )  .  select

tLengthToDepth, tLengthOnDepth, tLengthNubToDepth, tLengthNubOnDepth :: Int -> (T -> Bool) -> T -> Int
tLengthToDepth = ((length .) .) . selectToDepth
tLengthOnDepth = ((length .) .) . selectOnDepth
tLengthFromDepth = ((length .) .) . selectFromDepth
tLengthNubToDepth = (((length . nub) .) .) . selectToDepth
tLengthNubOnDepth = (((length . nub) .) .) . selectOnDepth
tLengthNubFromDepth = (((length . nub) .) .) . selectFromDepth

tLengthAllChilds :: T -> Int
tLengthAllChilds = length . selectAllChilds

tAny,tAll :: (T -> Bool) -> T -> Bool
tAnyToDepth, tAnyOnDepth, tAllToDepth, tAllOnDepth :: Int -> (T -> Bool) -> T -> Bool
tAny            = ((not . null) .)  .  select
tAnyToDepth     = (((not . null) .) .) . selectToDepth
tAnyOnDepth     = (((not . null) .) .) . selectOnDepth
tAnyFromDepth   = (((not . null) .) .) . selectFromDepth

tAll p            = not . tAny (not . p)
tAllToDepth d p   = not . tAnyToDepth d (not . p)
tAllOnDepth d p   = not . tAnyOnDepth d (not . p)
tAllFromDepth d p = not . tAnyFromDepth d (not . p)



tElem,tNotElem :: T -> T -> Bool
tElem = tAny . (==)
tNotElem = (not .) . tElem
tElemToDepth, tElemOnDepth, tElemFromDepth, tNotElemToDepth, tNotElemOnDepth, tNotElemFromDepth:: Int -> T -> T -> Bool
tElemToDepth d = tAnyToDepth d . (==)
tElemOnDepth d = tAnyOnDepth d . (==)
tElemFromDepth d = tAnyFromDepth d . (==)
tNotElemToDepth d = (not .) . tElemToDepth d
tNotElemOnDepth d = (not .) . tElemOnDepth d
tNotElemFromDepth d = (not .) . tElemFromDepth d


unique :: (T -> Bool) -> T -> Bool
unique     = (((==1) . length) .)  .  select

uniqueToDepth, uniqueOnDepth :: Int -> (T -> Bool) -> T -> Bool
uniqueToDepth = ((((==1) . length) .) .) . selectToDepth
uniqueOnDepth = ((((==1) . length) .) .) . selectOnDepth
uniqueFromDepth = ((((==1) . length) .) .) . selectFromDepth



-- low-level tfoldMap defs from which tfolfl is implemented
tfoldMap :: (Monoid a) => (T -> a) -> T -> a
tfoldMap = tfoldMapDepthCond (const True) (-1)

tfoldMapToDepth :: (Monoid a) => Int -> (T -> a) -> T -> a
tfoldMapToDepth = tfoldMapDepthCond (>=0)

tfoldMapOnDepth :: (Monoid a) => Int -> (T -> a) -> T -> a
tfoldMapOnDepth = tfoldMapDepthCond (==0)

tfoldMapFromDepth :: (Monoid a) => Int -> (T -> a) -> T -> a
tfoldMapFromDepth = tfoldMapDepthCond (<=0)


tfoldMapDepthCond :: (Monoid a) => (Int -> Bool) -> Int -> (T -> a) -> T -> a
tfoldMapDepthCond p d f =
    (<>) <$> (p d ?-> f)
         <*> (isAdd ?>>> mconcat . fmap (tfoldMapDepthCond p (d-1) f) . tAddTs
         ||>  isMul ?>>> mconcat . fmap (tfoldMapDepthCond p (d-1) f) . tMulTs
         ||>  isPow ?>>> ((<>) <$> tfoldMapDepthCond p (d-1) f . tPowB
                               <*> tfoldMapDepthCond p (d-1) f . tPowE )
         ||>  isLn  ?>>> mconcat . fmap (tfoldMapDepthCond p (d-1) f) . return .tLnT
         ||>             ifEnd )


