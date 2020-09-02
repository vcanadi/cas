{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Cas.Misc where

import           PreludeCustom
import           Data.Function
import           Data.List hiding (intersect)
import           Debug.Trace
import           Data.Foldable
import           Data.Ratio
import           Data.Complex.Generic

foldlFst :: (a -> b -> a) -> a -> [(b,c)] -> (a,c)
foldlFst f a =  bimap (foldl f a) head . unzip

debug :: (Show a) => String -> a -> a
debug s = (\x -> trace (s <> " " <> show x ) x )


showQ = (==1) . denominator ?>>> show . numerator
                             ||> ((<>) <$> (<>"/") . show . numerator
                                       <*> show . denominator)


showImg y = bool "" (showQ y) (y/=1) <> "i"

showF (x':+y') = case (x',y') of
               (0,0) -> "0"
               (x,0) -> showQ x
               (0,y) -> showImg y
               (x,y) -> "(" <> showQ x <> "+" <> showImg y <> ")"



-- data QWrapperForShow = QWrapperForShow { unwrapQ :: Rational }

-- instance Show QWrapperForShow where
--     show (QWrapperForShow q) = showQ q

-- showF :: Complex Rational -> String
-- showF (x:+y)= show $ (QWrapperForShow x) :+ (QWrapperForShow y)
--

wrap :: String -> String
wrap s = "(" <> s <> ")"


wrapshow :: (Show a) => a -> String
wrapshow = wrap . show

stableGroupOn :: (Ord b) => (a -> b) -> [a] -> [[a]]
stableGroupOn f = fmap fromJust
                . (fmap . flip lookup
                    <$> fmap (f . head &&& id)
                      . groupBy ((==) `on` f)
                      . sortBy  (comparing f)
                    <*> nub
                      . fmap f )

stableGroup :: (Ord a) => [a] -> [[a]]
stableGroup = stableGroupOn id
