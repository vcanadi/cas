{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Cas.Internal.Def where

import           PreludeCustom
import           Data.Complex.Generic

type F = Complex Rational

instance Ord F where
   (x0:+y0) <= (x1:+y1) = x0 < x1 || x0==x1 && y0 < y1

data C      = Pi  | E               deriving (Eq, Ord, Show)
newtype X   = X    { xI    :: Int } deriving (Eq,Ord)
newtype Add = Add  { addTs :: [T] } deriving (Eq,Ord)
newtype Mul = Mul  { mulTs :: [T] } deriving (Eq,Ord)

data T   = TF   { tF    :: F    }
         | TX   { tX    :: X    }
         | TC   { tC    :: C    }
         | TAdd { tAdd  :: Add  }
         | TMul { tMul  :: Mul  }
         | TPow { tPowB :: T
                , tPowE :: T    }
         | TLn  { tLnT  :: T    }
         deriving (Eq,Ord)

data Equ    = Equ    { equT       :: T     }  deriving (Eq,Ord)
data LinSys = LinSys { linSysEqus :: [Equ] }  deriving (Eq,Ord)

data Rule   = Rule {ruleX::X, ruleT::T}
(-->) :: X -> T -> Rule
(-->) = Rule


data LinSysSolution = LinSysSolution { solutionRules :: [Rule]
                                     , constraints   :: [Equ]  }



