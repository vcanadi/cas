module Cas.Internal.Def.Interface
    ( module Cas.Internal.Def.Interface
    , module ReEx
    ) where

import           Data.List
import           Cas.Misc
import           PreludeCustom
import           Cas.Internal.Def as ReEx
import           Data.Complex.Generic

cPi, cE :: T
cPi = TC Pi
cE  = TC E

f :: F -> T
f  = TF

i = TF (imag 1) :: T

zeroF, oneF, aInvOneF :: F
(zeroF, oneF, aInvOneF ) = (0, 1, (-1))

zero, one, aInvOne :: T
(zero, one, aInvOne) = (f 0, f 1, f (-1))

x :: Int -> T
x = TX . X

_x :: Int -> X
_x = X

add, mul :: [T] -> T
add ts' = case length ts of
         0 -> zero
         1 -> head ts
         _ -> TAdd $ Add ts
    where
    ts = filter (not . isZero) ts'

mul ts'' = case length ts of
         0 -> one
         1 -> head ts
         _ -> TMul $ Mul ts
    where
    ts' = filter (not . isOne) ts''
    ts  = if elem zero ts'' then [zero] else ts'

addBin, mulBin,sub, pow :: T -> T -> T
addBin x y = add [x,y]
mulBin x y = mul [x,y]
sub x y = addBin x (aInv y)
pow = TPow

addRaw = TAdd . Add
mulRaw = TMul . Mul
ln :: T -> T
ln = TLn

aInv :: T -> T
aInv =  mul . (:[aInvOne])

mInv :: T -> T
mInv = flip pow aInvOne

tAddTs, tMulTs :: T -> [T]

tXI    = xI    . tX
tAddTs = addTs . tAdd
tMulTs = mulTs . tMul


isZeroF, isOneF :: F -> Bool
isZero, isOne, isC, isF, isX, isAdd, isMul, isLeaf, isFlat :: T   -> Bool
isThisX :: X -> T -> Bool

isZero    = (==zero)
isZeroF   = (==zeroF)
isOne     = (==one)
isOneF    = (==oneF)
isC       = \case (TC _)   -> True; _ -> False
isF       = \case (TF _)   -> True; _ -> False
isX       = \case (TX _)   -> True; _ -> False
isThisX x = \case (TX y)   -> x==y; _ -> False
isAdd     = \case (TAdd _) -> True; _ -> False
isMul     = \case (TMul _) -> True; _ -> False
isPow     = \case (TPow _ _) -> True; _ -> False
isLn      = \case (TLn _)  -> True; _ -> False



isLeaf     = or . ([isF, isC, isX,isLn] <*>) . return
isNonFLeaf = or . ([isX, isC, isLn] <*>) . return
isOper     = or . ([isAdd, isMul, isPow] <*>) . return

isFlat     = isAdd ?>>> all (not . isAdd) . tAddTs
         ||> isMul ?>>- all (not . isMul) . tMulTs
                    ||> True


simpleShow :: T -> String
simpleShow TC {..}      = show $  tC
simpleShow t@TF {..}    = wrap $ showF  tF
simpleShow t@TX {..}    = "x" <> show ( tXI t)
simpleShow t@TAdd {..}  = wrap $ intercalate "+" $ fmap simpleShow (tAddTs t)
simpleShow t@TMul {..}  = wrap $ intercalate "*" $ fmap simpleShow (tMulTs t)
simpleShow t@TPow {..}  = wrap $ simpleShow tPowB <> "^" <> simpleShow tPowE
simpleShow t@TLn {..}   = wrap $ "ln " <> simpleShow tLnT

(+:+) z w = TF $ z :+ w

eqType :: T -> T -> Bool
eqType (TF _) (TF _) = True
eqType (TX _) (TX _) = True
eqType (TC _) (TC _) = True
eqType (TAdd _) (TAdd _) = True
eqType (TMul _) (TMul _) = True
eqType (TPow _ _) (TPow _ _) = True
eqType (TLn _) (TLn _) = True
eqType t r = False

