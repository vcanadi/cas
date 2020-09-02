module Cas.Internal.Utils
    ( module Cas.Internal.Utils
    , module Cas.Internal.Def.Interface
    , module Cas.Internal.Instances.TFunctor
    , module Cas.Internal.Instances.TFoldable ) where

import           PreludeCustom

import           Cas.Internal.Def.Interface
import           Cas.Internal.Instances.TFunctor
import           Cas.Internal.Instances.TFoldable
import           Data.Ratio
import           Data.Complex.Generic
import           Cas.Misc

isNegative :: T -> Bool
isNegative = isF   ?>>> (<0) . tF
         ||> isMul ?>>- uniqueOnDepth 1 ((&&) <$> isF <*> (<0) . tF)
         ||>            False


selectCs, selectFs, selectXs, selectNonFs  :: T   -> [T]
lengthCs, lengthFs, lengthXs, lengthNonFs, lengthOpers :: T   -> Int
selectCs = select isC
selectFs = select isF
selectXs = select isX
selectNonFs = select (not . isF)
selectNonFLeafs = select isNonFLeaf
lengthCs = tLength isC
lengthFs = tLength isF
lengthXs = tLength isX
lengthNonFs = tLength (not . isF)
lengthNonFLeafs = tLength isNonFLeaf
lengthLeafs = tLength isLeaf
lengthChilds = tLengthOnDepth 1 (const True)
lengthOpers = tLength isOper
lengthNonFlats = tLength (not . isFlat)
lengthAddChilds = length . concatMap tAddTs . select isAdd
lengthMulChilds = length . concatMap tMulTs . select isMul
lengthNotPowAddChilds = length
                         . concatMap tAddTs
                         . filter isAdd
                         . ((:) <$> id
                                <*> concatMap tToListChilds
                                  . select (not . isPow) )
lengthLns = tLength isLn
lengthNonOpers = tLength (not . isOper)

subTsAdd :: Add -> [[T]]
subTsAdd Add {..} = fmap (\case (TMul (Mul {..})) -> mulTs; t -> [t]) addTs

subTsMul :: Mul -> [[T]]
subTsMul Mul {..} = fmap (\case (TAdd (Add {..})) -> addTs; t -> [t]) mulTs


subTs :: T   -> [[T]]
subTs TAdd {..}  = subTsAdd tAdd
subTs TMul {..}  = subTsMul tMul
subTs _          = error "subTs called on non Add or Mul term"

addElems, mulElems :: T -> [T]
addElems = isAdd ?>>> tAddTs ||> return
mulElems = isMul ?>>> tMulTs ||> return

addFBin, mulFBin :: F -> F -> F
addFBin w0 w1 = w0+w1

mulFBin w0 w1 = w0*w1

addF, mulF :: [F] -> F
addF = foldr addFBin zeroF
mulF = foldr mulFBin oneF

filterF = fmap tF . filter isF
filterNotF = filter (not . isF)

addCollapse, mulCollapse :: [T] -> T
addCollapse = add
         . filter (not . isZero)
         . ((:) <$> TF .  addF . filterF
                <*> filterNotF)


mulCollapse = any isZero ?>>> const zero
                          ||> mul
                            . filter (not . isOne)
                            . ((:) <$> TF . mulF . filterF
                                   <*> filterNotF)

addBinCollapse, mulBinCollapse, powCollapse :: T -> T -> T
addBinCollapse x y = addCollapse $ addElems x <> addElems y
mulBinCollapse x y = mulCollapse $ mulElems x <> mulElems y

powCollapse _    (TF 0)  = one
powCollapse (TF 0) _     = zero
powCollapse (TF 1) _     = one
powCollapse x    (TF 1)  = x
powCollapse (TF z) (TF w) | imagPart w == 0 && denominator (realPart w) == 1 = f $ z^^(numerator $ realPart w)
powCollapse x    y           = pow x y



mulParts :: T -> (F,T)
mulParts = isF    ?>>>  (,) <$> tF
                            <*> const one
       ||> isMul  ?>>> ((,) <$> mulF . filterF
                            <*> mul . filterNotF ) . tMulTs
       ||>              (,) <$> const oneF
                            <*> id

powParts :: T -> (T,T)
powParts = ((&&) <$> isPow
                 <*> isF . tPowE) ?>>> (,) <$> tPowE <*> tPowB
                                   ||> (,) <$> const one <*> id
-- powParts =  isPow ?>>> (,) <$> tPowE <*> tPowB
--                                    ||> (,) <$> const one <*> id

