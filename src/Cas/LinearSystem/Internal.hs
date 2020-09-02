module Cas.LinearSystem.Internal where

import           Data.List
import           PreludeCustom
import           Cas.Interface
import           Cas.Graph
import           Data.List

--test if X can be expressed from Equ
isNonAddExpressableFromT :: X -> T -> Bool
isNonAddExpressableFromT x TX {..}     = tX == x
isNonAddExpressableFromT x t@TMul {..} = uniqueOnDepth 1 (isThisX x) t
                                      && tNotElemFromDepth 2 (TX x) t
isNonAddExpressableFromT i t           = False

isXExpressableFromT :: X -> T -> Bool
isXExpressableFromT x t@TAdd {..} = tAnyOnDepth 1 (isNonAddExpressableFromT x) t
isXExpressableFromT x t           = isNonAddExpressableFromT x t

isXExpressableFromEqu :: X -> Equ -> Bool
isXExpressableFromEqu x Equ {..} = isXExpressableFromT x equT


--test if Equ is linear on variable x
isTLinearNonAddFor :: [X] -> T -> Bool
isTLinearNonAddFor xs t@TMul {..} =  uniqueOnDepth 1 (flip elem (fmap TX xs)) t
                                && (not . tAnyFromDepth 2 (flip elem (fmap TX xs))) t
isTLinearNonAddFor i t            = True

isTLinearFor :: [X] -> T -> Bool
isTLinearFor xs t@TAdd {..} = tAllOnDepth 1 (isTLinearNonAddFor xs) t
isTLinearFor xs t           = isTLinearNonAddFor xs t

isEquLinearFor :: [X] -> Equ -> Bool
isEquLinearFor xs Equ {..} = isTLinearFor xs equT

isEquLinearForBin :: Equ -> X -> X -> Bool
isEquLinearForBin equ x y = isEquLinearFor [x,y] equ


--test is LinSys is linear on variables xs
isLinSysFor :: [X] -> LinSys -> Bool
isLinSysFor xs LinSys {..} = and $ isEquLinearFor xs <$> linSysEqus


--build normalized Equ
equ :: T -> Equ
equ = Equ . normalize

infix 4 ===
(===) :: T -> T -> LinSys
t === r = LinSys  [equ $ sub t r]

--build normalized LinSys
linSys :: [T] -> LinSys
linSys = LinSys . fmap equ

emptyLinSys :: LinSys
emptyLinSys = linSys []

infixr 3 &&:
(&&:) :: LinSys -> LinSys -> LinSys
(LinSys equs0)  &&: (LinSys equs1) = LinSys $ equs0 <> equs1


--input linSys as Matrix
linSysFromMatrix :: [[F]] -> LinSys
linSysFromMatrix = LinSys . fmap eqFromList
    where

    eqFromList :: [F] -> Equ
    eqFromList = equ . foldl (+) 0 . zipWith (*) (1: fmap TX [X 0..]) . fmap f


linSysXs :: LinSys -> [X]
linSysXs LinSys {..} = nub $ fmap tX $ concatMap (selectXs . equT) linSysEqus


--find Xs expressable From Equ
solutionCandidateForEqu :: Equ -> [X] -> Maybe X
solutionCandidateForEqu equ xs = case solutionCandidatesForEqu equ xs of
    []    -> Nothing
    (y:_) -> Just y

solutionCandidatesForEqu :: Equ -> [X] -> [X]
solutionCandidatesForEqu equ xs =  maxCompleteSubList (isEquLinearForBin equ) xs



addRule :: Rule -> LinSysSolution -> LinSysSolution
addRule rule LinSysSolution {..} = LinSysSolution (rule:solutionRules) constraints

addConstraintToSolution :: Equ -> LinSysSolution -> LinSysSolution
addConstraintToSolution constraint LinSysSolution {..} = LinSysSolution solutionRules (constraint:constraints)

emptySolution = LinSysSolution [] []
emptySolutionWithConstr = LinSysSolution []

removeEqu :: Equ -> LinSys -> LinSys
removeEqu equ (LinSys equs) = LinSys $ delete equ equs

findEquForX :: LinSys -> X -> Maybe Equ
findEquForX LinSys {..} x = find (isXExpressableFromEqu x) linSysEqus
