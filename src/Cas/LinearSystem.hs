{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Cas.LinearSystem
    ( module Cas.LinearSystem
    , equ, linSys, linSysFromMatrix, (===)
    , (&&:), emptyLinSys
    )where
import           PreludeCustom
import           Data.List
import           Cas.Interface
import           Cas.LinearSystem.Internal
import           Cas.Rules
import           Cas.Instances.Show
import           Debug.Trace
import           Control.Monad.State.Lazy

membersWithX, membersWithoutX :: X -> Add -> T
membersWithX x Add {..} = add $ filter (tElem (TX x)) addTs
membersWithoutX x Add {..} = add $ filter (tNotElem (TX x)) addTs

removeXFromMembers  :: X -> T -> T
removeXFromMembers x TX {..}     = one
removeXFromMembers x t@TMul {..} = mul $ selectOnDepth 1 (not . isThisX x) t
removeXFromMembers x t@TAdd {..} = add $ fmap (removeXFromMembers x) (addTs tAdd)

expressXFromEqu :: X -> Equ -> T
expressXFromEqu x Equ {..} = expand $ expressXFromT x (normalize equT)
    where
    expressXFromT :: X -> T -> T
    expressXFromT x t@TAdd {..} =
        mul [ aInv $ membersWithoutX x tAdd
            , mInv $ removeXFromMembers x $  membersWithX x tAdd
            ]
    expressXFromT x t           = zero

solveForXEqu :: X -> Equ -> Rule
solveForXEqu x equ = x --> expressXFromEqu x equ

solveFor :: [X] -> LinSys -> LinSysSolution
solveFor []     (LinSys equs) = emptySolutionWithConstr equs
solveFor xs     (LinSys [])   = emptySolution
solveFor (x:xs) linSys       =
    case findEquForX linSys x of
        Nothing  -> solveFor xs linSys
        Just equ ->
            let newRule           = solveForXEqu x equ
                linSysWithNewRule = substituteInLinSys
                                        newRule
                                        (removeEqu equ linSys)
            in  addRule newRule $
                        solveFor xs linSysWithNewRule

solveForDbg :: [X] -> LinSys -> State String LinSysSolution
solveForDbg []     (LinSys equs) = return $ emptySolutionWithConstr equs
solveForDbg xs     (LinSys [])   = return emptySolution
solveForDbg (x:xs) linSys       = do
    modify (<> (show linSys <> "\n"))
    modify (<> ("Finding Equ to solve for " <> show x <> "...\n"))
    case findEquForX linSys x of
        Nothing  -> do
            modify (<> ("No Equ solvable for " <> show x <> "\n"))
            solveForDbg xs linSys
        Just equ -> do
            modify (<> ("Solving for " <> show x <> ", Equ: " <> show equ <> "\n"))
            let newRule          = solveForXEqu x equ
            let linSysWithNewRule = substituteInLinSys
                                        newRule
                                        (removeEqu equ linSys)
            modify (<> ("newRule:\n" <> show newRule <> "\n"))
            rec <- solveForDbg xs linSysWithNewRule
            return $ addRule newRule rec



backtrackRules :: [Rule] -> [Rule]
backtrackRules = reverse . backtrackRules' . reverse
    where
    backtrackRules' []     = []
    backtrackRules' (r:rs) = r : backtrackRules' (fmap (substituteInRuleBody r) rs)

backtrackSolutions :: LinSysSolution -> LinSysSolution
backtrackSolutions LinSysSolution {..} = LinSysSolution (backtrackRules solutionRules) constraints

solveForWithBS :: [X] -> LinSys -> LinSysSolution
solveForWithBS xs linSys = backtrackSolutions $ solveFor xs linSys

solveForWithBSDbg :: [X] -> LinSys -> State String LinSysSolution
solveForWithBSDbg xs linSys = do
    solution <- solveForDbg xs linSys
    modify (<> (show (backtrackSolutions solution) <> "\n"))
    return $ backtrackSolutions solution



solve :: LinSys -> LinSysSolution
solve linSys = solveFor (linSysXs linSys) linSys

solveWithBS :: LinSys -> LinSysSolution
solveWithBS linSys = solveForWithBS (linSysXs linSys) linSys

