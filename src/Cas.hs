module Cas where


import           PreludeCustom

import           Cas.QuickCheckTest
import           Test.QuickCheck (arbitrary)
import           Test.QuickCheck.Gen
import           Cas.Rules
import           Cas.LinearSystem
import           Cas.Interface
import qualified Text.PrettyPrint.Boxes as Bx
import           Cas.Examples
import           Control.Monad.State.Lazy

debugTable :: [T] -> [[String]]
debugTable ts =
    [ "","","","converge","-","-","-","-","-"]
    :
    [ "term"
    , "normalize term"
    , "expanded term"
    ]
    :
    fmap (\t ->
        [  show t
        , (show . normalize) t
        , (show . expand) t

        ]
        ) ts


mainCas :: IO ()
mainCas = do
    generatedTs <- sample' (arbitrary :: Gen T)
    printTable $ debugTable generatedTs

normalizePrint :: Int -> T -> IO ()
normalizePrint n t =  mapM_  (printT .  flip normalizeIter t)  [0..n]

nprint = normalizePrint 1


x012 = [_x0,_x1,_x2]

linSys0, linSys1, linSys2, linSys3, linSys4, linSys5 :: LinSys
[equ0,equ1,equ2] = linSysEqus $ linSysFromMatrix [
      [1, 1, 1,  1]
    , [1, 2, 3,  4]
    , [1, 2, 4,  7]
    ]

linSys0 = LinSys [equ0,equ1,equ2]

linSys1 =
       2*x0 +   x1 +  3*x2 === 1
   &&: 2*x0 + 6*x1 +  8*x2 === 3
   &&: 6*x0 + 8*x1 + 18*x2 === 5


linSys2 =
       3*x0 +      x1 + (-6)*x2 === -10
   &&: 2*x0 +      x1 + (-5)*x2 === -8
   &&: 6*x0 + (-3)*x1 +    3*x2 === 0


linSys3 =
       x0 +           x2 === 1
   &&: x0 +      x1 + x2 === 2
   &&: x0 + (-1)*x1 + x2 === 1


linSys4 =
          x0 + x3*x1 === 1
   &&:  2*x0 +  6*x1  === 3



linSysDir = "linSysPrimjeri"

showSolveForWithBSDbg xs linSys s =  execState (solveForWithBSDbg xs linSys)
                                             ("printSolveForWithBSDbg " <> show xs <> " " <> s <> "\n")
printSolveForWithBSDbg xs linSys = putStrLn $ showSolveForWithBSDbg xs linSys "_"

linSysMain :: IO ()
linSysMain = do
   writeFile (linSysDir <> "/1.txt") $ showSolveForWithBSDbg x012 linSys1 "linSys1"
   writeFile (linSysDir <> "/2.txt") $ showSolveForWithBSDbg x012 linSys2 "linSys2"
   writeFile (linSysDir <> "/3.txt") $ showSolveForWithBSDbg x012 linSys3 "linSys3"
   writeFile (linSysDir <> "/4.txt") $ showSolveForWithBSDbg [_x0,_x1] linSys4 "linSys4"

linSys5 =
       x0 + x1 + x2 + x3 + x4  === 0
   &&: x0 + x1 + x2 + x3 + x4  === 0
   &&: x0 + x1 + x2 + x3 + x4  === 0
   &&: x0 + x1 + x2 + x3 + x4  === 0
   &&: x0 + x1 + x2 + x3 + x4  === 0
   &&: x0 + x1 + x2 + x3 + x4  === 0
   &&: x0 + x1 + x2 + x3 + x4  === 0
   &&: x0 + x1 + x2 + x3 + x4  === 0
   &&: emptyLinSys





-- LinSysSolution rules constraints = solveLinSys exLinSys

-- rule0 = rules!!0
-- rule1 = rules!!1
-- rule2 = rules!!2

-- x0Sol = substitute (rules!!1) x1Sol
-- x1Sol = substitute (rules!!2) x2Sol
-- x2Sol = ruleT (rules!!2)

solveForX012, solveForX012WithBS:: LinSys -> LinSysSolution
solveForX012  = solveFor x012
solveForX012WithBS = solveForWithBS x012

solveForEx0 = flip solveFor linSys0
solveForEx0WithBS = flip solveForWithBS linSys0

solveForX012Ex0 = solveForEx0 x012
solveForX012Ex0WithBS = solveForEx0WithBS x012

solveForX012Ex1 = solveFor x012 linSys1
solveForX012Ex1WithBS = solveForWithBS x012 linSys1
