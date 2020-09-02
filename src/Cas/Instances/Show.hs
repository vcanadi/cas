{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Cas.Instances.Show where


import           PreludeCustom
import           Data.List
import           Data.Tree (Tree(..))
import qualified Data.Tree as Tree
import qualified Data.Tree.Pretty as Pretty
import           Data.Ratio
import qualified Text.PrettyPrint.Boxes as Bx

import           Cas.Internal.Interface
import           Cas.Internal.Normalization
import           Cas.Misc

wrapAdd add@TAdd {..} = "(" <> show add <> ")"
wrapAdd t             = show t

wrapMul mul@TMul {..} = "(" <> show mul <> ")"
wrapMul t             = show t

wrapNonLeafs add@TAdd {..} = "(" <> show add <> ")"
wrapNonLeafs mul@TMul {..} = "(" <> show mul <> ")"
wrapNonLeafs pow@TPow {..} = "(" <> show pow <> ")"
wrapNonLeafs t             = show t

-- fix this to work: show ((-1)*x0) = -x0
-- showAdd [] = ""
-- showAdd [t] = show t
-- showAdd ts  = show (head ts)
--            <> concatMap
--               (\t -> bool ("+" <> show t)
--                           ("-" <> wrapAdd (normalize $ mul [aInvOne, t]))
--                           (isNegative t) )
--               (tail ts)


showX :: X -> String
showX (X i) = "x" <> show  i

instance Show X where
    show (X i) = "_x" <> show  i
instance Show Add where
    show Add {..} = intercalate " + " $ fmap  show  addTs
    -- show Add {..} = showAdd addTs
instance Show Mul where
    show Mul {..} = intercalate "*" $ fmap wrapAdd mulTs


instance Show T where
    -- show = simpleShow
    show TC {..}    = show tC
    show TF {..}    = showF tF
    show TX {..}    = showX tX
    show TAdd {..}  = show tAdd
    show TMul {..}  = show tMul
    show TPow {..}  =  wrapNonLeafs tPowB <> "**" <> wrapNonLeafs tPowE
    show TLn {..}   = "ln " <> wrapNonLeafs tLnT

toTree :: T -> Tree String
toTree TF {..}   = Node (showF tF) []
toTree TX {..}   = Node (showX tX) []
toTree TC {..}   = Node (show tC) []
toTree TAdd {..} = Node "+" $ fmap toTree (addTs tAdd)
toTree TMul {..} = Node "*" $ fmap toTree (mulTs tMul)
toTree TPow {..} = Node "**" [toTree tPowB, toTree tPowE ]
toTree TLn {..}  = Node ("ln")    [toTree tLnT ]

showT :: T -> String
showT = Pretty.drawVerticalTree . toTree

printT :: T -> IO ()
printT t = do
    putStrLn . show  $ norm t
    putStrLn . showT $ t


instance Show Equ where
    show Equ {..} = show equT <> " = 0"

instance Show LinSys where
    show LinSys {..} = "linSys:\n" <> intercalate "\n" (fmap show linSysEqus)

instance Show Rule where
    show (Rule x t) = show x <> " --> " <> show t

instance Show LinSysSolution where
    show LinSysSolution  {..} = "linSysSolution:\n"
                             <>  if any (\Equ {..} -> equT /= zero) constraints
                                 then "no solution"
                                 else "rules:\n" <> intercalate "\n" (fmap show solutionRules) <> "\n"
    -- show LinSysSolution  {..} = "linSysSolution:\n"
    --                          <> (if null solutionRules then "no solutions\n"
    --                             else  "rules:\n" <> intercalate "\n" (fmap show solutionRules) <> "\n")
    --                          <> (if null constraints   then "no constraints\n"
    --                             else "constraints:\n" <> intercalate "\n" (fmap show constraints))



printTable :: [[String]] -> IO ()
printTable rows = Bx.printBox $ Bx.hsep 2 Bx.left (map (Bx.vcat Bx.left . map Bx.text) (transpose rows))
