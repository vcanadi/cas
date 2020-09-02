{-# LANGUAGE TemplateHaskell #-}

module Cas.Misc.TH where

import           PreludeCustom
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

import           Cas.Interface

xIs   = [0..15]                                              :: [Int]

bakeAppName = (\nmStr -> mkName . (nmStr<>) . show)          :: String -> Int -> Name
bakeAppPat  = (\nmStr -> VarP . bakeAppName nmStr)           :: String -> Int -> Pat
bakeAppBody = (\nmStr i -> NormalB $
                    AppE (VarE $ mkName nmStr)
                         (LitE $ IntegerL $ fromIntegral i)) :: String -> Int -> Body

bakeApp :: String -> Int -> Dec
bakeApp nmStr = ValD <$> bakeAppPat nmStr <*> bakeAppBody nmStr <*> const []

bakeApps :: String -> [Int] -> [Dec]
bakeApps nmStr xs = fmap (bakeApp nmStr) xs

build :: Quasi m => m [Dec]
build = do
    return $ bakeApps "x"  xIs
          <> bakeApps "_x" xIs
