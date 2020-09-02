{-# LANGUAGE TemplateHaskell #-}
module Cas.Examples where

import           PreludeCustom

import           Cas.Interface
import           Cas.Misc.TH
import           Control.Monad.State.Lazy

build

add1 = add [x0,x0]
mul1 = mul [x0,x0]
add2 = add [x0,x1]
add3 = add [x0,x1,x2]
addMul3 = add [mul [x0,x1],x2]
mulAdd3 = mul [add [x0,x1],x2]
addMul4 = add [mul [x0,x1],mul [x2,x3]]
mulAdd4 = mul [add [x0,x1],add [x2,x3]]

