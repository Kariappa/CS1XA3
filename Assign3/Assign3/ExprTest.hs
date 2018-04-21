module Expr Test where

import ExprType
import ExprDiff
import ExprParser
import ExprPretty

impoty qualified Data.Map as Map

sampleExpr1 :: Expr Double
sampleExpr1 = (var "x") !+ (var "y")

listtoExpr1 :: [Double] -> Expr Double
listToExpr1 xs = 
