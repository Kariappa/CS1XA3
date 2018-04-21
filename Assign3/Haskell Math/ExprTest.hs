{-|
Module : ExprTest
Description : Contains functions that test the simplifying and evaluating of expressions.
Copyright : (c) Ali Kariapper 2018
License : WTFPL
Maintainer : kariappa@mcmaster.ca
Stability : experimental
Portability : Posix
-}


module ExprTest where

import ExprType
import ExprDiff
import ExprParser
import ExprPretty
import Test.QuickCheck

import qualified Data.Map as Map

-- | Property For QuickCheck to check the Mult contructor
testPropMult :: Double -> Double -> Bool
testPropMult a b = eval (Map.fromList [("x",a),("y",b)]) (Mult (Var "x") (Var "y")) == a * b

-- | Property For QuickCheck To check the Cos contructor
testPropCos :: Double -> Bool
testPropCos y = eval (Map.fromList [("x",y)]) (Cos (Var "x")) == cos(y)

-- | Checks the evaluation of an expression of type Expr Double

evalProp1 :: Double -> Double -> Bool  
evalProp1 a b = (eval (Map.fromList[("x", 10.0)]) (Mult (Add (Const a) (Const b)) (Var "x"))) - ((a+b)*(10.0)) <= 1.0 

-- | Sample expressions evaluated to test effectiveness of simplification
simpProp1 a b =
    let empty = Map.fromList[]
        beforesimp = (Mult (Mult (Const a) (Const b)) (Mult (Const a) (Const b)))
    in (eval empty (simplify empty beforesimp)) - ((a*b)**2) <= 1.0

-- | Checks if a variable is not in the list of vars it should be returned as a variable
simpProp2 :: Double -> Double -> String -> Bool
simpProp2 x y z = simplify (Map.fromList [("x", x),("y", y)]) (Var z) == Var z