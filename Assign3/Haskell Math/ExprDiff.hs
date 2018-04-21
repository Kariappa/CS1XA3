{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module : ExprDiff
Description : Has functions for evaluating and simplifying expressions, as well as computing partial derivatives. 
Copyright : (c) Ali Kariapper 2018
License : WTFPL
Maintainer : kariappa@mcmaster.ca
Stability : experimental
Portability : Posix
 - Methods:
 - eval: Uses a dictionary of variable identifiers and values to compute expressions
 - Simplify: Uses a potentially incomplete dictionary to simplify expressions
 - partDiff: Differentiates in terms of a given identifier
 - Default Methods:
        !+, !*, val, var : are function wrappers that provide additional simplification
-}


module ExprDiff where

import ExprType
import ExprPretty

import qualified Data.Map.Strict as Map
-- * Class Declaration For DiffExpr
class DiffExpr a where
  
  -- | Function that is used to evalute an expression given a dictionary
  eval :: Map.Map String a -> Expr a -> a
  
  -- | Function that is used to simplify an expression given a dictionary
  simplify :: Map.Map String a -> Expr a -> Expr a
  
  -- | Function that is used to calculate the partial differential of an expression
  partDiff :: String -> Expr a -> Expr a

  -- | These are the function that are used to apply operators such as + and - over Expr types
  (!+) :: Expr a -> Expr a -> Expr a
  e1 !+ e2 = simplify (Map.fromList []) $ Add e1 e2
  (!*) :: Expr a -> Expr a -> Expr a
  e1 !* e2 = simplify (Map.fromList []) $ Mult e1 e2

  (!/) :: Expr a -> Expr a -> Expr a
  e1 !/ e2 = simplify (Map.fromList []) $ Div e1 e2

  (!^) :: Expr a -> Expr a -> Expr a

  val :: a -> Expr a
  val x = Const x
  var :: String -> Expr a
  var x = Var x
  newSin :: Expr a -> Expr a
  newSin e1 = simplify (Map.fromList []) $ Sin e1
  newCos :: Expr a -> Expr a
  newCos e1 = simplify (Map.fromList []) $ Cos e1
  newLn :: Expr a -> Expr a
  newLn e1 = simplify (Map.fromList []) $ Ln e1
  newExp :: Expr a -> Expr a
  newExp e1 = simplify (Map.fromList []) $ Exp e1
  e1 !^ e2 = simplify (Map.fromList []) $ Power e1 e2

-- Instances for DiffExpr for function that are to be used with Expr Data Types
instance (Num a, Eq a, Fractional a, Show a, Floating a, Ord a) => DiffExpr a where
    eval vrs (Add e1 e2) = eval vrs e1 + eval vrs e2
    eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
    eval vrs (Neg e)          = (-1) * (eval vrs e)
    eval vrs (Sin e1)     = sin $ eval vrs e1
    eval vrs (Cos e1)     = cos $ eval vrs e1
    eval vrs (Exp e1)    = exp $ eval vrs e1
    eval vrs (Ln e1)      = log $ eval vrs e1
    eval vrs (Power e1 e2) = (eval vrs e1) ** (eval vrs e2)
    eval vrs (Const x) = x
    eval vrs (Var x) = case Map.lookup x vrs of 
                            Just v -> v
                            Nothing -> error "Failed lookup in eval"

 
  {- Here are different pattern matching situations for Add-}
    simplify vrs (Add e1 e2) =
      case (simplify vrs e1, simplify vrs e2) of
        (Const a,Const b) -> Const (a + b)
        (a, Const 0)     -> simplify vrs e1
        (Const 0, a)     -> simplify vrs e2
        (Const a, Var x) -> (Add (simplify vrs (Var x)) (Const a))
        (Var x, Const a) -> (Add (simplify vrs (Var x)) (Const a))
        (Ln a, Ln b)     -> simplify vrs $ Ln (Mult (simplify vrs a) (simplify vrs b))
        (a, b)            -> (Add a b)


  {- Here are different pattern matching situations for Mult-}
    simplify vrs (Mult e1 e2) =
      case (simplify vrs e1, simplify vrs e2) of

        (Const a, Const b ) -> Const (a*b)
        (a, Const 0) -> (Const 0)
        (Const 0, a) -> (Const 0)
        (Const 1, y) -> simplify vrs y
        (x, Const 1) -> simplify vrs x 
        (Power a b, Power c d) -> if a == c --
                                then Power a (Add b d)
                                else Mult (Power a b) (Power c d)
        (a, b)           -> (Mult a b)

  {- Here are different pattern matching situations for Div-}
    simplify vrs (Div e1 e2) = 
      case (simplify vrs e1, simplify vrs e2) of 
        (Const x, Const y) -> Const (x/y)
        (x, Const 0) -> error "Zero Division" 
        (Const 0, y) -> (Const 0)
        (Var a, Var b)             -> if a == b
                                    then (Const 1)
                                    else Div (Var a) (Var b)


  {- Here are different pattern matching situations for variables and constants-}
    simplify vrs (Const a) = (Const a)
    simplify vrs (Var x) = case Map.lookup x vrs of
                       Just v  -> (Const v)
                       Nothing -> (Var x)
    simplify vrs (Neg e)  = Const (eval vrs (Neg e))

  {- Here are different pattern matching situations for Sin-}
    simplify vrs (Sin e1) = 
      case (simplify vrs e1) of
        (Const x) -> (Const (sin(x))) 
        (Var x)   -> (Sin (Var x))
        (x)       -> (Sin (simplify vrs x))


  {- Here are different pattern matching situations for Cos-}
    simplify vrs (Cos e1) = 
      case (simplify vrs e1) of
        (Const x) -> (Const (cos(x))) 
        (Var x)   -> (Cos (Var x))
        (x)       -> (Cos (simplify vrs x))


  {- Here are different pattern matching situations for Natural log-}
    simplify vrs (Ln e1) =
      case (simplify vrs e1) of 
        (Const 1)   -> (Const 0)
        (Const x)   -> if x > 0 then (Const (log(x))) else error "No Value"
        (Var x)     -> (Ln (Var x))
        (x)         -> (Ln (simplify vrs x))

  {- Here are different pattern matching situations for e-}
    simplify vrs (Exp e1) = 
      case (simplify vrs e1) of
        (Const x) -> (Const (exp(x)))
        (Var x)   -> (Exp (Var x))
        (x)       -> (Exp (simplify vrs x))

  {- Here are different pattern matching situations for Power-}
    simplify vrs (Power e1 e2) = 
      case (simplify vrs e1, simplify vrs e2) of 
        (Const 0, x) -> (Const 0)
        (x, Const 0) -> (Const 1)
        (x, Const (-1))    -> simplify vrs $ Div (Const 1) x
        (a,b)       ->  Power (simplify vrs a) (simplify vrs b)
   
  {- Here are different pattern matching situations attempting to partially differentiate expressions-}
    partDiff f (Add a b) = Add (partDiff f a) (partDiff f b)
    partDiff f (Mult a b) = Add (Mult a (partDiff f b)) (Mult a (partDiff f b))
    partDiff f (Neg a) = Neg (partDiff f a)
    partDiff f (Sin a) = Mult (Cos a) $ partDiff f a
    partDiff f (Cos a) = Mult (Mult (Const (-1)) (Sin a)) $ partDiff f a
    partDiff f (Ln a)  = Mult (partDiff f a) (Div (Const 1) a)
    partDiff _ (Const a)    = Const 0
    partDiff f (Var a) = if a == f then (Const 1) else (Const 0)
    partDiff f (Power a b)  = partDiff f (Exp (Mult b (Ln a)))


























  


