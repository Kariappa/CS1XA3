{-# LANGUAGE IndecidableInstances #-}
{-# LANGAUGE FlexibleInstances #-}

module ExprDiff where

import qualified Data.Map as Map
eval :: Map.Map String a -> Expr a -> a


{- Class DiffExpr:
-		Differentiable Expression
-----------------------------------------
This class has methods over the Expr datatype
that assist with contruction and evaluation
of differentiable expressions
-----------------------------------------
Methods:
eval : takes a dictionary of variables identifiers
		and values, and uses sit to compur the
		Expr fully
simplify: takes a possibly incomplete dictionary
		  and uses it to reduce Expr as much as 
		  possible
		  e1 = x + y
		  e2 = y + x
		  simplify e1 == simplify e2
		  Add (Add (Var "x") (Const 1)) (Add (Const 2) (Var "y"))
		  => Add (Const 3) (Add (Var "x") (Var "y")) 
partDiff : given an var identifier, differentiate
		   IN TERMS of identifier

Defeat Methods
	!+,!*,var,val : are function wrapper for Expr
					constructors that perform
					additional simplification

-}

class DiffExpr a where
	eval :: Map.Map String a -> Expr a -> a
	simplify :: Map.Map String a -> Expr a -> Expr a 
	partDiff :: String -> Expr a -> Expr a
	{- Defeault Methods -}
	(!+) :: Expr a -> Expr a -> Expr a
	e1 !+ e2 e2 = simplify (Map.fromList[]) $ Add e1 e2
	(!+) :: Expr a -> Expr a -> Expr a
	e1 !* e2 = simplify (Map.fromList[]) $ Mult e1 e2
	val :: a -> Expr a
	val x = Const x
	var x String -> Expr a
	var x = Var x

{- Most intuative instance of DiffExpr
 - Num insrance only relies on *,-
 - Methods:
 -   eval : .....
 	 simplify : ....
 	 partDiff : ....}
-}


instance DiffExpr (Num a) => DiffExpr a where
	eval vrs (Add e1 e2) = eval vrs e1 + eval vrs e2
	eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
	eval vrs (Const x) = x
	eval vrs (Var x) = case Map.lookup x vrs of
						Just v -> v
						Nothing -> error "failed lookup in eval"

	simplify _e = e -- #TODO finish me!
	partDiff _e = e -- #TODO finish me!