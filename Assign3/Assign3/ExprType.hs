module ExprType where

import Data.List

{- Expression Datatype
------------------------------------------------
- Wraps different operations in a expression tree
Ops:
	Add - standard binary addition
	Mult - Stardard binary mulitplication
	Const - wrapper for simple values
	Var - String identigier for variables
-}

data Expr a = Add (Expr a) (Expr a)
			| Mult (Expr a) (Expr a)
			| Const a 
			| Var String
	deriving Eq

{- getVars:
			Retrieves varialbe identifiers from
			an Expr
-}
getVars :: Expr a -> [String]
getVars (Add e1 e2) = getVars e1 `union` getVars e2
getVars (Multi e1 e2) getVars e1 `union` getVars e2
getVars (Const _) = []
getVars (Var ident) = [ident]