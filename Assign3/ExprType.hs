module ExprType where

import           Data.List

data Expr a = Add (Expr a) (Expr a)
            | Mult (Expr a) (Expr a)
            | Const a
            | Var String
            | Sin ( Expr a)
            | Cos (Expr a)
            | Ln (Expr a) 
            | Exp (Expr a)
            | Power (Expr a) (Expr a)

  deriving Eq

getVars :: Expr a -> [String]
getVars (Add e1 e2) 	= getVars e1 `union` getVars e2
getVars (Mult e1 e2)  	= getVars e1 `union` getVars e2
getVars (Const _)     	= []
getVars (Var ident)   	= [ident]
getVars (Sin e1)	  	= getVars e1
getVars (Cos e1)		= getVars e1
getVars (Ln e1)			= getVars e1
getVars (Exp e1)	  	= getVars e1
getVars (Power e1 e1)  	= getVars e1 `union` getVarse e2