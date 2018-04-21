{-|
Module : ExprType
Description : Incudes the type defenitions for contructing the Expr type.
Copyright : (c) Ali Kariapper 2018
License : WTFPL
Maintainer : kariappa@mcmaster.ca
Stability : experimental
Portability : Posix
-}
module ExprType where

import           Data.List
-- | DataType Declaration
data Expr a = Add (Expr a) (Expr a)  -- ^ Standard Binary Addition
            | Mult (Expr a) (Expr a) -- ^ Standard Binary Multiplication
            | Div (Expr a) (Expr a)  -- ^ An expression divided by another expression
            | Neg (Expr a)			 -- ^ The negative of and expression
            | Const a                -- ^ Value Wrapper
            | Var String             -- ^ Variable Identifier
            | Sin ( Expr a)          -- ^ The Sin of an expression
            | Cos (Expr a)           -- ^ The Cos of an expression
            | Ln (Expr a)            -- ^ The natural log of an expression
            | Exp (Expr a)			 -- ^ e to the power of an expression
            | Power (Expr a) (Expr a)-- ^ An expression raised to the power of another

  deriving Eq



{- getVars :
            Unwraps and receives the identifiers from
            an Expr
-}
getVars :: Expr a -> [String]
getVars (Add e1 e2)     = getVars e1 `union` getVars e2
getVars (Mult e1 e2)    = getVars e1 `union` getVars e2
getVars (Neg e)          = getVars e
getVars (Const _)       = []
getVars (Var ident)     = [ident]
getVars (Sin e1)        = getVars e1
getVars (Cos e1)        = getVars e1
getVars (Ln e1)        = getVars e1
getVars (Exp e1)        = getVars e1
getVars (Power e1 e2)   = getVars e1 `union` getVars e2