{-|
Module : ExprType
Description : Contains functions that test the simplifying and evaluating of expressions.
Copyright : (c) Ali Kariapper 2018
License : WTFPL
Maintainer : kariappa@mcmaster.ca
Stability : experimental
Portability : Posix
-}

module ExprPretty where

import ExprType

parens' :: String -> String
parens' ss = "(" ++ ss ++ ")"

parens :: String -> String
parens ss = ss

-- | Instance of Show Class For The Expr Type Expressions
instance Show a => Show (Expr a) where
    show (Mult e1 e2)  = parens' $ (show e1) ++ " !* " ++ (show e2)
    show (Div e1 e2)  = parens' $ (show e1) ++ " !/ " ++ (show e2)
    show (Add e1 e2) = parens' $ (show e1) ++ " + " ++ (show e2)
    show (Var ss) = parens $ "var \"" ++ ss ++ "\""
    show (Const x) = parens $ "val " ++ show x
    show (Ln x) = parens' $ "Ln " ++ show x
    show (Exp x) = parens' $ "e ^ " ++ show x
    show (Cos x) = parens $ "Cos " ++ show x
    show (Sin x) = parens $ "Sin " ++ show x
    show (Power e1 e2) = parens' $ show e1 ++ " ^ " ++ show e2