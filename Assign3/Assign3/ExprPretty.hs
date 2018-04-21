module ExpePretty where
{- Instance Show Expr
		Provides a pretty resperesentation of our datatype
		Matching the DSL provied in Diff expr 
-}
parens :: String -> String
parens ss = "(" ++ ss ++")"
Intance Show a => Show (Expr a) where
	show (Mult e1 e2) = parens (show e1) ++ " !* " ++ parens (show  e2)
	show (Add e1 e2) = parens (show e1) ++ "!+ " ++ parens (show e2)
	show (Var ss) = parens $ "var \ " " ++ ss ++ "\"
	show (Const x) =parens $ "val " ++ show x