{-|
Module : ExprParser
Description : Includes the parsers to parse strings into expressions.
Copyright : (c) Ali Kariapper 2018
License : WTFPL
Maintainer : kariappa@mcmaster.ca
Stability : experimental
Portability : Posix
-}

module ExprParser (parseExprD,parseExprF) where

import           ExprType

import           Text.Parsec
import           Text.Parsec.String


-- | Used to parse a string into an Expr Double type
parseExprD :: String -> Expr Double
parseExprD ss = case parse exprDOp "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr

-- | Used to parse a string into an Expr Float type
parseExprF :: String -> Expr Float
parseExprF ss = case parse exprFOp "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr


-- | Chain doubles to (+), (-), (*) and (/) operators
exprDOp :: Parser (Expr Double)
exprDOp = termDOp `chainl1` operatorOp

-- | Chain floats to (+), (-), (*) and (/) operators
exprFOp :: Parser (Expr Float)
exprFOp = termFOp `chainl1` operatorOp

-- | Checks if double has a negative sign infront
termDOp :: Parser (Expr Double)
termDOp = (negDigits' dfactor) <|> dfactor

-- | Checks if the float has a negative sign infront
termFOp :: Parser (Expr Float)
termFOp = (negDigits' ffactor) <|> ffactor


-- | If there are no numbers in the string an error is thrown
parseExprDFunc :: String -> Expr Double             
parseExprDFunc ss = case parse exprDFunc "" ss of 
                    Left err   -> error $ show err
                    Right expr -> expr

-- | If there are no numbers in the string an error is thrown
parseExprFFunc :: String -> Expr Float             
parseExprFFunc ss = case parse exprFFunc "" ss of 
                    Left err   -> error $ show err
                    Right expr -> expr



-- | Tries to parse functions of Type Expr Double including (Sin, Cos, Ln )
exprDFunc :: Parser (Expr Double)
exprDFunc = let opF = do { op <- funcOp; 
                               spaces;
                               x <- termDFunc;
                               spaces;
                               return $ op x }
                in try opF <|> termDFunc

-- | Tries to parse functions of Type Expr Float including (Sin, Cos, Ln )
exprFFunc :: Parser (Expr Float)
exprFFunc = let opF = do { op <- funcOp; 
                               spaces;
                               x <- termFFunc;
                               spaces;
                               return $ op x }
                in try opF <|> termFFunc


-- | Checks to see if value has a negative infront
termDFunc :: Parser (Expr Double)
termDFunc = (negDigits' dfactor) <|> dfactor

-- | Checks to see if value has a negative infront
termFFunc :: Parser (Expr Float)
termFFunc = (negDigits' ffactor) <|> ffactor

-- | Checks to see if a variable or value is being parsed
dfactor :: Parser (Expr Double)
dfactor = try dParse <|> vParse

-- | Checks to see if a variable or value is being parsed
ffactor :: Parser (Expr Float)
ffactor = try fParse <|> vParse

-- | Parse instances of Double
dParse :: Parser (Expr Double)
dParse = do { a <- double;
                   return $ Const a}

-- | Parses instances of float
fParse :: Parser (Expr Float)
fParse = do { a <- float;
                   return $ Const a}

-- | Variable Parser)
vParse :: Parser (Expr x)
vParse = do { var <- many1 letter;
                return $ Var var }




{- Utility Parsers -}
symbol :: String -> Parser String
symbol ss = let
  symbol' :: Parser String
  symbol' = do { 
                spaces;
                ss' <- string ss;
                spaces;
                return ss' 
                }
  in try symbol'

digits :: Parser String
digits = many1 digit

negDigits :: Parser String
negDigits = do { 
                neg <- symbol "-" ;
                dig <- digits ;
                return (neg ++ dig) 
                }

negDigits' :: Parser (Expr a) -> Parser (Expr a)
negDigits' x = do { 
                  symbol "-";
                  expr <- x;
                  return $ Neg expr     
                  }


digits' :: Parser String 
digits' = do { 
              front <- try digits' <|> digits;
              back  <- try deci <|> return "";
              return $ front ++ back 
             }

deci :: Parser String 
deci = do { d  <- char '.';
                dd <- digits; 
                return $ d:dd }

double :: Parser Double
double = fmap read $ digits' 

float :: Parser Float
float = fmap read $ digits'

-- | Idea to write my parser like this instead of seperately was from https://github.com/elwazana/CS1XA3/tree/master/Assign3
operatorOp :: Parser (Expr a -> Expr a -> Expr a)
operatorOp = do { 
    symbol "+"; return Add }
    <|> do { symbol "/"; return Div }
    <|> do { symbol "*"; return Mult }
    <|> do { symbol "^"; return Power }

-- | Idea to write all my parser like this instead of seperately was from https://github.com/elwazana/CS1XA3/tree/master/Assign3
funcOp :: Parser (Expr a -> Expr a)
funcOp = do { 
       string "cos"; return Cos }
       <|> do { string "sin"; return Sin }
       <|> do { string "ln"; return Ln }
       <|> do { string "exp"; return Exp }






















