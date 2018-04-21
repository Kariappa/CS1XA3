module ExprParser (parseExprD,parseExprF) where

import           ExprType

import           Text.Parsec
import           Text.Parsec.String

parseExprD :: String -> Expr Double
parseExprD ss = case parse exprDOp "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr

parseExprF :: String -> Expr Float
parseExprF ss = case parse exprFOp "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr



exprDOp :: Parser (Expr Double)
exprDOp = termDOp `chainl1` operatorOp

exprFOp :: Parser (Expr Float)
exprFOp = termFOp `chainl1` operatorOp


termDOp :: Parser (Expr Double)
termDOp = (negDigits' dfactor) <|> dfactor

termFOp :: Parser (Expr Float)
termFOp = (negDigits' ffactor) <|> ffactor



parseExprDFunc :: String -> Expr Double             
parseExprDFunc ss = case parse exprDFunc "" ss of 
                    Left err   -> error $ show err
                    Right expr -> expr

parseExprFFunc :: String -> Expr Float             
parseExprFFunc ss = case parse exprFFunc "" ss of 
                    Left err   -> error $ show err
                    Right expr -> expr




exprDFunc :: Parser (Expr Double)
exprDFunc = let opF = do { op <- funcOp; 
                               spaces;
                               x <- termDFunc;
                               spaces;
                               return $ op x }
                in try opF <|> termDFunc


exprFFunc :: Parser (Expr Float)
exprFFunc = let opF = do { op <- funcOp; 
                               spaces;
                               x <- termFFunc;
                               spaces;
                               return $ op x }
                in try opF <|> termFFunc


termDFunc :: Parser (Expr Double)
termDFunc = (negDigits' dfactor) <|> dfactor


termFFunc :: Parser (Expr Float)
termFFunc = (negDigits' ffactor) <|> ffactor

dfactor :: Parser (Expr Double)
dfactor = try dParse <|> vParse

ffactor :: Parser (Expr Float)
ffactor = try fParse <|> vParse

dParse :: Parser (Expr Double)
dParse = do { a <- double;
                   return $ Const a}

fParse :: Parser (Expr Float)
fParse = do { a <- float;
                   return $ Const a}


vParse :: Parser (Expr x)
vParse = do { var <- many1 letter;
                return $ Var var }





symbol :: String -> Parser String
symbol ss = let
  symbol' :: Parser String
  symbol' = do { spaces;
                 ss' <- string ss;
                 spaces;
                 return ss' }
  in try symbol'

digits :: Parser String
digits = many1 digit

negDigits :: Parser String
negDigits = do { neg <- symbol "-" ;
                 dig <- digits ;
                 return (neg ++ dig) }

negDigits' :: Parser (Expr a) -> Parser (Expr a)
negDigits' x = do { symbol "-";
               expr <- x;
               return $ Neg expr }


digits' :: Parser String 
digits' = do { front <- try digits' <|> digits;
                    back  <- try deci <|> return "";
                    return $ front ++ back }

deci :: Parser String 
deci = do { d  <- char '.';
                dd <- digits; 
                return $ d:dd }

double :: Parser Double
double = fmap read $ digits' 

float :: Parser Float
float = fmap read $ digits'


operatorOp :: Parser (Expr a -> Expr a -> Expr a)
operatorOp = do { symbol "+"; return Add }
    <|> do { symbol "/"; return Div }
    <|> do { symbol "*"; return Mult }
    <|> do { symbol "^"; return Power }

-- | Unary 'Expr' datatype constructors
funcOp :: Parser (Expr a -> Expr a)
funcOp = do { string "cos"; return Cos }
       <|> do { string "sin"; return Sin }
       <|> do { string "ln"; return Ln }
       <|> do { string "exp"; return Exp }






















