module Calc where
import Data.Char
import Data.String

data Operator = Plus | Minus | Div | Times deriving (Show, Eq)
opToChar :: Operator -> Char
opToChar Plus = '+'
opToChar Minus = '-'
opToChar Div = '/'
opToChar Times = '*'

operator :: Char -> Operator
operator c | c == '+' = Plus
 | c == '-' = Minus
 | c == '/' = Div
 | c == '*' = Times

--constructors taking in argument of another type
data Token = TokLParen | TokRParen | TokAssign | TokSpace | TokOp Operator | TokIdent String | TokNum Int deriving (Show, Eq)

showContent :: Token -> String
showContent (TokOp op) = show $ opToChar op
showContent (TokIdent str) = str
showContent (TokNum i) = show i

data Expression

filterSpace :: [Token] -> [Token]
filterSpace = filter (\c -> c/=TokSpace)

tokenize :: String -> [Token]
tokenize (c:rest)
    | c == '(' = TokLParen : tokenize rest
    | c == ')' = TokRParen : tokenize rest
    | c == '=' = TokAssign : tokenize rest
    | elem c "+-*/" = TokOp (operator c) : tokenize rest
    | isDigit c = number c rest
    | isAlpha c = identifier c rest
    | isSpace c = TokSpace : tokenize rest
    | otherwise = error $ "Cannot tokenize " ++ [c]
tokenize [] = []

span :: (Char -> Bool) -> String -> (String, String)
span selector arg = als "" arg where
   als acc [] = (acc,[])
   als acc (c:cs)
     | selector c = let (acc',cs') = als acc cs in (c:acc',cs')
     | otherwise = (acc, c:cs)

identifier :: Char -> String -> [Token]
identifier c cs = let (str, cs') = Calc.span isAlphaNum cs in
                  TokIdent (c:str) : tokenize cs'
number :: Char -> String -> [Token]
number c cs = let (digs, cs') = Calc.span isDigit cs in
              TokNum (read (c:digs)) : tokenize cs'

parse :: [Token] -> Expression
parse = undefined

evaluate :: Expression -> Double
evaluate = undefined

main :: IO ()
main = print $ Calc.span isAlpha "robust123+1223greate222"