module Calc where
import Data.Char

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
data Token = TokOp Operator | TokIdent String | TokNum Int deriving (Show, Eq)

showContent :: Token -> String
showContent (TokOp op) = show $ opToChar op
showContent (TokIdent str) = str
showContent (TokNum i) = show i

data Expression

filterSpace :: String -> String
filterSpace = filter (\c -> c/=' ')

tokenize :: String -> [Token]
tokenize (c:rest)
    | elem c "+-*/" = TokOp (operator c) : tokenize rest
    | isDigit c  = TokNum (digitToInt c) : tokenize rest
    | isAlpha c  = identifier c rest
    | otherwise = error $ "Cannot tokenize " ++ [c]
tokenize [] = []

alnums :: String -> (String, String)
alnums arg = als "" arg where
   als acc [] = (acc,[])
   als acc (c:cs)
     | isAlpha c = let (acc',cs') = als acc cs in (c:acc',cs')
     | otherwise = (acc, c:cs)

identifier :: Char -> String -> [Token]
identifier c cs = let (str, cs') = alnums cs in
                  TokIdent (c:str) : tokenize cs'

parse :: [Token] -> Expression
parse = undefined

evaluate :: Expression -> Double
evaluate = undefined

main :: IO ()
main = print $ alnums "robust123+1223greate222"