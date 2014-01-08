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

tokenizeChar :: Char -> Token
tokenizeChar c
    | elem c "+-*/" = TokOp (operator c)
    | isDigit c  = TokNum (digitToInt c)
    | isAlpha c  = TokIdent [c]
    | otherwise = error $ "Cannot tokenize " ++ [c]

tokenize :: String -> [Token]
tokenize = map tokenizeChar


parse :: [Token] -> Expression
parse = undefined

evaluate :: Expression -> Double
evaluate = undefined

main :: IO ()
main = do

    putStrLn $ showContent $ TokNum 5

    line <- getLine
    putStrLn line
    if length line /= 0 then
        main
    else
        return ()