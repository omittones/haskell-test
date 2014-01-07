module Calc where

data Operator = Plus | Minus | Div | Times deriving (Show, Eq)
opToChar :: Operator -> Char
opToChar Plus = '+'
opToChar Minus = '-'
opToChar Div = '/'
opToChar Times = '*'

data Token = TokOp Operator | TokIdent String | TokNum Integer deriving (Show, Eq)

showContent :: Token -> String
showContent (TokOp op) = show $ opToChar op
showContent (TokIdent str) = str
showContent (TokNum i) = show i

data Expression

tokenize :: String -> [Token]
tokenize = undefined

parse :: [Token] -> Expression
parse = undefined

evaluate :: Expression -> Double
evaluate = undefined

main :: IO ()
main = do
    line <- getLine
    putStrLn line
    if length line /= 0 then
        main
    else
        return ()