module Calc where

data Token
data Expression

tokenize :: String -> [Token]
tokenize = undefined

parse :: [Token] -> Expression
parse = undefined

evaluate :: Expression -> Double
evaluate = undefined

main :: IO ()
main = putStrLn "It works, so are we done yet?"