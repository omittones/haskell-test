module Main where

add a b = a + b

askUser question = do
    print question
    output <- getLine
    return output

printStuff = do
    print "entered main method"
    --print $ add (askUser "first number") (askUser "secondNumber")
    print "exited main method"

main = do
    printStuff
