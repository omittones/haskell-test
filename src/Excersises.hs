module Excersises where

norm :: [Double] -> Double
norm [] = 0
norm (a:rest) = do
    let norm' = norm rest
    sqrt $ a * a + norm' * norm'

decimate :: [a] -> [a]
decimate (a:_:rest) = a:decimate rest
decimate (a:_) = [a]
decimate _ = []

zipList :: ([a],[b]) -> [(a,b)]
zipList ([],_) = []
zipList (_,[]) = []
zipList (l1:rest1,l2:rest2) = (l1,l2):zipList (rest1,rest2)

main :: IO ()
main = do

    print $ zipList ([1, 2, 3, 4], "Hello")

    print $ decimate [1]
    print $ decimate [1,2]
    print $ decimate [1,2,3]

    print $ norm []
    print $ norm [1]
    print $ norm [1,2]

    return ()