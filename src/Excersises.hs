module Excersises where
import Data.Char

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
--instead of [],_ and _,[] definition we can use _,_ after the full definition, to catch every non matched call
--remember pattern matching

tokenize :: String -> [Bool]
tokenize (f:rest) = do
    let is = if f == 'a' then True else False
    is:tokenize rest
tokenize _ = []

--currying
sum :: Int -> Int -> Int
sum a b = a + b
sum2 = Excersises.sum 2

--concatenate two lists
cat :: [a] -> [a] -> [a]
cat (a:at) b = a : cat at b
cat _ b = b

--currying
pig :: String -> String
pig = cat "pig"

toInts :: String -> [Int]
toInts (a:rest) = digitToInt a : toInts rest
toInts [] = []

--currying with map
altToInts = map digitToInt

squares :: [Int] -> [Int]
squares = map (\t -> t*t)



main :: IO ()
main = do

    print $ cat [1,2] [3,4]
    print $ cat [1] [3,4]
    print $ cat [] [3,4]

    print $ tokenize "abcaa"

    print $ zipList ([1, 2, 3, 4], "Hello")

    print $ decimate [1]
    print $ decimate [1,2]
    print $ decimate [1,2,3]

    print $ norm []
    print $ norm [1]
    print $ norm [1,2]

    return ()