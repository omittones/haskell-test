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

--foldl and foldr


myhead :: [a] -> a
myhead [] = undefined
myhead (h:rest) = h

-- to use + operator on a type we have to specify that a conforms to Num typeclass
addall :: (Num a) => [a] -> a
addall whole@(f:rest) = f + addall rest --we can access the whole list using whole
addall [] = 0

-- or Read and Num typeclass
addstring :: (Read a, Num a) => a -> String -> a
addstring arg text = do
    let parsed = read text
    arg + parsed

main :: IO ()
main = do

    --read convert String to something, :: Double specifies the desired return type (compiler can't infer so we have to be explicit)
    print $ (read "5.6" :: Double) * 2

    --list comprehension, for every x in [1..4] where x is not 4 and y in [1..4] where y is not 3
    print $ [(x,y) | x <- [1..4], x /= 4, y <- [1..4], y /= 3]

    --use list comprehension for calculating length
    print $ Prelude.sum [1 | _ <- [1,2,4]]

    --index operator
    print $ [1,2,4] !! 0
    return ()


main_10 = do

    print $ foldr (\value acc -> acc++[value]) "" ['a','b','c']
    print $ foldl (\acc value -> acc++[value]) "" ['a','b','c']

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