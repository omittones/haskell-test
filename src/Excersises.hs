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

-- to use + operator on a type we have to specify that a conforms to Num typeclass
addall :: (Num a) => [a] -> a
addall whole@(f:rest) = f + addall rest --we can access the whole list using whole
addall [] = 0

-- or Read and Num typeclass
addstring :: (Read a, Num a) => a -> String -> a
addstring arg text = do
    let parsed = read text
    arg + parsed

-- guard clauses, pattern match with condition
mysig :: (Num a, Ord a) => a -> Int
mysig a
 | a > 0 = 1
 | a < 0 = -1
 | a == 0 = 0

-- infinite list that cycles 'a' infinite times
-- the expression is lazy so we can write take 5 (repeat' 3) to get [3,3,3,3,3], and it won't block
repeat' a = a : repeat' a

-- quicksort using list comprehension
qs :: (Ord a) => [a] -> [a]
qs [] = []
qs (hd:tl) = let before = qs [t | t <- tl, t <= hd]
                 after = qs [t | t <- tl, t > hd]
                 in before ++ [hd] ++ after

-- more concise quick sort using filter function and curried <= and > operator
-- operators are also function, only infix
qs2 :: (Ord a) => [a] -> [a]
qs2 [] = []
qs2 (hd:tl) = qs2 (filter (<hd) tl) ++ [hd] ++ qs2 (filter (>=hd) tl)

main :: IO ()
main = do

    --prints when does sum of square roots of numbers [1..] exceed 1000
    print $ length $ takeWhile (<1000) $ scanl1 (+) $ map sqrt [1..]

    --function composition using .
    --whitespace before in is needed because Haskell
    let sqrt3 = sqrt . sqrt
     in print $ sqrt3 16

    --reverse list using foldl
    print $ foldl (\acc item -> item:acc) [] [1,2,3,4,5]

    --flip arguments of map, and apply curried lambda function (which adds 1) to a list
    print $ flip map [1,2,3,4,5] ((\x y -> x + y) 1)

    print $ qs [2,5,5,4,2,1,1,4]

    print $ qs2 [2,5,5,4,2,1,1,4]

    return ()

main_11 = do

    --show all numbers less than 4, < is also a function
    print $ filter (<5) [1,2,3,4,5]

    --will not cause block, because lazy eval
    print $ take 5 (repeat' 3)

    --we have to specify the return type explicitly because print needs it to know what to do
    print $ (qs [] :: [Int])

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