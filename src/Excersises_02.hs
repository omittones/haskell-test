module Excersises_02 where

--datatype in record syntax
data Person = Male { firstName :: String, lastName :: String } | Female { firstName :: String, lastName :: String }

--function that takes in a female, uses person@ for the entire pattern (Female _ _ )
--function that takes in a male uses standard datatype sytnax, also works as it should
greet :: Person -> String
greet person@(Female _ _) = "Hello, Miss " ++ (firstName person) ++ " " ++ (lastName person)
greet (Male firstName lastName) = "Hello, Mister " ++ firstName ++ " " ++ lastName

main_10 = do

    --equivalent invocations
    print $ greet $ Male { lastName="Doe", firstName="John" }
    print $ greet $ Male "John" "Doe"
    print $ greet $ Female "Jane" "Doe"
    return ()


--generic type, a can be of any type
data Nullable a = Null | Value a

printn :: (Show a) => Nullable a -> IO ()
printn (Value a) = print a
printn Null = print "NULL"

getnull :: Nullable a
getnull = Null

main :: IO ()
main = do
    printn $ Value 5
    printn $ Value "thing"
    printn $ (Null :: Nullable Int)
    return ()