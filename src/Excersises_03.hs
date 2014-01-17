module Excersises_03 where

main = do

    --getLine returns IO String, <- executes it and converts it to String
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "Hello, " ++ name ++ ", nice to meet you."

    putStrLn "Another line please"
    let thing = getLine
    name <- thing

    --return doesn't actualy stops execution, it creates a noop IO action to complement the other IO actions
    --so all of these will be queued but won't actually do anything, since they are noop IO actions
    return "one"
    return "two"

    --sequence takes an array of IO actions, and creates a single IO action from them
    sequence [print 1, print "2", print 3]
    --mapM does the same
    mapM print [1,2,3,4]
    --mapM_ also does the same, but throws the result away (IO action have a result, which is () when IO does output, and input when IO does input)
    mapM_ print [1,2,3,4]