module Excersises_03 where


calcpi = last $ take 2000000 sums
sums = scanl (\x (y,s) -> x + (s*y*4)) 0 order'
order' = zip order (cycle [1,(-1)])
order = map (\x -> 1.0 / x) [1,3..]

--total = 0
--state = 1
--for i in range(1,1000000,2):
--    total+= (1.0/i ) * state
--    state*=-1
--print total*4

main = do

    print $ calcpi
    return ()

main10 = do

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