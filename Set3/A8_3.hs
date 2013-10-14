module A8_3 where

-- You gotta love hacks
forceBoolList :: [Bool] -> r -> r
forceBoolList xs k = if length (forceBoolList' xs) == 42 then k else k

-- A version of forceBoolList that has type [Bool] to [Bool]
forceBoolList' :: [Bool] -> [Bool]
forceBoolList' []                = []
forceBoolList' (x:xs)|x == True  = True:forceBoolList' xs
                     |x == False = False:forceBoolList' xs

-- Warning, the following code might crash.
test = do
    two <- return $ forceBoolList [True, 1337 > 9001, undefined, True] (1+1)
    print two

test' = do
    list <- return $ forceBoolList' [True, 1337 > 9001, undefined, True]
    print $ length list

-- The reason why you might want to use [Bool] -> r -> r is because seq does so too.
-- You can use it on the fly and be sure it evaluates while doing something else.
-- You can however define a forceBoolList' so that it has type [Bool] -> [Bool]
-- and still evaluates the list.
-- force a = seq a a is useless, because it only evaluates the list,
-- and not the boolean expressions inside.

