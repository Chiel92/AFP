module A8_3 where

-- You gotta love hacks
forceBoolList :: [Bool] -> r -> r
forceBoolList xs k = if length (fbl xs) == 42 then k else k where
    fbl []     = []
    fbl (x:xs) |x==True  = True:fbl xs
               |x==False = False:fbl xs

-- Warning, the following code might crash.
test = do
    two <- return $ forceBoolList [True, False, undefined, True] (1+1)
    print two

