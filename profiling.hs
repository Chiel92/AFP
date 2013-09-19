import A7_1

array :: [Int]
array = [12,34,5,2,2,45,4,3]
main :: IO ()
main = do
    result1 <- return $ smoothPerms' 3 array
    result2 <- return $ smoothPerms' 6 array
    print result1
    print result2
