module A2_9 where

-- Some test expressions
p1, p2, p3 :: Int
p1 = start store 3 store 5 add stop
p2 = start store 3 store 6 store 2 mul add stop
p3 = start store 2 add stop


-- The start function


-- The store function
store :: Int -> [Int]
store x = x

-- The add function
add :: [Int] -> [Int]

-- The mul function

-- The stop function

