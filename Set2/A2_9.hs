module A2_9 where

-- A nice abbreviation
type Stack = [Int]

-- Some tests
p0, p1, p2, p3 :: Int
p0 = start store 1 stop
p1 = start store 3 store 5 add stop
p2 = start store 3 store 6 store 2 mul add stop
p3 = start store 2 add stop

-- The start function
start :: (Stack -> k) -> k
start k = k []

-- The store function
store :: Stack -> Int -> (Stack -> k) -> k
store s n k = k (n:s)

-- The add function
add :: Stack -> (Stack -> k) -> k
add (x:y:s) k = k (x + y : s)

-- The mul function
mul :: Stack -> (Stack -> k) -> k
mul (x:y:s) k = k (x * y : s)

-- The stop function
stop :: Stack -> Int
stop (x:s) = x

