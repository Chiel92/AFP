module A2_9 where

-- A nice abbreviation
type Stack = [Int]

-- Some test expressions
-- p0, p1, p2, p3 :: Int
p0 = start store 1 stop
p1 = start store 3 store 5 add stop
p2 = start store 3 store 6 store 2 mul add stop
p3 = start store 2 add stop

-- The start function
start :: (Stack -> r) -> r
start r = r []

-- The store function
store :: Stack -> Int -> (Stack -> r) -> r
store s n r = r (n:s)

-- The add function
add :: Stack -> (Stack -> r) -> r
add (x:y:ys) r = r (x + y : ys)

-- The mul function
mul :: Stack -> (Stack -> r) -> r
mul (x:y:ys) r = r (x * y : ys)

-- The stop function
stop :: Stack -> Int
stop []     = 0
stop (x:xs) = x

