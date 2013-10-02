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


-- The solution that rejects programs that require
-- nonexisting stack elements during type checking
data Stack' s = Stack' s Int deriving Show
data Empty = Empty

-- example usage for Stack'
x = Empty
y = Stack' x 2
z = Stack' y 5

-- Some tests
--p0', p1', p2' :: Int
p0' = start' store' 1 stop'
p1' = start' store' 3 store' 5 add' stop'
p2' = start' store' 3 store' 6 store' 2 mul' add' stop'
--p3' :: Int
--p3' = start' store' 2 add' stop'

-- The start function
start' :: (Empty -> r) -> r
start' k = k Empty

-- The store function
store' :: s -> Int -> (Stack' s -> k) -> k
store' s n k = k (Stack' s n)

-- The add function
add' :: Stack' (Stack' s) -> (Stack' s -> k) -> k
add' (Stack' (Stack' s x) y) k = k (Stack' s (x + y))

-- The mul function
mul' :: Stack' (Stack' s) -> (Stack' s -> k) -> k
mul' (Stack' (Stack' s x) y) k =  k (Stack' s (x + y))

-- The stop function
stop' :: Stack' s -> Int
stop' (Stack' s n) = n

