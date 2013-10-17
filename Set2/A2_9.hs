module A2_9 where

-- The naieve solution
type Stack = [Int]


-- Implementation of the stack functions
start :: (Stack -> k) -> k
start k = k []

store :: Stack -> Int -> (Stack -> k) -> k
store s n k = k (n:s)

add :: Stack -> (Stack -> k) -> k
add (x:y:s) k = k (x + y : s)

mul :: Stack -> (Stack -> k) -> k
mul (x:y:s) k = k (x * y : s)

stop :: Stack -> Int
stop (x:s) = x


-- Tests
p0, p1, p2, p3 :: Int
p0 = start store 1 stop
p1 = start store 3 store 5 add stop
p2 = start store 3 store 6 store 2 mul add stop
p3 = start store 2 add stop


-- This solution rejects programs that require
-- nonexisting stack elements during type checking
data Stack' s = Stack' s Int deriving Show
data EmptyStack' = EmptyStack' deriving Show


-- Implementation of the stack functions
start' :: (EmptyStack' -> r) -> r
start' k = k EmptyStack'

store' :: s -> Int -> (Stack' s -> k) -> k
store' s n k = k (Stack' s n)

add' :: Stack' (Stack' s) -> (Stack' s -> k) -> k
add' (Stack' (Stack' s x) y) k = k (Stack' s (x + y))

mul' :: Stack' (Stack' s) -> (Stack' s -> k) -> k
mul' (Stack' (Stack' s x) y) k =  k (Stack' s (x * y))

stop' :: Stack' s -> Int
stop' (Stack' s n) = n


-- Tests
p0', p1', p2' :: Int
p0' = start' store' 1 stop'
p1' = start' store' 3 store' 5 add' stop'
p2' = start' store' 3 store' 6 store' 2 mul' add' stop'

-- As required p3' doesn't compile
-- p3' :: Int
-- p3' = start' store' 2 add' stop'

