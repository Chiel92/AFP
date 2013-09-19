module A7_1 where

--
-- The initial code
--
split []          = []
split (x:xs)      = (x, xs):[(y, x:ys) | (y, ys) <- split xs]

perms []          = [[]]
perms xs          = [(v:p) | (v, vs) <- split xs, p <- perms vs]

smooth n (x:y:ys) = abs (y - x) <= n && smooth n (y:ys)
smooth _ _        = True

smoothPerms :: Int -> [Int] -> [[Int]]
smoothPerms n xs = filter (smooth n) (perms xs)

--
-- The faster code
--

-- Note that we denote the Root seperately.
-- That's because the root doesn't have a value,
-- while nodes and leaves do have a value.
data Tree a = Leaf a | Node a [Tree a] | Root [Tree a]

-- Build a tree representing all permutations,
-- but stop developing a branch whenever it's not smooth anymore
permTree :: Int -> [Int] -> Tree Int
permTree n list = Root [permTree' b n list' | (b, list') <- (split list)]
  where
    permTree' a n []   = Leaf a
    permTree' a n list = Node a [permTree' b n list' | (b, list') <- (split list), abs (a - b) <= n]

-- Convert the permutation tree into a list of permutations
smoothPerms' :: Int -> [Int] -> [[Int]]
smoothPerms' n list = buildList (permTree n list)
  where
    buildList (Root children)   = (foldr (++) [] (map buildList children))
    buildList (Leaf a)          = [[a]]
    buildList (Node a children) = [a:child_list | child_list <- (foldr (++) [] (map buildList children))]
