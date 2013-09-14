module A7_1 where

-- The naive code
split [] = []
split (x:xs) = (x, xs):[(y, x:ys) | (y, ys) <- split xs]
perms [] = [[]]
perms xs = [(v:p) | (v, vs) <- split xs, p <- perms vs]
smooth n (x:y:ys) = abs (y - x) <= n && smooth n (y:ys)
smooth _ _ = True
smoothPerms :: Int -> [Int] -> [[Int]]
smoothPerms n xs = filter (smooth n) (perms xs)

-- The smart code
data Tree a = Leaf a | Node a [Tree a] | Root [Tree a]

permTree :: Int -> [Int] -> Tree Int
permTree n list = Root [permTree' b n list' | (b, list') <- (split list)]
    where
        permTree' a n [] = Leaf a
        permTree' a n list = Node a [permTree' b n list' | (b, list') <- (split list), abs (a - b) <= n]

smoothPerms' :: Int -> [Int] -> [[Int]]
smoothPerms' n list = buildList (permTree n list)
    where
        buildList (Root children) = (foldr (++) [] (map buildList children))
        buildList (Leaf a) = [[a]]
        buildList (Node a children) = [a:child_list | child_list <- (foldr (++) [] (map buildList children))]
