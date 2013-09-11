module Main where

split [] = []
split (x:xs) = (x, xs):[(y, x:ys) | (y, ys) <- split xs]
perms [] = [[]]
perms xs = [(v:p) | (v, vs) <- split xs, p <- perms vs]
smooth n (x:y:ys) = abs (y - x) <= n && smooth n (y:ys)
smooth _ _ = True
smooth_perms :: Int -> [Int] -> [[Int]]
smooth_perms n xs = filter (smooth n) (perms xs)

main = do
    putStrLn "Boo"
