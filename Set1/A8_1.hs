module A8_1 where
import Test.QuickCheck
import Criterion.Main
import A7_1

-- A useful function for QuickCheck
allSmoothPerms :: Int -> [Int] -> Property
allSmoothPerms n l = (n > 0) ==> all (smooth n) (smoothPerms' n l)

-- A benchmarking function for Criterion
array :: [Int]
array = [1,4,5,2,2,5,4,3]
test :: (Int -> [Int] -> [[Int]]) -> IO ()
test f = defaultMain [
        bgroup "smoothPerms" [bench "1" $ whnf (f 3) array,
        bench "2" $ whnf (f 6) array]
    ]
