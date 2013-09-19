module A8_1 where
import Test.QuickCheck
import Criterion.Main
import A7_1

-- Testing can be slow with long arrays
allSmoothPerms :: Int -> [Int] -> Property
allSmoothPerms n l = (n > 0) ==> all (smooth n) (smoothPerms' n l)

array :: [Int]
array = [12,34,5,2,2,45,4,3]
test :: (Int -> [Int] -> [[Int]]) -> IO ()
test f = defaultMain [
        bgroup "smoothPerms" [bench "1" $ whnf (f 3) array,
        bench "2" $ whnf (f 6) array]
    ]

-- BENCHMARKING RESULTS FOR SMOOTHPERMS
-- benchmarking smoothPerms/1
-- mean: 395.6888 ms, lb 395.4506 ms, ub 395.9561 ms, ci 0.950
-- std dev: 1.297122 ms, lb 1.150550 ms, ub 1.521108 ms, ci 0.950
--
-- benchmarking smoothPerms/2
-- mean: 401.1403 ms, lb 400.6384 ms, ub 402.5480 ms, ci 0.950
-- std dev: 3.994759 ms, lb 1.702542 ms, ub 8.643855 ms, ci 0.950
--
--
-- BENCHMARKING RESULTS FOR SMOOTHPERMS'
-- benchmarking smoothPerms/1
-- mean: 4.303921 ms, lb 4.298044 ms, ub 4.314679 ms, ci 0.950
-- std dev: 39.47772 us, lb 24.92080 us, ub 68.04137 us, ci 0.950
--
-- benchmarking smoothPerms/2
-- mean: 4.344374 ms, lb 4.313322 ms, ub 4.482213 ms, ci 0.950
-- std dev: 285.1843 us, lb 30.18719 us, ub 672.1628 us, ci 0.950

