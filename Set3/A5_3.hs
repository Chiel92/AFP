{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module A5_3 where
import Control.Monad.Reader
import System.Random

one :: Int
one = 1

two :: Int
two = 2

randomN :: (RandomGen g) => Int -> g -> Int
randomN n g = (fst (next g) `mod` (two * n + one)) - n

-- This is the most general type
sizedInt :: (MonadTrans t, MonadReader Int (t m), MonadReader g m, RandomGen g) =>
            t m Int
sizedInt = do
    n <- ask
    g <- lift ask
    return (randomN n g)

-- TODO

