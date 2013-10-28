module Server where

import Control.Concurrent.STM
import Network
import System.Environment (getArgs)


main :: IO ()
main = do
    args <- getArgs
    putStrLn $ head args


