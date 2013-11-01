module Main where

import Network
import System.IO
import System.Environment
import Control.Concurrent


main :: IO ()
main = do
    args <- getArgs
    hostname <- return $ head args
    nickname <- return $ args!!1

    -- Connect to the server
    handle <- connectTo hostname $ PortNumber 9595
    hSetBuffering handle LineBuffering
    putStrLn "** Chat client, type quit to quit **"
    hPutStrLn handle nickname

    -- Start the send and receive loop
    receiveId <- forkIO $ receive handle
    send receiveId handle
    return ()


-- The send loop
send :: ThreadId -> Handle -> IO ()
send receiveId handle = do
    msg <- getLine
    if msg == "quit" || msg == "exit"
    then do
        killThread receiveId
        hClose handle
    else do
        hPutStrLn handle msg
        send receiveId handle


-- The receive loop
receive :: Handle -> IO ()
receive handle = do
    response <- hGetLine handle
    putStrLn response
    receive handle

