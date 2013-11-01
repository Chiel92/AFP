module Server where

import Prelude hiding (id, catch)
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Network
import Data.Map
import Data.Foldable


data Client = Client {
    getId     :: Int,
    getName   :: String,
    getHandle :: Handle ,
    getChan   :: TChan String
}
data Server = Server (TVar (Map Int Client)) -- TODO: data vs type


main :: IO ()
main = do
    server <- createServer
    socket <- listenOn $ PortNumber 9595
    acceptSockets server socket 0


-- Create a server with no clients
createServer :: IO Server
createServer = newTVarIO empty >>= return . Server


-- Create a client
createClient :: Int -> String -> Handle -> IO Client
createClient id name handle = newTChanIO >>= return . (Client id name handle)


-- Accept sockets
acceptSockets :: Server -> Socket -> Int -> IO ()
acceptSockets server socket id = do
    (handle, host, port) <- accept socket
    hSetBuffering handle LineBuffering
    forkIO $ addClient server handle id `finally` hClose handle
    acceptSockets server socket (id + 1)


-- Add the client and manage it
addClient :: Server -> Handle -> Int -> IO ()
addClient server handle id = do
    -- Insert the client in the server's clientList
    name   <- hGetLine handle
    client <- createClient id name handle
    addClient' server client

    -- Manage the client
    manageClient server client `finally` deleteClient server client
    -- TODO - we delete the same client twice!!!!!! Oh no.
    return ()


-- Do the actual adding of the client
addClient' :: Server -> Client -> IO ()
addClient' server@(Server clientList) client = atomically $ do
    modifyTVar' clientList $ insert (getId client) client
    broadcast server $ "** " ++ (getName client) ++ " entered the room **"


-- Manage the client
manageClient :: Server -> Client -> IO ()
manageClient server client = do
    waitUntillClosed <- newEmptyMVar
    listenId <- forkIO $ listenClient server client `finally` tryPutMVar waitUntillClosed ()
    sendId   <- forkIO $ sendClient client `finally` tryPutMVar waitUntillClosed ()
    takeMVar waitUntillClosed
    killThread listenId
    killThread sendId


-- Delete the client
deleteClient :: Server -> Client -> IO ()
deleteClient server@(Server clientList) client = atomically $ do
    modifyTVar' clientList $ delete $ getId client
    broadcast server $ "** " ++ (getName client) ++ " left the room **"


-- Listen to the client
listenClient :: Server -> Client -> IO ()
listenClient server client = do
    msg <- hGetLine $ getHandle client
    atomically $ broadcast server $ getName client ++ ": " ++ msg
    listenClient server client


-- Send messages to the clients
sendClient :: Client -> IO ()
sendClient client = do
    putStrLn $ "send " ++ getName client
    msg <- atomically $ readTChan $ getChan client
    putStrLn $ "sent " ++ getName client ++ ": " ++ msg
    hPutStrLn (getHandle client) msg
    sendClient client


-- Send a message to all clients
broadcast :: Server -> String -> STM ()
broadcast (Server clientList) msg = do
    readTVar clientList >>= Data.Foldable.mapM_ sendMsg
      where
        sendMsg client = writeTChan (getChan client) msg

