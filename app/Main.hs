module Main where

import Lib
import Control.Monad (forever)
import Control.Exception (try, IOException)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TChan, atomically, writeTChan, readTChan, newTChanIO, newBroadcastTChan, dupTChan)
import Network.Socket

data Message = Message {userId:: Int, message:: String}
type CicaChannel = TChan Message

main :: IO ()
main = do
    broadcastChannel <- atomically newBroadcastTChan :: IO CicaChannel
    sock <- createSocket
    acceptSocket 1 sock broadcastChannel

consume :: Int -> Socket -> CicaChannel -> IO ()
consume userId sock channel = do
    msg <- try (recv sock 122) :: IO (Either IOException String)
    case msg of
        Left error -> do
            atomically $ writeTChan channel $ Message 0 ("User [" ++ (show userId) ++ "] left\n")
            pure ()
        Right message -> do
            atomically $ writeTChan channel $ Message userId message
            consume userId sock channel

produce :: Int -> Socket -> CicaChannel -> IO ()
produce userId sock channel = forever $ do
    (Message senderId msg) <- atomically $ readTChan channel
    if senderId == userId then
        pure ()
    else do
        send sock ("[" ++ (show senderId)++ "]: " ++ msg)
        pure ()

createSocket :: IO Socket
createSocket = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 5
    pure sock

acceptSocket :: Int -> Socket ->  CicaChannel -> IO ()
acceptSocket userId listeningSocket channel = do
    sock <- fst <$> accept listeningSocket
    _ <- forkIO $ consume userId sock channel
    broadcastChannel <- atomically $ dupTChan channel
    _ <- forkIO $ produce userId sock broadcastChannel
    atomically $ writeTChan channel $ Message 0 ("User [" ++ (show userId) ++ "] joined\n")
    acceptSocket (userId + 1) listeningSocket channel
