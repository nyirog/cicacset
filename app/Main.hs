module Main where

import Lib
import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TChan, atomically, writeTChan, readTChan, newTChanIO, newBroadcastTChan, dupTChan)
import Network.Socket

data Message = Message {userId:: Int, message:: String}
type CicaChannel = TChan Message

main :: IO ()
main = do
    broadcastChannel <- atomically newBroadcastTChan :: IO CicaChannel
    sock <- createSocket
    acceptSocket sock broadcastChannel

consume :: Socket -> CicaChannel -> IO ()
consume sock channel = forever $ do
    msg <- recv sock 122
    atomically $ writeTChan channel $ Message 0 msg

produce :: Socket -> CicaChannel -> IO ()
produce sock channel = forever $ do
    (Message userId msg) <- atomically $ readTChan channel
    send sock msg
    pure ()

createSocket :: IO Socket
createSocket = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 5
    pure sock

acceptSocket :: Socket ->  CicaChannel -> IO ()
acceptSocket listeningSocket channel = forever $ do
    sock <- fst <$> accept listeningSocket
    _ <- forkIO $ consume sock channel
    broadcastChannel <- atomically $ dupTChan channel
    _ <- forkIO $ produce sock broadcastChannel
    pure ()
