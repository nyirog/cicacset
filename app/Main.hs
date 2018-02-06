module Main where

import Lib
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TChan, atomically, writeTChan, readTChan, newTChanIO, newBroadcastTChan, dupTChan)
import Network.Socket

type CicaChannel = TChan String

main :: IO ()
main = do
    readerChannel <- newTChanIO :: IO CicaChannel
    broadcastChannel <- atomically newBroadcastTChan :: IO CicaChannel
    _ <- forkIO $ forwardMessage readerChannel broadcastChannel
    sock <- createSocket
    acceptSocket sock readerChannel broadcastChannel

consume :: Socket -> CicaChannel -> IO ()
consume sock channel = forever $ do
    msg <- recv sock 122
    atomically $ writeTChan channel msg

produce :: Socket -> CicaChannel -> IO ()
produce sock channel = forever $ do
    msg <- atomically $ readTChan channel

    send sock msg
    pure ()

forever :: Monad a => a () -> a ()
forever u = do
    u
    forever u

forwardMessage :: CicaChannel -> CicaChannel -> IO ()
forwardMessage readerChannel broadcastChannel = forever $ do
    msg <- atomically $ readTChan readerChannel
    atomically $ writeTChan broadcastChannel msg

createSocket :: IO Socket
createSocket = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 iNADDR_ANY)
  listen sock 5
  pure sock

acceptSocket :: Socket -> CicaChannel -> CicaChannel -> IO ()
acceptSocket listeningSocket readerChannel broadcastChannel = forever $ do
    sock <- fst <$> accept listeningSocket
    _ <- forkIO $ consume sock readerChannel
    bb <- atomically $ dupTChan broadcastChannel
    _ <- forkIO $ produce sock bb
    pure ()
