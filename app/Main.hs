module Main where

import Lib
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TChan, atomically, writeTChan, readTChan, newTChanIO)
import Network.Socket

type CicaChannel = TChan String

main :: IO ()
main = do
    chan <- newTChanIO :: IO CicaChannel
    _ <- forkIO $ generate chan
    _ <- forkIO $ consume chan
    sock <- createSocket
    forever $ forwardMessage sock chan

generate :: CicaChannel -> IO ()
generate channel = forever $ do
    atomically $ writeTChan channel "cica"
    threadDelay 1000000

consume :: CicaChannel -> IO ()
consume channel = forever $ do
    s <- atomically $ readTChan channel
    putStrLn s

forever :: Monad a => a () -> a ()
forever u = do
    u
    forever u

forwardMessage :: Socket -> CicaChannel -> IO ()
forwardMessage sock channel = do
    msg <- recv sock 100
    atomically $ writeTChan channel msg

createSocket :: IO Socket
createSocket = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 iNADDR_ANY)
  listen sock 2
  fst <$> accept sock
