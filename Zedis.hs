module Zedis (
  connectRedis,
  disconnectRedis,
  brpop,
  RedisConnection(..)
) where

import Data.Maybe

import Network.BSD
import Network.Socket
import Network.Socket.ByteString

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

data RedisConnection = RedisConnection {
  redisHost :: String,
  redisPort :: Integer,
  tcpSocket :: Socket
}

connectRedis :: String -> IO RedisConnection
connectRedis host = do
  proto <- Network.BSD.getProtocolNumber "tcp"
  he <- Network.BSD.getHostByName host
  sock <- socket AF_INET Stream proto
  setSocketOption sock KeepAlive 1
  
  Network.Socket.connect sock (SockAddrInet (6379 :: PortNumber) (hostAddress he))
  
  return RedisConnection {
    redisHost = host,
    redisPort = 6379,
    tcpSocket = sock
  }
  
disconnectRedis :: RedisConnection -> IO ()
disconnectRedis conn = do
  shutdown (tcpSocket conn) ShutdownBoth
  Network.Socket.shutdown (tcpSocket conn) ShutdownBoth

readSection :: Socket -> IO B.ByteString
readSection sock = do
  res <- readSection' sock B.empty
  return res

readSection' :: Socket -> B.ByteString -> IO B.ByteString
readSection' sock str
  | (BC.length str) == 0 = do
    p <- Network.Socket.ByteString.recv sock 1
    res <- readSection' sock p
    return res
  | (BC.last str) == '\n' = return $ B.init $ B.init $ str
  | otherwise = do
    p <- Network.Socket.ByteString.recv sock 1
    readSection' sock (B.append str p)
    
bcToInt :: BC.ByteString -> Int
bcToInt bc = fst $ fromJust $ BC.readInt bc

readBulkStr :: Socket -> IO B.ByteString
readBulkStr sock = do
  lenBulkStr <- readSection sock
  let strlen = (bcToInt $ B.tail lenBulkStr)
  str <- Network.Socket.ByteString.recv sock (strlen+2)
  return $ B.take strlen str
  
brpop :: RedisConnection -> String -> IO (Maybe B.ByteString)
brpop conn list = do
  -- this line is a complete hack
  _ <- Network.Socket.ByteString.send (tcpSocket conn) (BC.pack ("*3\r\n$5\r\nbrpop\r\n$" ++ (show $ length list) ++ "\r\n" ++ list ++ "\r\n$1\r\n0\r\n"))
  
  firstChunk <- readSection (tcpSocket conn)

  case BC.head firstChunk of
    '-' -> return Nothing
    '*' -> do
      if (bcToInt (B.tail firstChunk)) == 2
      then do
        listName <- readBulkStr (tcpSocket conn)
        payload <- readBulkStr (tcpSocket conn)
        return $ Just payload
      else return Nothing
    otherwise -> return $ Just firstChunk