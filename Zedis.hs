module Zedis (
  connectRedis,
  disconnectRedis,
  brpop
) where
  
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Network.Socket
import Network.BSD
import Network.Socket.ByteString

import GHC.Word

data RedisConnection = RedisConnection {
  redisHost :: String,
  redisPort :: Integer,
  tcpSocket :: Socket,
  connected :: Bool
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
    tcpSocket = sock,
    connected = True
  }
  
disconnectRedis :: RedisConnection -> IO ()
disconnectRedis conn = do
  shutdown (tcpSocket conn) ShutdownBoth

readHeader :: Socket -> B.ByteString -> IO B.ByteString
readHeader sock str
  | (BC.length str) == 0 = do
    p <- Network.Socket.ByteString.recv sock 1
    res <- readHeader sock p
    return res
  | (BC.last str) == '\n' = do
    return str
  | otherwise = do
    p <- Network.Socket.ByteString.recv sock 1
    readHeader sock (B.append str p)

brpop :: RedisConnection -> String -> IO (Maybe B.ByteString)
brpop conn list = do
  -- this line is a complete hack
  _ <- Network.Socket.ByteString.send (tcpSocket conn) (BC.pack ("*3\r\n$5\r\nbrpop\r\n$" ++ (show $ length list) ++ "\r\n" ++ list ++ "\r\n$1\r\n0\r\n"))
  
  -- TODO:
  -- Redis returns a 2-element array, not a bulk string
  
  firstChunk <- readHeader (tcpSocket conn) B.empty
  
  case BC.head firstChunk of
    '-' -> return $ Just firstChunk
    '$' -> do
      let numBytes = (read $ BC.unpack $ B.tail firstChunk) :: Int
  
      case numBytes of
        -1 -> return Nothing
        0 -> return Nothing -- should return something, but I'm too tired
        otherwise -> do
          _ <- Network.Socket.ByteString.recv (tcpSocket conn) 2 -- skip CRLF
          payload <- Network.Socket.ByteString.recv (tcpSocket conn) numBytes
          return $ Just payload
          
    otherwise -> return $ Just firstChunk