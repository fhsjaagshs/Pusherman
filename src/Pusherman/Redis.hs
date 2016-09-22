{-# LANGUAGE OverloadedStrings #-}

module Redis
(
  Redis(..),
  open,
  close,
  brpop
)

import Data.Binary
import Network.BSD
import Network.Socket
import Network.Socket.ByteString as SBS
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

data Redis = RedisOffline {
               redisHost :: B.ByteString,
               redisPort :: Int
             }
           | RedisOnline Socket

open :: Redis -> IO ()
open (RedisOffline h p) = do
  sock <- socket AF_INET Stream =<< getProtocolNumber "tcp"
  setSocketOption sock KeepAlive 1
  connect sock . SockAddrInet p . hostAddress =<< getHostByName h
  return $ RedisOnline sock
open r = return r

close :: Redis -> IO ()
close (RedisOnline s) = shutdown s ShutdownBoth
close _ = return () 

brpop :: ByteString -> Redis -> IO (Maybe RESP)
brpop _ (RedisOffline _ _) = return Nothing
brpop q (RedisOnline s) = SBS.send s cmd >> (fmap removekey $ join $ fmap readRESP $ SBS.recv s)
  where
    -- TODO: sanitize @q@@
    cmd = "*3\r\n$5\r\nbrpop\r\n$"
       <> (BL.toStrict $ encode $ B.length q)
       <> "\r\n"
       <> q
       <> "\r\n$1\r\n0\r\n"
   removekey (RedisArray [_,v]) = RedisBulkString v
   removekey s = s
