{-# LANGUAGE OverloadedStrings #-}

module Redis
(
  Redis(..)
)

import Data.Binary
import Network.BSD
import Network.Socket
import Network.Socket.ByteString as SBS
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

import Pusherman.QueueFace

data Redis = RedisOffline {
               redisHost :: B.ByteString,
               redisPort :: Int
             }
           | RedisOnline Socket

instance QueueFace Redis ByteString where
  open (RedisOffline h p) = do
    sock <- socket AF_INET Stream =<< getProtocolNumber "tcp"
    setSocketOption sock KeepAlive 1
    S.connect sock . SockAddrInet p . hostAddress =<< getHostByName h
    return $ RedisOnline sock
  open z = z
  close (RedisOnline s) = shutdown s ShutdownBoth
  close _ = return ()
  brpop _ (RedisOffline _ _) = return Nothing
  brpop q (RedisOnline s) = SBS.send s command >> (readRESP (SBS.recv sock)  >>= (>>= go))
    where
      command = "*3\r\n$5\r\nbrpop\r\n$"
        <> (BL.toStrict $ encode $ B.length q)
        <> "\r\n"
        <> q
        <> "\r\n$1\r\n0\r\n"
      go resp = return ()
      -- TODO: implement BRPOP
