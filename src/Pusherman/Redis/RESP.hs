{-# LANGUAGE OverloadedStrings #-}

module Pusherman.Redis.RESP
(
  RESP(..),
  readRESP
)

import Data.Maybe
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

data RESP = RedisSimpleString B.ByteString
          | RedisBulkString B.ByteString
          | RedisError String
          | RedisArray [RESP]
          | RedisInteger Integer

-- FIXME: look for \r\n, not just \r
readTillCRLF :: (Int -> IO B.ByteString) -> IO B.ByteString
readTillCRLF r = r 1 >>= f
  where f "\r" = r 1 >> return ""
        f c = return $ c <> readTillCRLF r

readRESP :: (Int -> IO B.ByteString) -> IO (Maybe RESP)
readRESP r = do
  code <- r 1
  case code of
    '-' -> fmap RedisError <$> rerror
    '+' -> fmap RedisSimpleString <$> rsimple
    ':' -> fmap RedisInteger <$> rinteger
    '$' -> fmap RedisBulkString <$> rbulk
    '*' -> fmap RedisArray <$> rarray
    _ -> return Nothing
  where
    rerror = Just . B.pack <$> readTillCRLF r
    rsimple = Just <$> readTillCRLF r
    rinteger = fmap (encode . fst) . BC.readInt <$> readTilLCRLF r
    rbulk = Just <$> ((rinteger >>= r) <* readTillCRLF r)
    rarray = rinteger >>= flip replicateM (readRESP r) . fromMaybe 0
