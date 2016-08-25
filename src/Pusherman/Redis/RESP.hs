{-# LANGUAGE OverloadedStrings #-}

module Pusherman.Redis.RESP
(
  RESP(..),
  readRESP
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

data RESP = RedisSimpleString B.ByteString
          | RedisBulkString B.ByteString
          | RedisError String
          | RedisArray [RESP]
          | RedisInteger Integer

readTillCRLF :: (Int -> IO B.ByteString) -> IO B.ByteString
readTillCRLF r = do
  c <- r 1
  if c == "\r" then (r 1) >> return ""
  else return $ c <> readTillCRLF r

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
    rbulk = do
      count <- rinteger
      case count of
        Just (-1) -> Just ""
        Just c -> r c <* readTillCRLF r
    rarray = return [] -- TODO implement me! Pusherman doesn't need to be able to use arrays.
