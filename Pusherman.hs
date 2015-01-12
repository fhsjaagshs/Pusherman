{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Push

import Data.Aeson
import Data.Functor
import Data.Maybe
import qualified Data.Text as TXT
import qualified Data.HashSet as HST
import qualified Data.ByteString.Char8 as BS

import GHC.Generics

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Database.Redis

-- generatePayloadJSON :: String -> String -> Integer -> JSObject (JSValue)
-- generatePayloadJSON alert sound badge =
--

-- getJSONWithMessage :: String -> JSObject (JSValue)
-- getJSONWithMessage msg =
--   let jmsg = JSString (toJSString msg) in
--   toJSObject [("aps", JSObject (toJSObject [
--                 ("alert", JSString (toJSString msg)),
--                 ("sound", JSString (toJSString badge)),
--                 ("badge", )
--               ]))]

data Config = Config { 
  certificate :: !FilePath,
  key :: !FilePath,
  redisServer :: !String, -- I know how slow these haskell Strings are...
  redisQueue :: !String
} deriving (Show,Generic)
  
data Notification = Notification {
  tokens :: ![String],
  badge :: Integer,
  alert :: !String,
  sound :: String
} deriving (Show,Generic)

instance FromJSON Config
instance FromJSON Notification

-- parseFeedback :: SSL -> Get [(B.ByteString, B.ByteString)] -- (timestamp, token)
-- parseFeedback ssl = do
--   OpenSSL.Session.connect sslsocket
--   timestampBytes <- OpenSSL.Session.read sslsocket 4
--   tokenLengthBytes <- OpenSSL.Session.read sslsocket 2
--   -- TODO: load token
--   OpenSSL.Session.shutdown sslsocket Unidirectional
  
  
  -- | (B.length feedback < 6) = fail "Invalid feedback: too short"
  -- | (B.length feedback > 255) = fail "Invalid feedback: too long"
  -- | otherwise = do
  --   timestamp <- getWord32be
  --   tokenLength <- getWord16be
  --   if
  --   return $ (fromIntegral timestamp, token)

-- transforms the JSON from Redis to an APNS payload
processPayload :: String -> [(String, String)]
processPayload input = [("0805ab2e45ceddbbb5b83597a1f45fddd93708e1d107de34d5a3e71b6434232a", input)]

-- Gets a payload from Redis
loadPayload :: Connection -> String -> IO (Maybe String)
loadPayload redisconn queue = runRedis redisconn $ do
  res <- blpop [(BS.pack queue)] 0
  case res of
    Left r -> return Nothing
    Right r -> case r of
                 Nothing -> return Nothing
                 Just value -> return $ Just (BS.unpack(snd value))

-- Pattern matching wrapper
-- TODO: retrieve status
sendPush :: Config -> (String, String) -> IO ()
sendPush config (token, payload) = Push.sendAPNS (certificate config) (key config) token payload

-- TODO: retrieve status
listener :: Config -> Connection -> IO ()
listener config redisconn = do
  payload <- loadPayload redisconn (redisQueue config)
  
  case payload of
    Nothing -> putStrLn "Failed to load notification payload from Redis."
    Just p -> mapM_ (sendPush config) (processPayload p)
  
  listener config redisconn
  
main :: IO ()
main = do
  -- TODO: Implement command line args
  s <- BS.readFile "config.json"
  case Data.Aeson.decodeStrict s of
    Nothing -> putStrLn "Invalid configuration."
    Just cnf -> do
      putStrLn ("Connected to Redis @ " ++ (redisServer cnf) ++ " on queue '" ++ (redisQueue cnf) ++ "'")
      redisconn <- connect $ defaultConnectInfo { connectHost = (redisServer cnf) }
      listener cnf redisconn
