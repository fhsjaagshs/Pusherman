{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Push

import qualified Text.JSON as JSON

import Data.Aeson
import Data.Functor
import Data.Maybe
import qualified Data.Text as TXT
--import qualified Data.HashSet as HST
import qualified Data.ByteString.Char8 as BS

import GHC.Generics

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Database.Redis

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

-- buildAPSBody :: [(String, JSON.JSValue)] -> [(String, JSON.JSValue)]
-- buildAPSBody [] = []
-- buildAPSBody (x:xs)
--   | (fst x) == "tokens" = (buildAPSBody xs)
--   | (fst x) == "data" = (buildAPSBody xs)
--   | otherwise = case (snd x) of
--                         JSON.JSObject o -> Prelude.foldr (:) (JSON.fromJSObject o) (buildAPSBody xs)
--                         otherwise -> x:(buildAPSBody xs)
--
-- buildAPNSPayload :: String -> Maybe String
-- buildAPNSPayload input = case JSON.decode input of
--                             JSON.Ok a -> Just $ "{\"aps\":" ++ (JSON.encode (JSON.toJSObject (buildAPSBody (JSON.fromJSObject a)))) ++ "}"
--                             JSON.Error e -> Nothing

buildAPSBody :: [(String, JSON.JSValue)] -> [(String, JSON.JSValue)]
buildAPSBody [] = []
buildAPSBody (x:xs)
  | (fst x) == "tokens" = (buildAPSBody xs)
  | (fst x) == "data" = (buildAPSBody xs)
  | otherwise = case (snd x) of
                  JSON.JSObject o -> Prelude.foldr (:) (JSON.fromJSObject o) (buildAPSBody xs)
                  otherwise -> x:(buildAPSBody xs)

buildPushPayload :: String -> Maybe String
buildPushPayload json = case JSON.decode json of
                            JSON.Error e -> Nothing
                            JSON.Ok (JSON.JSObject a) -> case JSON.valFromObj "data" a of
                                                          JSON.Error err -> Just $ JSON.encode $ JSON.toJSObject $ [("aps", JSON.JSObject (JSON.toJSObject (buildAPSBody (JSON.fromJSObject a))))]
                                                          JSON.Ok d -> Just $ JSON.encode $ JSON.toJSObject $ ("aps", JSON.JSObject (JSON.toJSObject (buildAPSBody (JSON.fromJSObject a)))):(JSON.fromJSObject d)
                            otherwise -> Nothing
                            
getTokens :: String -> Maybe [String]
getTokens json = case JSON.decode json of
                  JSON.Error e -> Nothing
                  JSON.Ok (JSON.JSObject a) -> case JSON.valFromObj "tokens" a of
                                                JSON.Error e -> Nothing
                                                JSON.Ok a -> Just $ map JSON.fromJSString a

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

-- Gets a payload from Redis
loadPayload :: Connection -> String -> IO (Maybe String)
loadPayload redisconn queue = runRedis redisconn $ do
  res <- brpop [(BS.pack queue)] 0
  case res of
    Left r -> return Nothing
    Right r -> case r of
                 Nothing -> return Nothing
                 Just value -> return $ Just (BS.unpack(snd value))

sendPush :: Config -> String -> String -> IO ()
sendPush config json token = do
  -- TODO: Log this shit
  Push.sendAPNS (certificate config) (key config) token json

-- TODO: retrieve status
listener :: Config -> Connection -> IO ()
listener config redisconn = do
  fromRedis <- loadPayload redisconn (redisQueue config)
  
  case fromRedis of
    Nothing -> putStrLn "Failed to load notification payload from Redis."
    Just res -> mapM_ (sendPush config (fromJust (buildPushPayload res))) (fromJust (getTokens res))
  
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
