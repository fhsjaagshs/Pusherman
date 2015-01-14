{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import qualified Push

import Data.Maybe
import GHC.Generics
import Control.Concurrent

import qualified Text.JSON as JSON
import qualified Data.Aeson as Aeson

import System.Environment

import qualified Data.Time.Clock.POSIX as Clock

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

import Database.Redis as Redis
import Network.HTTP.Client.Conduit

data Config = Config { 
  certificate :: !FilePath,
  key :: !FilePath,
  redisServer :: !String, -- I know how slow these haskell Strings are...
  redisQueue :: !String,
  notifLogFile :: Maybe FilePath,
  feedbackLogFile :: Maybe FilePath,
  webhook :: Maybe String
} deriving (Show,Generic)

instance Aeson.FromJSON Config

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
  case (notifLogFile config) of
    Nothing -> return ()
    Just fp -> BS.appendFile fp (BS.pack (token ++ "\t" ++ json ++ "\n"))
    
  Push.sendAPNS (certificate config) (key config) token json

listener :: Config -> Connection -> IO ()
listener config redisconn = do
  fromRedis <- loadPayload redisconn (redisQueue config)
  
  case fromRedis of
    Nothing -> putStrLn "Failed to load notification payload from Redis."
    Just res -> mapM_ (sendPush config (fromJust (buildPushPayload res))) (fromJust (getTokens res))
  
  listener config redisconn
  
callWebhook :: String -> String -> Integer -> IO ()
callWebhook url token timestamp = do
  initReq <- parseUrl url
  let req = (flip urlEncodedBody) initReq { method = "POST" } $ [("token", BS.pack token), ("timestamp", BS.pack (show timestamp))]
  withManager $ httpNoBody req
  return ()
  
feedbackListener :: Config -> IO ()
feedbackListener config = do
  Push.readFeedback (certificate config) (key config) (processFeedback config)
  feedbackListener config
  
processFeedback :: Config -> (Integer, String) -> IO ()
processFeedback cnf (timestamp, token) = do
  
  case (feedbackLogFile cnf) of
    Nothing -> return ()
    Just fp -> BS.appendFile fp (BS.pack ((show timestamp) ++ "\t" ++ (show token) ++ "\n"))
    
  case (webhook cnf) of
    Nothing -> return ()
    Just url -> callWebhook url token timestamp
  
getConfigFile :: IO String
getConfigFile = do
  args <- getArgs
  if (length args) == 0 then return "config.json"
  else return $ head args
  
main :: IO ()
main = do
  configFile <- getConfigFile
  s <- BS.readFile configFile
  case Aeson.decodeStrict s of
    Nothing -> putStrLn "Invalid configuration."
    Just cnf -> do
      forkIO (feedbackListener cnf)
        
      putStrLn ("Connected to Redis @ " ++ (redisServer cnf) ++ " on queue '" ++ (redisQueue cnf) ++ "'")
      redisconn <- connect $ defaultConnectInfo { connectHost = (redisServer cnf) }
      listener cnf redisconn
