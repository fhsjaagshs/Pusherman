{-# LANGUAGE DeriveGeneric #-}

-- TODO: FOR THE FUTURE
-- 1. Allow for an SSL context to be used multiple times,
--    decreasing the number of times the keypair must be read
-- 2. Disable concurrent feedback processing when
--    there is no place to put it (log, webhook)
-- 3. Rewrite feedback processing so that it's not
--    a constant loop and waits between hits.
-- 4. Port Text.JSON to use Aeson

import qualified Push
import qualified Zedis

import Data.Maybe
import GHC.Generics
import Control.Concurrent

--import qualified Text.JSON as JSON
import qualified Data.Aeson as Aeson

import System.IO-- as SystemIO
import System.Environment as Environment

import qualified Data.Time.Clock.POSIX as Clock

import qualified Data.Vector as Vector

import Data.Text as Text
import Data.Text.Encoding
import qualified Data.Text.IO as Text.IO

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS

import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as BL

import Network.HTTP.Client.Conduit

data Config = Config {
  certificate :: !FilePath,
  key :: !FilePath,
  redisServer :: !String,
  redisQueue :: !String,
  notifLogFile :: Maybe FilePath,
  feedbackLogFile :: Maybe FilePath,
  webhook :: Maybe String
} deriving (Show,Generic)

instance Aeson.FromJSON Config

-- Payload construction

-- buildAPSBody :: [(String, JSON.JSValue)] -> [(String, JSON.JSValue)]
-- buildAPSBody [] = []
-- buildAPSBody (x:xs)
--   | (fst x) == "tokens" = (buildAPSBody xs)
--   | (fst x) == "data" = (buildAPSBody xs)
--   | otherwise = case (snd x) of
--                   JSON.JSObject o -> Prelude.foldr (:) (JSON.fromJSObject o) (buildAPSBody xs)
--                   otherwise -> x:(buildAPSBody xs)
--
-- buildPushPayload :: String -> Maybe String
-- buildPushPayload json = case JSON.decode json of
--                             JSON.Error e -> Nothing
--                             JSON.Ok (JSON.JSObject a) -> case JSON.valFromObj "data" a of
--                                                           JSON.Error err -> Just $ JSON.encode $ JSON.toJSObject $ [("aps", JSON.JSObject (JSON.toJSObject (buildAPSBody (JSON.fromJSObject a))))]
--                                                           JSON.Ok d -> Just $ JSON.encode $ JSON.toJSObject $ ("aps", JSON.JSObject (JSON.toJSObject (buildAPSBody (JSON.fromJSObject a)))):(JSON.fromJSObject d)
--                             otherwise -> Nothing
--
-- getTokens :: String -> Maybe [String]
-- getTokens json = case JSON.decode json of
--                   JSON.Error e -> Nothing
--                   JSON.Ok (JSON.JSObject a) -> case JSON.valFromObj "tokens" a of
--                                                 JSON.Error e -> Nothing
--                                                 JSON.Ok a -> Just $ Prelude.map JSON.fromJSString a

-- Gets a payload from Redis
-- loadPayload :: RedisConnection -> String -> IO (Maybe String)
-- loadPayload redisconn queue = runRedis redisconn $ do
--   eitherres <- brpop [BS.pack queue] 0
--   case eitherres of
--       Left errormsg -> return Nothing
--       Right res -> case res of
--                    Nothing -> return Nothing
--                    Just value -> return $ Just $ BS.unpack $ snd value

processPayload :: Aeson.Object -> Maybe Aeson.Object
processPayload parsed = case HM.lookup (Text.pack "data") parsed of
                          Nothing -> Nothing
                          Just (Aeson.Object dataObj) -> Just $ HM.insert (Text.pack "aps") (Aeson.Object (HM.delete (Text.pack "data") (HM.delete (Text.pack "tokens") parsed))) dataObj

generatePayload :: BS.ByteString -> Maybe ([BS.ByteString], BS.ByteString)
generatePayload json = do
  case Aeson.decodeStrict json of
    Nothing -> Nothing
    Just (Aeson.Object parsed) -> case HM.lookup (Text.pack "tokens") parsed of
                                    Nothing -> Nothing
                                    Just (Aeson.Array tokensArray) -> Just (
                                                            Prelude.map (BS.concat . BL.toChunks . BL.init . BL.tail . Aeson.encode) (Vector.toList $ tokensArray),-- Prelude.map Data.Text.Encoding.encodeUtf8 (toList tokensArray),
                                                            BS.concat . BL.toChunks $ Aeson.encode $ fromJust $ processPayload parsed
                                                      )

-- APNS Notifications

sendPush :: Config -> B.ByteString -> B.ByteString -> IO ()
sendPush config json token = do
  case (notifLogFile config) of
    Nothing -> return ()
    Just fp -> writeLog fp ((BS.unpack token) ++ "\t" ++ (BS.unpack json))
    
  Push.sendAPNS (certificate config) (key config) (BS.unpack token) (Data.Text.Encoding.decodeUtf8 json)

listener :: Config -> Zedis.RedisConnection -> IO ()
listener config redisconn = do
  fromRedis <- Zedis.brpop redisconn (redisQueue config)
  
  print $ fromJust $ fromRedis
  
  case fromRedis of
    Nothing -> putStrLn "Failed to load notification payload from Redis."
    Just res -> case generatePayload res of
                  Nothing -> putStrLn "Failed to generate APNS payload."
                  Just payload -> mapM_ (sendPush config (snd payload)) (fst payload)
    
    
  
  listener config redisconn
  
-- Webhook
  
callWebhook :: String -> String -> Integer -> IO ()
callWebhook url token timestamp = do
  initReq <- parseUrl url
  let req = (flip urlEncodedBody) initReq { method = BS.pack "POST" } $ [(BS.pack "token", BS.pack token), (BS.pack "timestamp", BS.pack (show timestamp))]
  withManager $ httpNoBody req
  return ()
  
-- APNS Feedback
  
feedbackListener :: Config -> IO ()
feedbackListener config = do
  Push.readFeedback (certificate config) (key config) (processFeedback config)
  feedbackListener config
  
processFeedback :: Config -> (Integer, String) -> IO ()
processFeedback cnf (timestamp, token) = do
  
  case (feedbackLogFile cnf) of
    Nothing -> return ()
    Just fp -> writeLog fp ((show timestamp) ++ "\t" ++ token)
    
  case (webhook cnf) of
    Nothing -> return ()
    Just url -> callWebhook url token timestamp
  
-- Logging
  
writeLog :: FilePath -> String -> IO ()
writeLog filepath contents = do
  h <- System.IO.openFile filepath AppendMode
  Text.IO.hPutStrLn h (Text.pack contents)
  System.IO.hClose h
  
-- Config reading
  
readConfigFile :: IO BS.ByteString
readConfigFile = do
  fileP <- getConfigFile
  h <- openFile fileP ReadMode
  hSeek h AbsoluteSeek 0
  c <- BS.hGetContents h
  hClose h
  return c
  
getConfigFile :: IO FilePath
getConfigFile = do
  args <- Environment.getArgs
  if (Prelude.length args) == 0 then return "config.json"
  else return $ Prelude.head args
  
-- Main
  
main :: IO ()
main = do
  s <- readConfigFile
  case Aeson.eitherDecodeStrict s of
    Left errormsg -> System.IO.putStrLn $ "Invalid configuration: " ++ errormsg
    Right cnf -> do
      forkIO $ feedbackListener cnf
      redisconn <- Zedis.connectRedis (redisServer cnf)
      System.IO.putStrLn $ "Connected to Redis @ " ++ (redisServer cnf) ++ " on list '" ++ (redisQueue cnf) ++ "'"
      listener cnf redisconn
