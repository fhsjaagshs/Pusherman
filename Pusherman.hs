{-# LANGUAGE DeriveGeneric #-}

-- TODO: FOR THE FUTURE
-- 1. Allow for an SSL context to be used multiple times,
--    decreasing the number of times the keypair must be read
-- 2. Disable concurrent feedback processing when
--    there is no place to put it (log, webhook)
-- 3. Rewrite feedback processing so that it's not
--    a constant loop and waits between hits.

import qualified Push
import qualified Zedis

import Data.Maybe
import GHC.Generics
import Control.Concurrent
import Control.Lens

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Lens as Aeson.Lens

import System.IO
import System.Environment as Environment

import qualified Data.Time.Clock.POSIX as Clock

import Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
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

processPayload :: Aeson.Object -> Maybe Aeson.Object
processPayload parsed = case HM.lookup (Text.pack "data") parsed of
                          Nothing -> Nothing
                          Just (Aeson.Object dataObj) -> Just $ HM.insert (Text.pack "aps") (Aeson.Object (HM.delete (Text.pack "data") (HM.delete (Text.pack "tokens") parsed))) dataObj

getTokens :: Maybe Aeson.Value -> [Text.Text]
getTokens decoded = decoded & catMaybes . toListOf (Aeson.Lens.key (Text.pack "tokens") . Aeson.Lens.traverseArray) :: [Text.Text]

generatePayload :: BS.ByteString -> Maybe ([BS.ByteString], BS.ByteString)
generatePayload json = do
  let parsed = Aeson.decodeStrict json
  case parsed of
    Nothing -> Nothing
    Just (Aeson.Object parsedObject) -> Just $ (Prelude.map Text.Encoding.encodeUtf8 (getTokens parsed), BL.toStrict $ Aeson.encode $ fromJust $ processPayload $ parsedObject)

-- APNS Notifications

sendPush :: Config -> B.ByteString -> B.ByteString -> IO ()
sendPush config json token = do
  case (notifLogFile config) of
    Nothing -> return ()
    Just fp -> writeLog fp (BS.append (BS.append token (BS.pack "\t")) json)
    
  Push.sendAPNS (certificate config) (key config) (BS.unpack token) (Text.Encoding.decodeUtf8 json)

listener :: Config -> Zedis.RedisConnection -> IO ()
listener config redisconn = do
  fromRedis <- Zedis.brpop redisconn (redisQueue config)

  case fromRedis of
    Nothing -> putStrLn "Failed to load notification payload from Redis."
    Just res -> case generatePayload res of
                  Nothing -> putStrLn "Failed to generate APNS payload."
                  Just payload -> mapM_ (sendPush config (snd payload)) (fst payload) -- ([tokens],payload)
    
    
  
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
    Just fp -> writeLog fp (BS.pack ((show timestamp) ++ "\t" ++ token))
    
  case (webhook cnf) of
    Nothing -> return ()
    Just url -> callWebhook url token timestamp
  
-- Logging
  
writeLog :: FilePath -> B.ByteString -> IO ()
writeLog filepath contents = do
  h <- System.IO.openFile filepath AppendMode
  Text.IO.hPutStrLn h (Text.Encoding.decodeUtf8 contents)
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
