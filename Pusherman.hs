{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import qualified Push

import Data.Maybe
import GHC.Generics
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import qualified Text.JSON as JSON
import qualified Data.Time.Clock.POSIX as Clock
import qualified Data.Aeson as Aeson
import qualified Data.Text as TXT
import qualified Data.ByteString.Char8 as BS

import Database.Redis as Redis

data Config = Config { 
  certificate :: !FilePath,
  key :: !FilePath,
  redisServer :: !String, -- I know how slow these haskell Strings are...
  redisQueue :: !String,
  notifLogFile :: !FilePath,
  feedbackLogFile :: !FilePath
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
  BS.appendFile (notifLogFile config) (BS.pack (json ++ "hello!"))
  Push.sendAPNS (certificate config) (key config) token json

listener :: Config -> Connection -> IO ()
listener config redisconn = do
  fromRedis <- loadPayload redisconn (redisQueue config)
  
  case fromRedis of
    Nothing -> putStrLn "Failed to load notification payload from Redis."
    Just res -> mapM_ (sendPush config (fromJust (buildPushPayload res))) (fromJust (getTokens res))
  
  listener config redisconn
  
processFeedback :: Config -> (Integer, BS.ByteString) -> IO ()
processFeedback cnf (timestamp, token) = do
  liftIO $ BS.appendFile (feedbackLogFile cnf) (BS.pack ((show timestamp) ++ "," ++ (show token) ++ "\t")
  
main :: IO ()
main = do
  -- TODO: Implement command line args
  s <- BS.readFile "config.json"
  case Aeson.decodeStrict s of
    Nothing -> putStrLn "Invalid configuration."
    Just cnf -> do
      putStrLn ("Connected to Redis @ " ++ (redisServer cnf) ++ " on queue '" ++ (redisQueue cnf) ++ "'")
      redisconn <- connect $ defaultConnectInfo { connectHost = (redisServer cnf) }
      forkIO $ do 
        Push.readFeedback (certificate cnf) (key conf) (processFeedback cnf)
        readFeedback :: FilePath -> FilePath -> ((B.ByteString, B.ByteString) -> ()) -> IO ()
        readFeedback certificateFile keyFile callback = withOpenSSL $ do
      putStrLn "After thingy"
      listener cnf redisconn
