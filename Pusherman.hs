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

processJSON :: BS.ByteString -> Maybe APNSMessage
processJSON json = case decodeStrict json of
                      Nothing -> Nothing
                      Just notif -> Just (APNSmessage {
                                            deviceTokens = (HST.fromList (map TXT.pack (notif tokens))),
                                            expiry = Nothing,
                                            alert = (TXT.pack (notif alert)),
                                            badge = (notif badge),
                                            sound = (notif sound)
                                          })

apnsMsg :: Maybe (BS.ByteString, BS.ByteString) -> Maybe APNSmessage
apnsMsg maybeTuple = if isNothing maybeTuple then Nothing else processJSON $ snd $ fromJust maybeTuple

sendAPNSTo :: Config -> BS.ByteString -> IO ()
sendAPNSTo c bs = do
   print bs
   listenRedis c
   
processAPNSResult :: APNSConfig -> IO APNSResult -> IO ()
processAPNSResult config ioResult = do
  result <- ioResult
  
  feedbackAPNS config
  -- TODO: implement

listenPusherman :: Connection -> APNSManager -> IO APNSResult
listenPusherman conn apns = do
  runRedis conn $ do
    res <- blpop [(BS.pack (redisQueue c))] 0
  
    case res of
      Left r -> (liftIO $ putStrLn "Failure to listen.")
      Right r -> case r of
                   Nothing -> liftIO $ putStrLn "Failure to return results"
                   Just value -> sendAPNS apns (apnsMsg value)

main :: IO ()
main = do
  s <- BS.readFile "config.json"
  config <- decodeStrict s

  case config of
    Nothing -> (liftIO $ putStrLn "Invalid configuration.")
    Just cnf -> do
      putStrLn ("Connected to Redis @ " ++ (redisServer cnf) ++ " on queue '" ++ (redisQueue cnf) ++ "'")
      redisconn <- connect $ defaultConnectInfo { connectHost = (redisServer cnf) }
      cert <- fileReadCertificate (certificate cnf)
      key <- fileReadPrivateKey (key cnf)
      
      let apnsConfig = def {
                        apnsCertificate = cert,
                        apnsPrivateKey  = key,
                        environment     = Local
                      }
      
      manager <- startAPNS apnsConfig
      listenPusherman redisconn manager
      closeAPNS manager
      