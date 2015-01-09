{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

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

--import Network.PushNotify.APNS
-- import Network.TLS.Extra        (fileReadCertificate,fileReadPrivateKey)

-- for CTRL-C
import Control.Exception as E
import Control.Concurrent
import System.Posix.Signals
import System.Exit

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

processJSON :: BS.ByteString -> Maybe [APNSMessage]
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
   liftIO $ print bs
   listenRedis c
   
listenPusherman :: Connection -> APNSManager -> IO ()
listenPusherman conn apns = do
  runRedis conn $ do
    res <- blpop [(BS.pack (redisQueue c))] 0
  
    case res of
      Left r -> (liftIO $ putStrLn "Failure to listen.")
      Right r -> case r of
                   Nothing -> liftIO $ putStrLn "Failure to return results"
                   Just value -> liftIO $ sendAPNSTo c (snd value)


main :: IO ()
main = do
  s <- BS.readFile "config.json"
  config <- decodeStrict s
  
  let apnsConfig = def {
                    apnsCertificate = cert,
                    apnsPrivateKey  = key,
                    environment     = Local
                  }
  
  case config of
    Nothing -> (liftIO $ putStrLn "Invalid configuration.")
    Just cnf -> do
      putStrLn ("Connected to Redis @ " ++ (redisServer cnf) ++ " on queue '" ++ (redisQueue cnf) ++ "'")
      redisconn <- connect $ defaultConnectInfo { connectHost = (redisServer c) }
      manager <- startAPNS apnsConfig
      listenPusherman redisconn manager
      closeAPNS manager
      