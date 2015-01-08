{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Aeson
import Data.Text
--import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import GHC.Generics
import Control.Applicative
import Control.Monad

import Database.Redis

data Config = Config { 
  certificate :: !FilePath,
  key :: !FilePath,
  redisServer :: ![Char],
  redisQueue :: ![Char]
} deriving (Show,Generic)
  
data Notification = Notification {
  tokens :: ![Text],
  badge :: Integer,
  alert :: !Text,
  sound :: Text
} deriving (Show,Generic)

instance FromJSON Config
instance FromJSON Notification

sendAPNSTo :: (ByteString, ByteString) -> IO ()
sendAPNSTo (redisQueue, notifJSON) = do
  notif <- decodeStrict notifJSON
  -- TODO: Implement this

connectAndListen :: Config -> IO ()
connectAndListen c = do
  conn <- connect $ defaultConnectInfo { connectHost = (redisServer c) }
  runRedis conn $ do
    --blpop :: RedisCtx m f	 => [ByteString] -> Integer	-> m (f (Maybe (ByteString, ByteString)))
    pair <- blpop [(BS.pack (redisQueue c))] 0
    
    case pair of
      Just p -> sendAPNSTo pair
      Nothing -> putStrLn "Failed to load next notification."
    -- do the deed

main :: IO ()
main = do
  config <- fmap decodeStrict (BS.readFile "config.json")

  case config of
    Nothing -> putStrLn "failure"
    Just cnf -> putStrLn "Redis server: " ++ (redisServer cnf)
  
  case config of
    Nothing -> putStrLn "TODO: Implement cmd line args"--(connectAndListen config)
    Just cnf -> (connectAndListen cnf)
    