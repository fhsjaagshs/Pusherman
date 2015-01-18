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
import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Applicative

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Lens as Aeson.Lens

import System.IO
import System.Environment as Environment

import Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.IO as Text.IO

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS

import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as BL

import Network.HTTP.Client.Conduit

import OpenSSL
import OpenSSL.Session

data Config = Config {
  certificate :: !FilePath,
  key :: !FilePath,
  redisHost :: !String,
  redisList :: !String,
  pushLog :: Maybe FilePath,
  feedbackLog :: Maybe FilePath,
  errorLog :: Maybe FilePath,
  webhook :: Maybe String
}

createSSLContext :: String -> String -> IO SSLContext
createSSLContext cerFile keyFile = withOpenSSL $ do
  ssl <- OpenSSL.Session.context
  OpenSSL.Session.contextSetPrivateKeyFile ssl keyFile
  OpenSSL.Session.contextSetCertificateFile ssl cerFile
  OpenSSL.Session.contextSetDefaultCiphers ssl
  OpenSSL.Session.contextSetVerificationMode ssl OpenSSL.Session.VerifyNone
  return ssl
  
instance Aeson.FromJSON Config where
  parseJSON (Aeson.Object v) = Config <$>
                                 v Aeson..:  (Text.pack "certificate") <*>
                                 v Aeson..:  (Text.pack "key") <*>
                                 v Aeson..:  (Text.pack "redis_host") <*>
                                 v Aeson..:  (Text.pack "redis_list") <*>
                                 v Aeson..:? (Text.pack "push_log") <*>
                                 v Aeson..:? (Text.pack "feedback_log") <*>
                                 v Aeson..:? (Text.pack "error_log") <*>
                                 v Aeson..:? (Text.pack "webhook")
                                 
  parseJSON _ = mzero

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

sendPush :: SSLContext -> Maybe FilePath -> B.ByteString -> B.ByteString -> IO ()
sendPush ssl maybeLogFile json token = do
  case maybeLogFile of
    Nothing -> BS.putStrLn (BS.append (BS.append token (BS.pack "\t")) json)
    Just fp -> writeLog fp (BS.append (BS.append token (BS.pack "\t")) json)
    
  Push.sendAPNS ssl (BS.unpack token) (Text.Encoding.decodeUtf8 json)

listener :: SSLContext -> Maybe FilePath -> Maybe FilePath -> Zedis.RedisConnection -> String -> IO ()
listener ssl maybeLogFile maybeErrorLog redisconn redisQueue = do
  fromRedis <- Zedis.brpop redisconn redisQueue

  case fromRedis of
    Nothing -> case maybeErrorLog of
                Nothing -> putStrLn "error\tFailed to load notification from Redis."
                Just fp -> writeLog fp (BS.pack "Failed to load notification from Redis.")
    Just res -> case generatePayload res of
                  Nothing -> case maybeErrorLog of
                                Nothing -> BS.putStrLn $ (BS.append (BS.pack "error\tFailed to generate APNS payload\t") res)
                                Just fp -> writeLog fp (BS.append (BS.pack "Failed to generate APNS payload\t") res)
                  Just payload -> mapM_ (sendPush ssl maybeLogFile (snd payload)) (fst payload) -- ([tokens],payload)

  listener ssl maybeLogFile maybeErrorLog redisconn redisQueue
  
-- Webhook
  
callWebhook :: String -> String -> Integer -> IO ()
callWebhook url token timestamp = do
  initReq <- parseUrl url
  let req = (flip urlEncodedBody) initReq { method = BS.pack "POST" } $ [(BS.pack "token", BS.pack token), (BS.pack "timestamp", BS.pack (show timestamp))]
  withManager $ httpNoBody req
  return ()
  
-- APNS Feedback

feedbackListener :: SSLContext -> Maybe FilePath -> Maybe String -> IO ()
feedbackListener ssl maybeFeedbackLog maybeWebhook = do
  Push.readFeedback ssl (processFeedback maybeFeedbackLog maybeWebhook)
  feedbackListener ssl maybeFeedbackLog maybeWebhook

-- called by Push.readFeedback
processFeedback :: Maybe FilePath -> Maybe String -> (Integer, String) -> IO ()
processFeedback maybeFeedbackLog maybeWebhookUrl (timestamp, token) = do
  case maybeFeedbackLog of
    Nothing -> putStrLn $ "feedback\t" ++ (show timestamp) ++ "\t" ++ token
    Just fp -> writeLog fp (BS.pack ((show timestamp) ++ "\t" ++ token))
    
  case maybeWebhookUrl of
    Nothing -> return ()
    Just url -> callWebhook url token timestamp
  
-- Logging
  
writeLog :: FilePath -> B.ByteString -> IO ()
writeLog filepath contents = do
  h <- System.IO.openFile filepath AppendMode
  Text.IO.hPutStrLn h (Text.Encoding.decodeUtf8 contents)
  System.IO.hFlush h
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
      ssl <- createSSLContext (certificate cnf) (key cnf)
      redisconn <- Zedis.connectRedis (redisHost cnf)
      
      forkIO $ feedbackListener ssl (feedbackLog cnf) (webhook cnf)
      
      System.IO.putStrLn $ "connected\t" ++ (redisHost cnf) ++ "\t" ++ (redisList cnf)
      
      listener ssl (pushLog cnf) (errorLog cnf) redisconn (redisList cnf)
