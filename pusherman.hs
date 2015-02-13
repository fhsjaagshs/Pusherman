-- TODO: FOR THE FUTURE
-- 1. Rewrite feedback processing so that it's not
--    a constant loop and waits between hits.
-- 2. Notification throttling
-- 3. Port webhook calling code to Data.HTTP.Client

import qualified Push
import qualified Zedis

import Data.Maybe
import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Applicative

import Network.HTTP.Client.Conduit

import OpenSSL
import OpenSSL.Session

import System.IO
import System.Environment

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Lens as Aeson.Lens

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.Text.Encoding as T.Encoding
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS

import qualified Data.HashMap.Strict as HM

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
  
instance Aeson.FromJSON Config where
  parseJSON (Aeson.Object v) = Config <$>
                                 v Aeson..:  (T.pack "certificate") <*>
                                 v Aeson..:  (T.pack "key") <*>
                                 v Aeson..:  (T.pack "redis_host") <*>
                                 v Aeson..:  (T.pack "redis_list") <*>
                                 v Aeson..:? (T.pack "push_log") <*>
                                 v Aeson..:? (T.pack "feedback_log") <*>
                                 v Aeson..:? (T.pack "error_log") <*>
                                 v Aeson..:? (T.pack "webhook")
  parseJSON _ = mzero

processPayload :: Aeson.Object -> Maybe Aeson.Object
processPayload parsed = case HM.lookup (T.pack "data") parsed of
                          Nothing -> Nothing
                          Just (Aeson.Object dataObj) -> Just $ HM.insert (T.pack "aps") (Aeson.Object (HM.delete (T.pack "data") (HM.delete (T.pack "tokens") parsed))) dataObj

getTokens :: Maybe Aeson.Value -> [T.Text]
getTokens decoded = decoded & catMaybes . toListOf (Aeson.Lens.key (T.pack "tokens") . Aeson.Lens.traverseArray) :: [T.Text]

generatePayload :: BS.ByteString -> Maybe ([BS.ByteString], BS.ByteString)
generatePayload json = do
  let parsed = Aeson.decodeStrict json
  case parsed of
    Nothing -> Nothing
    Just (Aeson.Object parsedObject) -> Just $ (Prelude.map T.Encoding.encodeUtf8 (getTokens parsed), BL.toStrict $ Aeson.encode $ fromJust $ processPayload $ parsedObject)

-- APNS Notifications

sendPush :: SSLContext -> Maybe FilePath -> Maybe FilePath -> BS.ByteString -> BS.ByteString -> IO ()
sendPush ssl maybeLogFile maybeErrorLog json token = do
  case maybeLogFile of
    Nothing -> BS.putStrLn (BS.append (BS.append token (BS.pack "\t")) json)
    Just fp -> writeLog fp (BS.append (BS.append token (BS.pack "\t")) json)
    
  if BS.length json > 1900
  then 
    case maybeErrorLog of
      Nothing -> BS.putStrLn (BS.append (BS.pack "paylod too long\t") json)
      Just fp -> writeLog fp (BS.append (BS.pack "paylod too long\t") json)
  else
    if BS.length token /= 64
    then
      case maybeErrorLog of
        Nothing -> BS.putStrLn (BS.append (BS.pack "invalid token\t") token)
        Just fp -> writeLog fp (BS.append (BS.pack "invalid token\t") token)
    else
      Push.sendAPNS ssl token (T.Encoding.decodeUtf8 json)

listener :: SSLContext -> Maybe FilePath -> Maybe FilePath -> Zedis.RedisConnection -> String -> IO ()
listener ssl maybeLogFile maybeErrorLog redisconn redisQueue = do
  fromRedis <- Zedis.brpop redisconn redisQueue

  case fromRedis of
    Nothing -> case maybeErrorLog of
                Nothing -> putStrLn "error\tFailed to load notification from Redis."
                Just fp -> writeLog fp (BS.pack "Failed to load notification from Redis.")
    Just res -> case generatePayload res of -- ([tokens],payload)
                  Nothing -> case maybeErrorLog of
                                Nothing -> BS.putStrLn $ (BS.append (BS.pack "error\tFailed to generate APNS payload\t") res)
                                Just fp -> writeLog fp (BS.append (BS.pack "Failed to generate APNS payload\t") res)
                  Just payload -> mapM_ (sendPush ssl maybeLogFile maybeErrorLog (snd payload)) (fst payload)

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
  
writeLog :: FilePath -> BS.ByteString -> IO ()
writeLog filepath contents = withFile filepath AppendMode $ \h -> do
  T.IO.hPutStrLn h (T.Encoding.decodeUtf8 contents)
  hFlush h
  
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
  args <- getArgs
  if (Prelude.length args) == 0 then return "config.json"
  else return $ Prelude.head args
  
-- SSL 

createSSLContext :: String -> String -> IO SSLContext
createSSLContext cerFile keyFile = withOpenSSL $ do
  ssl <- OpenSSL.Session.context
  OpenSSL.Session.contextSetPrivateKeyFile ssl keyFile
  OpenSSL.Session.contextSetCertificateFile ssl cerFile
  OpenSSL.Session.contextSetDefaultCiphers ssl
  OpenSSL.Session.contextSetVerificationMode ssl OpenSSL.Session.VerifyNone
  return ssl
  
-- Main
  
main :: IO ()
main = do
  s <- readConfigFile
  case Aeson.eitherDecodeStrict s of
    Left errormsg -> System.IO.putStrLn $ "Invalid configuration: " ++ errormsg
    Right cnf -> do
      ssl <- createSSLContext (certificate cnf) (key cnf)
      redisconn <- Zedis.connectRedis (redisHost cnf)

      -- forkIO $ feedbackListener ssl (feedbackLog cnf) (webhook cnf)
      
      System.IO.putStrLn $ "connected\t" ++ (redisHost cnf) ++ "\t" ++ (redisList cnf)
      
      listener ssl (pushLog cnf) (errorLog cnf) redisconn (redisList cnf)
