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

--import OpenSSL
--import OpenSSL.Session

import Network.TLS
import Crypto.Random.AESCtr (makeSystem)

import System.IO
import System.Environment

import qualified Data.Aeson as A
import qualified Data.Aeson.Lens as A.Lens

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.Text.Encoding as T.Encoding
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS

import qualified Data.HashMap.Strict as HM

newtype PushermanT m a = PushermanT { runPushermantT :: ReaderT (Config,RedisConnection,Context) m a }

redis :: Monad m => PushermanT m RedisConnection
redis = do
  (_,c,_) <- ask
  return c

config :: Monad m => PushermanT m Config
config = do
  (c,_,_) <- ask
  return c

context :: Monad m => PushermanT m Context
context = do
  (_,_,c) <- ask
  return c

writeError :: MonadIO m => String -> PushermanT m ()
writeError err = config >>= maybe writeStderr writeToFile . errorLog
  where
    writeStderr = hPutStrLn stderr err
    writeToFile fp = withFile fp AppendMode $ \h -> do
      hPutStrLn h err
      hFlush h

writeLog :: MonadIO m => String -> PushermanT m ()
writeLog out = config >>= maybe writeStdout writeToFile . pushLog
  where
    writeStdout = hPutStrLn stdout out
    writeToFile fp = withFile fp AppendMode $ \h -> do
      hPutStrLn h err
      hFlush h

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

instance A.FromJSON Config where
  parseJSON (A.Object v) = Config <$>
                                 v A..:  (T.pack "certificate") <*>
                                 v A..:  (T.pack "key") <*>
                                 v A..:  (T.pack "redis_host") <*>
                                 v A..:  (T.pack "redis_list") <*>
                                 v A..:? (T.pack "push_log") <*>
                                 v A..:? (T.pack "feedback_log") <*>
                                 v A..:? (T.pack "error_log") <*>
                                 v A..:? (T.pack "webhook")
  parseJSON _ = mzero

processPayload :: A.Object -> Maybe A.Object
processPayload parsed = case HM.lookup (T.pack "data") parsed of
                          Nothing -> Nothing
                          Just (A.Object dataObj) -> Just $ HM.insert (T.pack "aps") (A.Object (HM.delete (T.pack "data") (HM.delete (T.pack "tokens") parsed))) dataObj

getTokens :: Maybe A.Value -> [T.Text]
getTokens decoded = decoded & catMaybes . toListOf (A.Lens.key (T.pack "tokens") . A.Lens.traverseArray) :: [T.Text]

generatePayload :: BS.ByteString -> Maybe ([BS.ByteString], BS.ByteString)
generatePayload json = do
  let parsed = A.decodeStrict json
  case parsed of
    Nothing -> Nothing
    Just (A.Object parsedObject) -> Just $ (Prelude.map T.Encoding.encodeUtf8 (getTokens parsed), BL.toStrict $ A.encode $ fromJust $ processPayload $ parsedObject)

-- APNS Notifications
  
-- Webhook

callWebhook :: String -> String -> Integer -> IO ()
callWebhook url token timestamp = do
  initReq <- parseUrl url
  let req = (flip urlEncodedBody) initReq { method = BS.pack "POST" } $ [(BS.pack "token", BS.pack token), (BS.pack "timestamp", BS.pack (show timestamp))]
  withManager $ httpNoBody req
  return ()

-- called by Push.readFeedback
processFeedback :: Maybe FilePath -> Maybe String -> (Integer, String) -> IO ()
processFeedback maybeFeedbackLog maybeWebhookUrl (timestamp, token) = do
  writeLog maybeFeedbackLog (BS.pack ("feedback\t" ++ (show timestamp) ++ "\t" ++ token))
  case maybeWebhookUrl of
    Nothing -> return ()
    Just url -> callWebhook url token timestamp
  
-- Logging
  
-- SSL 

--createSSLContext :: String -> String -> IO SSLContext
--createSSLContext cerFile keyFile = withOpenSSL $ do
--  ssl <- OpenSSL.Session.context
--  OpenSSL.Session.contextSetPrivateKeyFile ssl keyFile
--  OpenSSL.Session.contextSetCertificateFile ssl cerFile
--  OpenSSL.Session.contextSetDefaultCiphers ssl
--  OpenSSL.Session.contextSetVerificationMode ssl OpenSSL.Session.VerifyNone
--  return ssl
  

-- Main Action

feedback :: MonadIO m => PushermanT m ()
feedback = do
  c <- config
  Push.readFeedback ssl (processFeedback (feedbackLog c) (webhook c))

pusher :: MonadIO m => PushermanT m ()
pusher = do
  c <- config
  redisconn <- redis
  fromRedis <- Zedis.brpop redisconn (redisList c)

  case fromRedis of
    Nothing -> writeError "error\tFailed to load notification from Redis."
    Just res -> case generatePayload res of
                  Nothing -> writeError $ "error\tFailed to generate APNS payload\t" ++ (BS.unpack res)
                  Just (tokens,payload) -> do
                    if BS.length json > 1900
                       then writeError $ "error\tpayload too long\t" ++ (BS.unpack json)
                       else mapM_ (sendPush payload) tokens
  where
    sendPush json token = do
      writeLog $ token <> (BS.pack "\t") <> json
      
      if BS.length token /= 64
        then writeError $ "error\tinvalid token\t" ++ (BS.unpack token)
        else do
          let ssl = undefined -- TODO: implement this
          Push.sendAPNS ssl token (T.Encoding.decodeUtf8 json)

-- Config reading

readConfig :: IO (Either String Config)
readConfig = do
  json <- BS.readFile fromMaybe "config.json" . listToMaybe =<< getArgs
  return $ A.eitherDecode json
-- Main

main :: IO ()
main = readConfig >>= either (putStrLn . mappend "Invalid configuration: ") run
  where
    run cnf = do
      ecreds <- credentialLoadX509 (certificate cnf) (key cnf)
      case ecreds of
        Left err -> putStrLn $ "Invalid certificate: " ++ err
        Right creds -> do
          redisconn <- Zedis.connectRedis (redisHost cnf)
          -- TODO: handshake this using creds
          let context = Nothing
          let state = (cnf,redisconn,context)
          forkIO $ forever $ runReaderT (runPushermantT feedback) state
          putStrLn $ "connected\t" ++ (redisHost cnf) ++ "\t" ++ (redisList cnf)
          forever $ runReaderT (runPushermantT pusher) state


sslHttpConnect :: ConnOpts -> IO ByteString
sslHttpConnect c = do
    gen <- makeSystem
    let params = defaultParams
               { pCiphers = ciphersuite_all
               , onCertificatesRecv = certificateChecks
                     [ certificateVerifyChain
                     , return . certificateVerifyDomain (cDomain c)
                     ]
               }
        (host,url) = break (=='/') $ cUrl c
        headers = [ ("Host", L.pack host)
                  , ("Connection", "closed") ] ++ cHeaders c
    ctx <- connectionClient (cDomain c) (cPort c) params gen
    let httpPkt = cMethod c <+> " " <+> L.pack url <+> " HTTP/1.1\r\n"
              <+> L.concat (map (\(h, v) -> h <+> ": " <+> v <+> "\r\n") headers)
              <+> if not . null $ cPayload c
                     then "Content-Length: " <+> L.pack (show . L.length $ payload)
                      <+> "\r\n\r\n" <+> payload
                     else ""
        payload = L.intercalate "&" . map (\(k,v) -> k <+> "=" <+> v) $ cPayload c
        line = "\n" <+> L.replicate 20 '-' <+> "\n"
    L.putStrLn $ "HTTP Packet: " <+> line <+> httpPkt <+> line
    putStrLn "Performing handshake..."
    handshake ctx
    putStrLn "Sending data..."
    sendData ctx httpPkt
    strictBS <- recvData ctx
    return $ Chunk strictBS Empty
