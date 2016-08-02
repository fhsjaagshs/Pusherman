-- TODO: FOR THE FUTURE
-- 1. Rewrite feedback processing so that it's not
--    a constant loop and waits between hits.
-- 2. Notification throttling
-- 3. Port webhook calling code to Data.HTTP.Client

{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.Text.IO as T {-# SCC "" #-} 
import qualified Data.Text.Encoding as T
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

instance A.FromJSON Config where
  parseJSON (A.Object v) = Config
                             <$> v A..:  (T.pack "certificate")
                             <*> v A..:  (T.pack "key")
                             <*> v A..:  (T.pack "redis_host")
                             <*> v A..:  (T.pack "redis_list")
                             <*> v A..:? (T.pack "push_log")
                             <*> v A..:? (T.pack "feedback_log")
                             <*> v A..:? (T.pack "error_log")
                             <*> v A..:? (T.pack "webhook")
  parseJSON _ = mzero

data State = State {
  stateContext :: Context,
  stateRedisConnection :: RedisConnection,
  stateRedisQueue :: String,
  statePushOutput :: Handle,
  stateErrorOutput :: Handle,
  stateFeedbackHandle :: Handle,
  stateFeedbackWebhook :: Maybe String
}

mkState :: Config -> IO State
mkState (Config c k rh rq pl fl el wh) = State
                                           <$> context
                                           <*> redisConnection
                                           <*> redisQueue
                                           <*> pushOutput
                                           <*> errorOutput
                                           <*> stateFeedbackHandle
                                           <*> stateFeedbackWebhook
  where
    context = return () -- TODO: implement me
    redisConnection = Zedis.connectRedis rh
    redisQueue = return rq
    pushOutput = maybe (return stdout) (\fp -> openFile fp AppendMode) pl
    errorOutput = maybe (return stderr) (\fp -> openFile fp AppendMode) el
    stateFeedbackHandle = maybe (return stdout) (\fp -> openFile fp AppendMode) fl
    stateFeedbackWebhook = return wh

destroyState :: State -> IO ()
destroyState (State ssl r q out err fb _) = return ()

newtype PushermanT m a = PushermanT { runPushermantT :: ReaderT State m a }

redis :: Monad m => PushermanT m RedisConnection
redis = stateRedisConnection <$> lift ask

-- FIXME: should these really be String inputs?

writeError :: MonadIO m => String -> PushermanT m ()
writeError err = do
  eh <- stateErrorOutput <$> lift ask
  liftIO $ do
    hPutStrLn eh err
    hFlush eh

writeLog :: MonadIO m => String -> PushermanT m ()
writeLog out = do
  oh <- statePushOutput <$> lift ask
  liftIO $ do
    hPutStrLn oh out
    hFlush oh

writeFeedback :: MonadIO m => String -> PushermanT m ()
writeFeedback str = do
  fh <- stateFeedbackHandle <$> lift ask
  liftIO $ do
    hPutStrLn fh str
    hFlush fh


--
-- Payload Generation
--

-- Takes JSON from Redis in the format:
-- { "tokens": [], "push_payload": {} }
-- and turns it into a tuple in the form of (tokens,payload)
splitPayloads :: BS.ByteString -> Maybe ([BS.ByteString], BS.ByteString)
splitPayloads json = do
  parsed <- A.decodeStrict json >>= value2hm
  tokens <- HM.lookup "tokens" parsed
  payload <- HM.lookup "payload" parsed
  return (tokens,payload)
  where
    value2hm (A.Object hm) = Just hm
    value2hm _ = Nothing

--
-- Push Handling Logic
--

callWebhook :: MonadIO m => String -> Integer -> PushermanT m ()
callWebhook token timestamp = config >>= maybe (return ()) f . webhook
  where
    f url = do
      initReq <- liftIO $ parseUrl url
      let req = (flip urlEncodedBody) initReq { method = "POST" } $ [("token", BS.pack token), ("timestamp", BS.pack $ show timestamp)]
      void $ liftIO $ withManager $ httpNoBody req
  
-- Main Action

feedback :: MonadIO m => PushermanT m ()
feedback = do
  c <- config
  Push.readFeedback ssl $ \(timestamp, token) -> do
    writeFeedback $ (show timestamp) ++ "\t" ++ token
    callWebhook token timestamp

push :: MonadIO m => PushermanT m ()
push = do
  c <- config
  redisconn <- redis
  fromRedis <- liftIO $ Zedis.brpop redisconn (redisList c)

  case fromRedis of
    Nothing -> writeError "failed to load notification from redis"
    Just res -> case generatePayload res of
      Nothing -> writeError $ "failed to generate apns payload\t" ++ (BS.unpack res)
      Just (tokens,payload) -> if BS.length json > 1900
        then writeError $ "payload too long\t" ++ (BS.unpack json)
        else mapM_ (sendPush payload) tokens
  where
    sendPush json token = do
      if BS.length token /= 64
        then writeError $ "invalid token\t" ++ (BS.unpack token)
        else do
          writeLog $ token <> "\t" <> json
          let ssl = undefined -- TODO: implement this
          liftIO $ Push.sendAPNS ssl token (T.decodeUtf8 json)

-- Config reading

-- TODO: catch file reading errors
readConfig :: IO (Either String Config)
readConfig = A.eitherDecode =<< BS.readFile . fromMaybe "config.json" . listToMaybe =<< getArgs
  
-- Main

main :: IO ()
main = readConfig >>= either (hPutStrLn stderr . mappend "invalid configuration: ") run
  where
    run cnf = do
      ecreds <- credentialLoadX509 (certificate cnf) (key cnf)
      case ecreds of
        Left err -> hPutStrLn stderr $ "invalid certificate: " ++ err
        Right creds -> do
          redisconn <- Zedis.connectRedis (redisHost cnf)
          -- TODO: handshake this using creds
          let context = Nothing
          let state = (cnf,redisconn,context)
          forkIO $ forever $ runReaderT (runPushermantT feedback) state
          putStrLn $ "connected\t" ++ (redisHost cnf) ++ "\t" ++ (redisList cnf)
          forever $ runReaderT (runPushermantT push) state
          destroyState state -- TODO: ensure this gets called when the program receives an exit signal

-- -- An example of the TLS library
-- sslHttpConnect :: ConnOpts -> IO ByteString
-- sslHttpConnect c = do
--     gen <- makeSystem
--     let params = defaultParams
--                { pCiphers = ciphersuite_all
--                , onCertificatesRecv = certificateChecks
--                      [ certificateVerifyChain
--                      , return . certificateVerifyDomain (cDomain c)
--                      ]
--                }
--         (host,url) = break (=='/') $ cUrl c
--         headers = [ ("Host", L.pack host)
--                   , ("Connection", "closed") ] ++ cHeaders c
--     ctx <- connectionClient (cDomain c) (cPort c) params gen
--     let httpPkt = cMethod c <+> " " <+> L.pack url <+> " HTTP/1.1\r\n"
--               <+> L.concat (map (\(h, v) -> h <+> ": " <+> v <+> "\r\n") headers)
--               <+> if not . null $ cPayload c
--                      then "Content-Length: " <+> L.pack (show . L.length $ payload)
--                       <+> "\r\n\r\n" <+> payload
--                      else ""
--         payload = L.intercalate "&" . map (\(k,v) -> k <+> "=" <+> v) $ cPayload c
--         line = "\n" <+> L.replicate 20 '-' <+> "\n"
--     L.putStrLn $ "HTTP Packet: " <+> line <+> httpPkt <+> line
--     putStrLn "Performing handshake..."
--     handshake ctx
--     putStrLn "Sending data..."
--     sendData ctx httpPkt
--     strictBS <- recvData ctx
--     return $ Chunk strictBS Empty
