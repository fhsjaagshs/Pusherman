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
import Control.Monad
import Control.Applicative

import Network.HTTP.Client.Conduit

import Network.TLS
import Crypto.Random.AESCtr (makeSystem)

import System.IO
import System.Environment

import qualified Data.Aeson as A

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.IO as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS

import qualified Data.HashMap.Strict as HM

-- TODO:
--   features:
--     - support reading from multiple lists
--   to-finish:
--     - Redis implementation
--     - push/pop
--     - SSL reading/writing using TLS library

-- Configuration from JSON
data Config = Config {
  configCertificate :: !FilePath,
  configKey :: !FilePath,
  configRedisHost :: !String,
  configRedisPort :: !Int,
  configRedisList :: !String,
  configPushLog :: Maybe FilePath,
  configFeedbackLog :: Maybe FilePath,
  configErrorLog :: Maybe FilePath,
  configWebhookURL :: Maybe String
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

-- Pusherman state
data State = State {
  stateContext :: Context,
  stateRedis :: Redis,
  stateRedisQueue :: B.ByteString,
  stateOutputHandle :: Handle,
  stateErrorHandle :: Handle,
  stateFeedbackHandle :: Handle,
  stateFeedbackWebhook :: Maybe String
}

mkState :: Config -> IO State
mkState (Config c k rh rp rq pl fl el wh) = State
                                            <$> context
                                            <*> redisConnection
                                            <*> redisQueue
                                            <*> pushOutput
                                            <*> errorOutput
                                            <*> stateFeedbackHandle
                                            <*> stateFeedbackWebhook
  where
    context = return () -- TODO: implement me
    redisConnection = open $ RedisOffline rh rp
    redisQueue = return $ B,pack rq
    pushOutput = maybe (return stdout) (flip openFile $ AppendMode) pl
    errorOutput = maybe (return stderr) (flip openFile $ AppendMode) el
    stateFeedbackHandle = maybe (return stdout) (\fp -> openFile fp AppendMode) fl
    stateFeedbackWebhook = return wh

    callWebhook :: MonadIO m => BS.ByteString -> Integer -> PushermanT m ()
    callWebhook token ts = ask >>= maybe (return ()) f . stateFeedbackWebhook
      where
        params = [("token",token), ("timestamp", BS.pack $ show ts)]
        f url = do
          initReq <- liftIO $ parseUrl url
          let req = (flip urlEncodedBody) initReq { method = "POST" } params
          void $ liftIO $ withManager $ httpNoBody req


destroyState :: State -> IO ()
destroyState (State ssl r q out err fb _) = do
  hClose fb
  hClose err
  hClose out
  close r
  -- TODO: close down @ssl@

newtype PushermanT m a = PushermanT { runPushermantT :: ReaderT State m a }

-- |Uses @f@ to derive a 'Handle' from the current PushermanT state
logg :: MonadIO m => (State -> Handle) -> B.ByteString -> PushermanT m ()
logg f bs = (f <$> ask) >>= \hdl -> liftIO (B.hPutStrLn hdl bs >> hFlush hdl)

--
-- Payload Generation
--

-- TODO: typecheck tokens and push_payload
-- |Takes JSON from Redis in the format:
-- @{ "tokens": [], "push_payload": {} }@
-- and turns it into a tuple in the form of (tokens,payload)
splitPayloads :: BS.ByteString -> Maybe ([BS.ByteString], BS.ByteString)
splitPayloads json = do
  parsed <- A.decodeStrict json >>= value2hm
  tokens <- filter ((==) 64 . BS.length) HM.lookup "tokens" parsed
  payload <- HM.lookup "payload" parsed
  if BS.length payload > 1900 || length tokens == 0
     then Nothing
     else Just (tokens,payload)
  where
    value2hm (A.Object hm) = Just hm
    value2hm _ = Nothing

--
-- Push Handling Logic
--
  
-- Main Action

feedback :: MonadIO m => PushermanT m ()
feedback = do
  Push.readFeedback readSSL $ \(timestamp, token) -> do
    logg stateFeedbackHandle $ (B.pack $ show timestamp) <> "\t" <> token
    callWebhook token timestamp
  where
    readSSL = return $ Just ""

push :: MonadIO m => PushermanT m ()
push = do
  q <- stateRedisQueue <$> ask
  conn <- stateRedis <$> ask
  maybe failure success $ (liftIO $ brpop conn q) >>= splitPayloads
  where
    toByteStr (RedisSimpleString s) = Right [s]
    toByteStr (RedisBulkString s) = Right [s]
    toByteStr (RedisError e) = Left e
    toByteStr (RedisArray xs) = map toByteString xs -- TODO: flatten Either String [ByteString]
    failure = logg stateErrorHandle "failed to load valid notification from redis"
    success (toks,pl) = mapM_ (sendPush pl) toks
    sendPush json token = do
      logg stateOutputHandle $ token <> "\t" <> json -- TODO: decide on logging behavior
      liftIO $ Push.sendApns sslwrite token json
    sslwrite _ = return () -- TODO: implement me

-- Main Function

pusherman :: Config -> IO ()
pusherman cnf = do
  st <- mkState cnf
  forkIO $ runReaderT (forever feedback) st
  (flip runReaderT) st $ do
    logg stateOutputHandle $ "connected\t"
                           <> (B.pack $ configRedisHost cnf)
                           <> "\t"
                           <> (B.pack $ configRedisList cnf)
    forever push
--   destroyState state -- TODO: ensure this gets called when the program receives an exit signal
        
-- Entry Point

main :: IO ()
main = readConfig >>= either invalidConfig pusherman
  where
    invalidConfig = hPutStrLn stderr . mappend "invalid configuration: "
    readConfig = A.eitherDecode =<< f =<< getArgs -- TODO: eitherDecode does not return @IO Config@
      where f = try . BS.readFile . fromMaybe "config.json" . listToMaybe

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
