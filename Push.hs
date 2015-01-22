module Push (
  sendAPNS,
  readFeedback
) where
  
import GHC.Word
import Data.Binary.Put
import Data.Binary.Get
import Data.Convertible

import Data.Maybe

import Data.Time.Clock.POSIX

import OpenSSL
import OpenSSL.Session

import Network.BSD
import Network.Socket

import qualified Data.Text as T
import qualified Data.Text.Encoding as T.Encoding

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

getFeedback :: Get (Integer, String)
getFeedback = do
  time <- getWord32be
  len <- getWord16be
  dtoken <- getByteString $ convert len
  return (fromIntegral time, BS.unpack $ B16.encode dtoken)
    
buildPDU :: B.ByteString -> BU.ByteString -> Word32 -> Put -- second one is BU.ByteString
buildPDU token payload expiry
  | (B.length token) /= 32 = fail "Invalid token"
  | (B.length payload > 1900) = fail "Payload bigger than APNS size limit." -- 2000 is the max payload (minus some thick padding)
  | otherwise = do
    putWord8 1
    putWord32be 1
    putWord32be expiry
    putWord16be ((Data.Convertible.convert $ B.length token) :: Word16)
    putByteString token
    putWord16be ((Data.Convertible.convert $ B.length payload) :: Word16)
    putByteString payload

parseAndCall :: BL.ByteString -> ((Integer, String) -> IO ()) -> IO ()
parseAndCall readStr callback
  | (BL.length readStr) < 1 = do return ()
  | otherwise = callback (runGet getFeedback readStr)
  
writeSSL :: SSLContext -> String -> PortNumber -> B.ByteString -> IO ()
writeSSL ssl host port str = withOpenSSL $ do
  proto <- Network.BSD.getProtocolNumber "tcp"
  he <- Network.BSD.getHostByName host
  sock <- socket AF_INET Stream proto
  
  Network.Socket.connect sock (SockAddrInet port (hostAddress he))
  
  sslsocket <- OpenSSL.Session.connection ssl sock
  
  OpenSSL.Session.connect sslsocket
  OpenSSL.Session.write sslsocket str
  OpenSSL.Session.shutdown sslsocket Biidirectional
  case OpenSSL.Session.sslSocket sslsocket of
    Nothing -> Nothing
    Just tcpSock -> Network.Socket.shutdown tcpSock ShutdownBoth
  return ()
  
readSSL :: SSLContext -> String -> PortNumber -> Int -> IO (B.ByteString)
readSSL ssl host port readLen = withOpenSSL $ do
  proto <- Network.BSD.getProtocolNumber "tcp"
  he <- Network.BSD.getHostByName host
  sock <- socket AF_INET Stream proto
  
  Network.Socket.connect sock (SockAddrInet port (hostAddress he))
  
  sslsocket <- OpenSSL.Session.connection ssl sock
  
  OpenSSL.Session.connect sslsocket
  readStr <- OpenSSL.Session.read sslsocket readLen
  OpenSSL.Session.shutdown sslsocket Bidirectional
  case OpenSSL.Session.sslSocket sslsocket of
    Nothing -> Nothing
    Just tcpSock -> Network.Socket.shutdown tcpSock ShutdownBoth
  return readStr
  
readFeedback :: SSLContext -> ((Integer, String) -> IO ()) -> IO ()
readFeedback ssl callback = withOpenSSL $ do
  readStr <- readSSL ssl "feedback.push.apple.com" 2196 38
  parseAndCall (BL.fromStrict readStr) callback
    
sendAPNS :: SSLContext -> B.ByteString -> T.Text -> IO ()
sendAPNS ssl token json = withOpenSSL $ do
  posixTime <- Data.Time.Clock.POSIX.getPOSIXTime
  let expiry = (round posixTime + 60*60) :: Word32
  let pdu = (BL.toStrict $ runPut $ buildPDU (fst $ B16.decode token) (T.Encoding.encodeUtf8 json) expiry)
  
  writeSSL ssl "gateway.push.apple.com" 2195 pdu
