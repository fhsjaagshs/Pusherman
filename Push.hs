module Push (
  sendAPNS,
  readFeedback
) where

import Hex

import Data.Text
import Data.Text.Encoding

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import Data.Binary.Get
import GHC.Word
import Data.Convertible

import Data.Time.Clock.POSIX

import Network.Socket
import Network.BSD
import OpenSSL
import OpenSSL.Session

getFeedback :: Get (Integer, String)
getFeedback = do
  time <- getWord32be
  len <- getWord16be
  dtoken <- getByteString $ convert len
  return (fromIntegral time, BS.unpack $ B16.encode dtoken)
    
buildPDU :: B.ByteString -> BU.ByteString -> Word32 -> Put -- second one is BU.ByteString
buildPDU token payload expiry
  | (B.length token) /= 32 = fail "Invalid token"
  | (B.length payload > 255) = fail "Too long payload"
  | otherwise = do
    putWord8 1
    putWord32be 1
    putWord32be expiry
    putWord16be ((Data.Convertible.convert $ B.length token) :: Word16)
    putByteString token
    putWord16be ((Data.Convertible.convert $ B.length payload) :: Word16)
    putByteString payload
    
getHourExpiryTime :: IO (Word32)
getHourExpiryTime = do
  pt <- Data.Time.Clock.POSIX.getPOSIXTime
  return ( (round pt + 60*60) :: Word32)
  
socketWithKeypair :: FilePath -> FilePath -> String -> PortNumber -> IO SSL
socketWithKeypair certificateFile keyFile host port = do
  -- setup ssl context
  ssl <- OpenSSL.Session.context
  OpenSSL.Session.contextSetPrivateKeyFile ssl keyFile
  OpenSSL.Session.contextSetCertificateFile ssl certificateFile
  OpenSSL.Session.contextSetDefaultCiphers ssl
  OpenSSL.Session.contextSetVerificationMode ssl OpenSSL.Session.VerifyNone

  -- Open SSL socket
  proto <- Network.BSD.getProtocolNumber "tcp"
  he <- Network.BSD.getHostByName host
  sock <- socket AF_INET Stream proto
  Network.Socket.connect sock (SockAddrInet port (hostAddress he))
  
  sslsocket <- OpenSSL.Session.connection ssl sock
  return sslsocket
  
writeSSL :: SSL -> B.ByteString -> IO ()
writeSSL sslsocket str = do
  OpenSSL.Session.connect sslsocket
  OpenSSL.Session.write sslsocket str
  OpenSSL.Session.shutdown sslsocket Unidirectional

parseAndCall :: BL.ByteString -> ((Integer, String) -> IO ()) -> IO ()
parseAndCall readStr callback
  | (BL.length readStr) < 1 = do return ()
  | otherwise = callback (runGet getFeedback readStr)
  
readFeedback :: FilePath -> FilePath -> ((Integer, String) -> IO ()) -> IO ()
readFeedback certificateFile keyFile callback = withOpenSSL $ do
  sslsocket <- socketWithKeypair certificateFile keyFile "feedback.push.apple.com" 2196
  OpenSSL.Session.connect sslsocket
  readStr <- OpenSSL.Session.read sslsocket 38
  
  parseAndCall (BL.fromStrict readStr) callback
  
  OpenSSL.Session.shutdown sslsocket Unidirectional
  
sendAPNS :: FilePath -> FilePath -> String -> Text -> IO ()
sendAPNS certificateFile keyFile token json = withOpenSSL $ do
  sslsocket <- socketWithKeypair certificateFile keyFile "gateway.push.apple.com" 2195
  expiration <- getHourExpiryTime
  
  let toStrict = B.concat . BL.toChunks
  writeSSL sslsocket (toStrict $ runPut $ buildPDU (hexToByteString token) (encodeUtf8 json) expiration)
    