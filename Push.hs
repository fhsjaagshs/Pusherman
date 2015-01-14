module Push (
  sendAPNS,
  readFeedback
) where

import Hex

import Data.Attoparsec.Char8 as Attoparsec

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
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

getFeedback :: Get (Integer, B.ByteString)
getFeedback = do
  time <- getWord32be
  len <- getWord16be
  dtoken <- getBytes $ convert len
  return (posixSecondsToUTCTime $ fromInteger $ convert time, B16.encode dtoken)
  
buildPDU :: B.ByteString -> BU.ByteString -> Word32 -> Put
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
  return ( (round pt + 60*60):: Word32)
  
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
  return $ sslsocket
  
writeSSL :: SSL -> B.ByteString -> IO ()
writeSSL sslsocket str = do
  OpenSSL.Session.connect sslsocket
  OpenSSL.Session.write sslsocket str
  OpenSSL.Session.shutdown sslsocket Unidirectional
  
readFeedbackRecur :: SSL -> ((Integer, B.ByteString) -> IO ()) -> IO ()
readFeedbackRecur sslsocket callback = do
  OpenSSL.Session.connect sslsocket
  readStr <- OpenSSL.Session.read sslsocket 38 -- 38 is the length of the feedback payload.
  
  callback (runGet readStr getFeedback)
    
  OpenSSL.Session.shutdown sslsocket Unidirectional
  
  readFeedbackRecur sslsocket callback
  
  
readFeedback :: FilePath -> FilePath -> ((Integer, B.ByteString) -> IO ()) -> IO ()
readFeedback certificateFile keyFile callback = withOpenSSL $ do
  sslsocket <- socketWithKeypair certificateFile keyFile "feedback.push.apple.com" 2196
  readFeedbackRecur sslsocket callback
  
sendAPNS :: FilePath -> FilePath -> String -> String -> IO ()
sendAPNS certificateFile keyFile token json = withOpenSSL $ do
  sslsocket <- socketWithKeypair certificateFile keyFile "gateway.push.apple.com" 2195
  expiration <- getHourExpiryTime
  
  writeSSL sslsocket (toStrict $ runPut $ buildPDU (hexToByteString token) (BU.fromString json) expiration)
  where
    toStrict = B.concat . BL.toChunks
    