module Push (
  sendAPNS,
) where

import Hex

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import GHC.Word
import Data.Convertible

import Data.Time.Clock.POSIX

import Network.Socket
import Network.BSD
import OpenSSL
import OpenSSL.Session
  
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
  
socketWithKeypair :: FilePath -> FilePath -> String -> Integer -> IO SSL
socketWithKeypair certificateFile keyFile host port = do
  -- setup ssl context
  ssl <- OpenSSL.Session.context
  OpenSSL.Session.contextSetPrivateKeyFile ssl certificateFile
  OpenSSL.Session.contextSetCertificateFile ssl keyFile
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
  
readFeedback :: FilePath -> FilePath -> (SSL -> IO [(B.ByteString, B.ByteString)]) -> IO [(B.ByteString, B.ByteString)]
readFeedback certificateFile keyFile parsefunc = withOpenSSL $ do
  sslsocket <- socketWithKeypair certificateFile keyFile "feedback.push.apple.com" 2196
  parsed <- parsefunc sslsocket
  return $ parsed
  
sendAPNS :: FilePath -> FilePath -> String -> String -> IO ()
sendAPNS certificateFile keyFile token json = withOpenSSL $ do
  sslsocket <- socketWithKeypair certificateFile keyFile "gateway.push.apple.com" 2195
  expiration <- getHourExpiryTime
  
  writeSSL sslsocket (toStrict $ runPut $ buildPDU (hexToByteString token) (BU.fromString json) expiration)
  where
    toStrict = B.concat . BL.toChunks
    
-- TODO: Write function to read feedback
    