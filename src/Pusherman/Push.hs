module Pusherman.Push (
  apnsHost,
  apnsPort,
  apnsFeedbackHost,
  apnsFeedbackPort,
  readFeedback,
  sendApns
) where
  
import GHC.Word
import Data.Binary.Put
import Data.Binary.Get
import Data.Convertible

import Data.Maybe
import Data.Time.Clock.POSIX as CP

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16

feedback :: Get (Integer, B.ByteString)
feedback = (,)
  <$> (fromIntegral <$> getWord32be) -- time
  <*> (getWord16be >>= fmap B16.encode . getByteString . convert) -- token
    
buildPDU :: B.ByteString -> B.ByteString -> Word32 -> Put
buildPDU token payload expiry
  | (B.length token) /= 32 = fail "invalid token"
  | (B.length payload > 1900) = fail "payload too large (max <=2000)"
  | otherwise = do
    putWord8 1
    putWord32be 1
    putWord32be expiry
    putByteStringLength token
    putByteStringLength payload
    where
      putByteStringLength bs = do
        putWord16be $ convert $ B.length bs
        putByteString bs

-- TODO: ensure this works
readFeedback :: IO (Maybe B.ByteString) -> ((Integer, B.ByteString) -> IO ()) -> IO
readFeedback read callback = incrGet feedback read >>= either (return ()) callback
  where -- read 38
    incrGet g r = r >>= h
      where
        h = f . runGetIncremental g
        f (Fail _ _ str) = return $ Left str 
        f (Done _ _ res) = return $ Right res
        f (Partial p) = r >>= f . p . Just
      

sendApns :: (B.ByteString -> IO ()) -> B.ByteString -> B.ByteString -> IO ()
sendApns w token json = CP.getPOSIXTime >>= w . runPut . buildPayload . (+3600) . round
  where base16Token = fst $ B16.decode token
        buildPayload = buildPDU base16Token json

apnsHost :: (IsString a) => a
apnsHost = "gateway.push.apple.com"

apnsPort :: (Num a) => a
apnsPort = 2195

apnsFeedbackHost :: (IsString a) => a
apnsFeedbackHost = "feedback.push.apple.com"

apnsFeedbackPort :: (Num a) => a
apnsFeedbackPort = 2196
