{-# LANGUAGE OverloadedStrings #-}

module Pusherman.Push (
  apnsPayloadLength,
  apnsHost,
  apnsPort,
  apnsFeedbackHost,
  apnsFeedbackPort,
  feedback,
	apnsPayload,
	readOne,
	readAvailable
) where
  
import GHC.Word
import Data.Binary.Put
import Data.Binary.Get
import Data.Convertible

import Data.Maybe
import Data.Time.Clock.POSIX as CP

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16

apnsPayloadLength :: Num a => a
apnsPayloadLength = 2000

apnsTimeout :: (Num a) => a
apnsTimeout = 3600

apnsHost :: (IsString a) => a
apnsHost = "gateway.push.apple.com"

apnsPort :: (Num a) => a
apnsPort = 2195

apnsFeedbackHost :: (IsString a) => a
apnsFeedbackHost = "feedback.push.apple.com"

apnsFeedbackPort :: (Num a) => a
apnsFeedbackPort = 2196

-- | APNS Get and Put monads

-- | Reads feedback from Apple's APNS Feedback service
feedback :: Get (Integer, B.ByteString)
feedback = (,)
  <$> (fromIntegral <$> getWord32be) -- time
  <*> (getWord16be >>= fmap B16.encode . getByteString . convert) -- token

-- a sane value for timeout is 3600
-- | Builds an APNS payload given an APNS token, a body
apnsPayload :: Num a => B.ByteString -> B.ByteString -> a -> IO Put
apnsPayload token payload timeout
  | B.length payload > apnsPayloadLength = return $ fail "provided payload exceeds maximum size of " ++ show apnsPayloadLength
	| otherwise = do
		expiry <- CP.getPOSIXTime >>= ((+) timeout) . round
		return $ do
	    putWord8 1
	    putWord32be 1
	    putWord32be expiry
	    putByteStringLength $ fst $ B16.decode token
	    putByteStringLength payload

-- | helpers

readOne :: Get a -> B.ByteString -> IO (Either String (B.ByteString, a))
readOne gt carry = f $ pushChunk carry $ runGetIncremental gt
  where f (Fail _ _ str) = return $ Left str
        f (Done rdr _ res) = return $ Right (rdr, res)
        f (Partial p) = rd >>= f . p . Just

readAvailable :: IO B.ByteString -> Get a -> IO (Either String [a])
readAvailable rd gt = go [] ""
  where
	  go acc cry = readOne gt cry >>= either (return . Left) (recur acc)
		recur acc ("", a) = acc ++ [a]
		recur acc (rdr, a) = go (acc ++ [a]) rdr
		
-- | Internal

putByteStringLength :: B.ByteString -> Put
putByteStringLength bs = do
  putWord16be $ convert $ B.length bs
  putByteString bs