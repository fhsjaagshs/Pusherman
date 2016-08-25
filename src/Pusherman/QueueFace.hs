{-# LANGUAGE MultiParamTypeclasses #-}

module Pusherman.QueueFace
(
  QueueFace(..)
)
where

import Data.ByteString (ByteString)

class QueueFace a b where
  open :: a -> IO ()
  close :: a -> IO a
  brpop :: ByteString -> a -> IO (Maybe b)

instance (Binary b) => QueueFace (QueueFace a ByteString) b where
