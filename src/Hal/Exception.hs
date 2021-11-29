module Hal.Exception
  ( HalException (..),
    throwException,
  )
where

import Control.Exception (Exception, throw)

newtype HalException = HalException String

instance Show HalException where
  show (HalException msg) = "Exception: " ++ msg

instance Exception HalException

throwException :: String -> IO a
throwException = throw . HalException
