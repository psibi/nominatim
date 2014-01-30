{-# LANGUAGE OverloadedStrings #-}

module Data.Definitions where

import Data.Aeson
import Data.Aeson.Types
import Control.Applicative ((<$>), (<*>), empty)
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import qualified Control.Exception as E
import Numeric

data NominatimRequest = NominatimRequest {
  reqEmail :: Maybe String,
  reqQ :: String
  }

data NominatimResponse = NominatimResponse {
  latitude :: Double,
  longitude :: Double
  } deriving (Show)
  
instance FromJSON NominatimResponse where
  parseJSON (Object v) = let lat = v .: "lat"
                             lon = v .: "lon"
                         in  NominatimResponse <$>
                             (liftM toDouble lat) <*>
                             (liftM toDouble lon)
                         
  parseJSON _ = empty

toDouble :: String -> Double
toDouble str = fst . head . readFloat $ str

data Error = HttpConnectionError E.SomeException
           | JSONError
           deriving (Show)
