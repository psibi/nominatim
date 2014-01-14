{-# LANGUAGE OverloadedStrings #-}

module Data.Definitions where

import Data.Aeson
import Data.Aeson.Types
import Control.Applicative ((<$>), (<*>), empty)
import qualified Data.ByteString.Char8 as BS


data NominatimRequest = NominatimRequest {
  reqEmail :: Maybe String,
  reqQ :: String
  }

data NominatimResponse = NominatimResponse {
  latitude :: Double,
  longitude :: Double
  } deriving (Show)
  
instance FromJSON NominatimResponse where
  parseJSON (Object v) = NominatimResponse <$>
                         v .: "lat" <*>
                         v .: "lon"
  parseJSON _ = empty

