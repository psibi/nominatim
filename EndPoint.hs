{-# LANGUAGE OverloadedStrings, StandaloneDeriving, DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
module EndPoint where

import Data.Aeson
import Data.Aeson.Types
import Data.Definitions
import Network.HTTP.Types.URI (renderQuery)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Conduit
import Network.HTTP.Types (Method, Status(..))
import qualified Control.Exception as E
import Data.Maybe (fromMaybe)

nominatimEndPoint = BS.pack "http://nominatim.openstreetmap.org/search"

toBS :: [(String, Maybe String)] -> [(BS.ByteString, Maybe BS.ByteString)]
toBS = map (\(a,b) -> (BS.pack a, b >>= return . BS.pack))

buildURI :: NominatimRequest -> BS.ByteString
buildURI NominatimRequest { reqEmail = email, reqQ = q} =
  let params = [("format",Just "json"),
                ("q",Just q),
                ("email", email)]
  in BS.append nominatimEndPoint (renderQuery True (toBS params))

test = NominatimRequest Nothing "Adyar, Chennai"

main :: IO ()
main = do
  -- resp <- simpleHttp (BS.unpack (buildURI test))
  -- -- putStrLn (L.unpack resp)
  -- let d = eitherDecode resp :: Either String [NominatimResponse]
  d <- latLon test (Just $ Proxy "127.0.0.1" 3129)
  putStrLn (show d)


latLon :: NominatimRequest -> Maybe Proxy -> IO (Either String [NominatimResponse])
latLon req proxy =
  nominatimAPI (BS.pack "GET")
               (BS.unpack (buildURI req))
               (Just (RequestBodyBS (BS.pack "")))
               (proxy)

             

nominatimAPI :: (FromJSON b, Show b) => BS.ByteString
                -> String
                -> Maybe RequestBody
                -> Maybe Proxy
                -> IO (Either String b)
nominatimAPI apiMethod url body hproxy = do
  result <- doHttp apiMethod url body hproxy
  case result of
    Left e -> return (Left ("HttpConnectionError"))
    Right resp -> return $ eitherDecode (responseBody resp)
    -- Right resp -> case  responseBody (eitherDecode resp) of
    --   Left e -> return (Left JSONError)
    --   Right ans -> return (Right ans)

doHttp reqMethod url body hproxy = do
  let reqBody = fromMaybe (RequestBodyBS $ BS.pack "") body
      Just uri = parseUrl url
      request = uri { method = reqMethod
                    , requestBody = reqBody
                    , checkStatus = successOrMissing
                    , proxy = hproxy
                    }
 
  (getResponse request >>= return . Right) `E.catches` [
      -- Re-throw AsyncException, otherwise execution will not terminate on
      -- SIGINT (ctrl-c).  All AsyncExceptions are re-thrown (not just
      -- UserInterrupt) because all of them indicate severe conditions and
      -- should not occur during normal operation.
      E.Handler (\e -> E.throw (e :: E.AsyncException)),
      E.Handler (\e -> (return . Left) (e :: E.SomeException))
      ]
  where
    getResponse request = withManager $ \manager -> httpLbs request manager
    successOrMissing s@(Status sci _) hs cookiejar
      | (200 <= sci && sci < 300) || sci == 404 = Nothing
      | otherwise = Just $ E.toException $ StatusCodeException s hs cookiejar


