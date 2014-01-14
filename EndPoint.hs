module EndPoint where

import Data.Aeson
import Data.Aeson.Types
import Data.Definitions
import Network.HTTP.Types.URI (renderQuery)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B
import Network.HTTP.Conduit

nominatimEndPoint = B.pack "http://nominatim.openstreetmap.org/search"

toBS :: [(String, Maybe String)] -> [(B.ByteString, Maybe B.ByteString)]
toBS = map (\(a,b) -> (B.pack a, b >>= return . B.pack))

buildURI :: NominatimRequest -> B.ByteString
buildURI NominatimRequest { reqEmail = email, reqQ = q} =
  let params = [("format",Just "json"),
                ("q",Just q),
                ("email", email)]
  in B.append nominatimEndPoint (renderQuery True (toBS params))

test = NominatimRequest Nothing "Adyar"

main :: IO ()
main = do
  resp <- simpleHttp (B.unpack (buildURI test))
  -- putStrLn (L.unpack resp)
  let d = decode (L.tail . L.init $ resp) :: [Maybe NominatimResponse]
  putStrLn (show d) 

