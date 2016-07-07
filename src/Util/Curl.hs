module Util.Curl where

import Network.URI (isAllowedInURI, escapeURIString)
import Network.Curl (initialize, withCurlDo, do_curl_, CurlOption(CurlPostFields), method_POST, CurlResponse, respCurlCode, CurlCode(CurlOK), respBody)

submitPOSTForm :: String -> [(String, String)] -> IO (Maybe String)
submitPOSTForm url vars =
  withCurlDo $
    do let url' = escapeURIString isAllowedInURI url
       let formData = map (\(a,b) -> a ++ "=" ++ b) vars
       curl <- initialize
       r <- do_curl_ curl url' (CurlPostFields formData : method_POST) :: IO CurlResponse
       case respCurlCode r of
         CurlOK -> return (Just (respBody r))
         _ -> return Nothing

