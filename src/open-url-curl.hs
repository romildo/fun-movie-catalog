module Main where

import Network.Curl.Download (openURIWithOpts)
import Network.Curl.Opts (CurlOption(CurlEncoding))
import qualified Data.ByteString as B

main =
  do src <- openURIWithOpts [CurlEncoding "gzip"] "http://www.adorocinema.com/common/search/search_by_film/?criteria=Bourne"
     case src of
       Left err -> print err
       Right doc -> do B.writeFile "test.html" doc
                       B.putStrLn doc
