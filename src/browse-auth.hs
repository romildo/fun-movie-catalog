import Codec.Compression.GZip (decompress)
import Network.URI (isAllowedInURI, escapeURIString, parseURI)
import Network.HTTP (Header(Header), RequestMethod(..), Request(Request,rqURI,rqMethod,rqHeaders,rqBody), RequestMethod(GET,POST), mkRequest, setRequestBody, rspBody, getRequest, simpleHTTP, insertHeader, HeaderName(HdrAcceptEncoding,HdrContentEncoding,HdrContentType,HdrUserAgent,HdrContentLength), findHeader, urlEncodeVars, defaultUserAgent)
import Network.Browser (Cookie, setCookies, getCookies, Authority(..), setAllowRedirects, addAuthority, request, browse)
import Network.Stream (ConnError(ErrorMisc))
import qualified Data.ByteString.Lazy.Char8 as LB.Char8

import Util (authenticatePOST, postURIString, openURIString)

getURI :: [Cookie] -> String -> [(String,String)] -> IO (Either ConnError String)
getURI cookies uriString' vars =
  do let uriString = uriString' ++ "?" ++ urlEncodeVars vars
     putStrLn ("> getURI: " ++ uriString)
     case parseURI uriString of
       Nothing -> return $ Left (ErrorMisc ("cannot parse uri: " ++ uriString))
       Just uri -> do (_,rsp) <- browse $ do setAllowRedirects True
                                             setCookies cookies
                                             request (mkRequest GET uri)
                      return $ Right $ rspBody rsp

-- postURI :: [Cookie] -> String -> [(String,String)] -> IO (Either ConnError String)
-- postURI cookies uriString vars =
--   do putStrLn ("> postURI: " ++ uriString)
--      case parseURI uriString of
--        Nothing -> return $ Left (ErrorMisc ("cannot parse uri: " ++ uriString))
--        Just uri -> do (_,rsp) <- browse $ do setAllowRedirects True
--                                              setCookies cookies
--                                              request (setRequestBody (mkRequest POST uri) ("application/x-www-form-urlencoded", urlEncodeVars vars))
--                                              -- request (getRequest "http://www.manicomio-share.com/torrents-details.php?s=101589&id=296711&hit=1")
--                       return $ Right $ rspBody rsp

test1 = do x <- getURI [] "http://www.manicomio-share.com" [("username","romildo"),("password","ana1995")]
           case x of
             Left e -> putStrLn (show e)
             Right doc -> writeFile "test1.html" doc

test2 = do x <- postURIString [] "http://www.manicomio-share.com/account-login.php" [("username","romildo"),("password","ana1995")]
           case x of
             Left e -> putStrLn (show e)
             Right doc -> writeFile "test2.html" doc


browseSite usr pass authURI otherURI =
  case parseURI authURI of
    Nothing -> return $ Left (ErrorMisc ("cannot parse uri: " ++ authURI))
    Just uri -> do (_,rsp) <- browse $ do setAllowRedirects True
                                          let vars = [("username",usr),("password",pass)]
                                          request (setRequestBody (mkRequest POST uri) ("application/x-www-form-urlencoded", urlEncodeVars vars))
                                          request (getRequest otherURI)
                   return $ Right $ rspBody rsp


browseCapasBrasil =
  do cookies <- authenticatePOST
                  "http://www.capasbrasil.com.br/index.php?app=core&module=global&section=login&do=process"
                  "eagle007"
                  "1995.An@"
     case cookies of
       Left err ->
         putStrLn (show err)
       Right cs ->
         do x <- postURIString cs "http://www.capasbrasil.com.br/index.php?app=core&module=search&do=search&fromMainBar=1" [("search_app","gallery"), ("search_term", "informante")]
            -- x <- openURIString cs "http://www.capasbrasil.com.br/index.php?/user/99956-eagle007/"
            case x of
              Left err -> putStrLn (show err)
              Right doc -> writeFile "test3.html" doc


-- browseCapasBrasil =
--   browseSite
--     "eagle007"
--     "1995.An@"
--     "http://www.capasbrasil.com.br/index.php?app=core&module=global&section=login&do=process"
--     "http://www.capasbrasil.com.br/index.php?/user/99956-eagle007/"
