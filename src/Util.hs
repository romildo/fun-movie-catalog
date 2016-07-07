{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}

#define REGEX_PCRE

module Util where

import Debug.Trace (trace)
import Data.Char (isSpace, isAlphaNum, isUpper, toLower, toUpper)
import Data.Maybe (mapMaybe)
import Data.List (findIndex)
import Data.List.Utils (join)
import Data.Time (getCurrentTime, utctDay, toGregorian)
import Data.Encoding (decodeLazyByteString)
import Data.Encoding.CP1252 (CP1252(CP1252))
import Codec.Text.IConv (convert)
import Control.Monad (when)
import Control.Exception (IOException,try)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as LB.UTF8
import qualified Data.ByteString.Lazy.Char8 as LB.Char8
import Codec.Compression.GZip (decompress)
import Network.URI (isAllowedInURI, escapeURIString, parseURI)
import Network.HTTP (Header(Header), Request(Request,rqURI,rqMethod,rqHeaders,rqBody), RequestMethod(GET,POST), mkRequest, rspBody, getRequest, simpleHTTP, insertHeader, HeaderName(HdrAcceptEncoding,HdrContentEncoding,HdrContentType,HdrUserAgent,HdrContentLength), findHeader, urlEncodeVars, defaultUserAgent, setRequestBody, replaceHeader)
import Network.Browser (Cookie, setCookies, getCookies, setAllowRedirects, request, browse)
import Network.Stream (ConnError(ErrorMisc))
import System.FilePath (addExtension, combine)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing)
import Data.Array.IArray (elems, (!))
import Text.Regex.Base (makeRegex, makeRegexOpts, defaultCompOpt, defaultExecOpt, matchAll, match)

#if defined(REGEX_PCRE)
import Text.Regex.PCRE (Regex, compCaseless, matchOnceText, (=~))
#elif defined(REGEX_TDFA)
import Text.Regex.TDFA (Regex, CompOption(caseSensitive), matchOnceText, (=~))
#endif


import qualified Codec.Binary.UTF8.String as UTF8



tracefun msg f x = let x' = trace ("> " ++ msg ++ ": " ++ show x) x
                       y  = f x'
                   in trace ("< " ++ msg ++ ": " ++ show y) y


m1 `after` m2 = do x <- m2
                   m1
                   return x



stripSuffix xs ys = go (reverse xs) (reverse ys)
  where
    go [] ys = Just (reverse ys)
    go (x:xs) (y:ys) | x == y = go xs ys
    go _ _ = Nothing




trimLeft =
    dropWhile isSpace

trimRight =
    reverse . trimLeft . reverse

trim =
    trimRight . trimLeft




whenM cond action =
    do result <- cond
       when result action

findPos compareTo list = go list 0
    where
      go []     i = Right i
      go (x:xs) i = case compareTo x of
                      EQ -> Left i
                      LT -> Right i
                      GT -> go xs (i + 1)

-- split delim s =
--     let (token,rest) = span (/=delim) s
--     in case rest of
--          [] -> [ token ]
--          _:rest' -> token : split delim rest'

-- join delim [] = ""
-- join delim (x:xs) = x ++ foldr (\y s -> delim:y ++ s) "" xs



findPosition [] _ i = (i,False)
findPosition (x:xs) y i | x == y = (i,True)
                        | x >  y = (i,False)
                        | otherwise = findPosition xs y (i + 1)









-- steal from getRequest but change type
-- it's lazy bytestring because the GZip library specifies it
myGetRequest :: String -> Request LB.ByteString
myGetRequest s =
  case parseURI s of
    Nothing -> error "url syntax error"
    Just uri -> mkRequest GET uri

-- INCOMPLETE: check exceptions
openURI :: [Cookie] -> String -> IO (Either ConnError LB.Char8.ByteString)
openURI cookies uriString =
  do putStrLn ("> openURI: " ++ uriString)
     case parseURI uriString of
       Nothing -> return $ Left (ErrorMisc ("cannot parse uri: " ++ uriString))
       Just uri -> do (_,rsp) <- browse $ do setAllowRedirects True
                                             setCookies cookies
                                             request (mkRequest GET uri)
                      return $ Right $ rspBody rsp

openURI_ :: [Cookie] -> String -> IO (Either ConnError LB.Char8.ByteString)
openURI_ cookies uri =
  do putStrLn ("> openURI_: " ++ uri)
     src <- simpleHTTP (insertHeader HdrAcceptEncoding "gzip" (myGetRequest uri))
     return $ fmap gunzip src
  where
    gunzip rsp = case findHeader HdrContentEncoding rsp of
                   Just "gzip" -> decompress (rspBody rsp)
                   _ -> rspBody rsp

openURIString :: [Cookie] -> String -> IO (Either ConnError String)
openURIString cookies uri =
  do src <- openURI cookies uri
     return (fmap decodeHtml src)

decodeHtml :: LB.Char8.ByteString -> String
decodeHtml text =
  case match regex text :: (LB.ByteString,LB.ByteString,LB.ByteString,[LB.ByteString]) of
    (a,_,z,[prefix,encoding,suffix])
      -- | trace ("HERE: " ++ LB.Char8.unpack encoding) False -> undefined
      | enc == LB.Char8.pack "iso-8859-1"   -> LB.Char8.unpack text'
      | enc == LB.Char8.pack "windows-1252" -> decodeLazyByteString CP1252 text' -- using the encoding package
      -- | enc == LB.Char8.pack "windows-1252" -> LB.UTF8.toString (convert "CP1252" "UTF8" text') -- using the iconv package
      where
        enc = LB.Char8.map toLower encoding
        text' = LB.Char8.append a (LB.Char8.append prefix (LB.Char8.append (LB.Char8.pack "UTF-8") (LB.Char8.append suffix z)))
    _ -> LB.UTF8.toString text
  where
    re = "(<meta http-equiv=\"Content-Type\" content=\"text/html; charset=)([a-zA-Z0-9-]+)(\")"
#if defined(REGEX_TDFA)
    regex = makeRegexOpts (defaultCompOpt{caseSensitive=False}) defaultExecOpt (LB.Char8.pack re) :: Regex
#elif defined(REGEX_PCRE)
    regex = makeRegexOpts compCaseless defaultExecOpt (LB.Char8.pack re) :: Regex
#endif

-- | Post some content to a uri, return the content of the response or
-- an error.

-- postURI :: String -> [(String, String)] -> [Cookie] -> IO (Either ConnError LB.Char8.ByteString)
-- postURI uriStr vars cookies =
--   case parseURI uriStr of
--     Nothing -> return $ Left (ErrorMisc ("Cannot parse uri: " ++ uriStr))
--     Just uri -> simpleHTTP rq >>= return . fmap rspBody
--       where
--         body = LB.Char8.pack (urlEncodeVars vars)
--         rq :: Request LB.ByteString
--         rq = Request
--              { rqURI = uri
--              , rqMethod = POST
--              , rqHeaders = [ Header HdrContentType "application/x-www-form-urlencoded"
--                            , Header HdrUserAgent defaultUserAgent
--                            , Header HdrContentLength (show (LB.Char8.length body))
--                            ]
--              , rqBody = body
--              }

mkPostRequest :: String -> [(String,String)] -> Either ConnError (Request LB.Char8.ByteString)
mkPostRequest uriString vars =
  case parseURI uriString of
    Nothing -> Left (ErrorMisc ("cannot parse uri: " ++ uriString))
    Just uri -> let body = LB.Char8.pack (urlEncodeVars vars)
                in Right $ Request
                     { rqURI = uri
                     , rqMethod = POST
                     , rqHeaders = [ Header HdrUserAgent defaultUserAgent
                                   , Header HdrContentType "application/x-www-form-urlencoded"
                                   , Header HdrContentLength (show (LB.Char8.length body))
                                   ]
                     , rqBody = body
                     }

postURI :: [Cookie] -> String -> [(String,String)] -> IO (Either ConnError LB.Char8.ByteString)
postURI cookies uriString vars =
  do putStrLn ("> postURI: " ++ uriString)
     case mkPostRequest uriString vars of
       Left er -> return $ Left er
       Right req -> do (_,rsp) <- browse $ do setAllowRedirects True
                                              setCookies cookies
                                              request req
                       return $ Right $ rspBody rsp


postURIString :: [Cookie] -> String -> [(String, String)] -> IO (Either ConnError String)
postURIString cookies uriStr vars =
  postURI cookies uriStr vars >>= return . fmap decodeHtml

test1 =
  postURI [] "http://www.interfilmes.com/busca.html" [("search","aranha")] >>= handleE (error . show) >>=
  LB.Char8.writeFile "test1.html"

test2 =
  postURIString [] "http://www.interfilmes.com/busca.html" [("search","aranha")] >>= handleE (error . show) >>=
  writeFile "test2.html"



findIndexM p xs = go 0 xs
  where
    go _ [] = return Nothing
    go n (x:xs) = do res <- p x
                     if res then return (Just n) else go (n+1) xs



spanM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
spanM _ xs@[]      = return (xs,xs)
spanM p xs@(x:xs') = do res <- p x
                        if res
                        then do (ys,zs) <- spanM p xs'
                                return (x:ys,zs)
                        else return ([],xs)

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([],[])
partitionM p (x:xs) = do res <- p x
                         (as,bs) <- partitionM p xs
                         return $ if res then (x:as,bs) else (as,x:bs)



replace [] _ text = text
replace s1 s2 text =
  go s1 text text
  where
    go [] ys _ = s2 ++ go s1 ys ys
    go _ [] text = text
    go (x:xs) (y:ys) text@(z:zs) | x == y = go xs ys text
                                 | otherwise = z : go s1 zs zs






-- match text re = case text =~ re :: [[String]] of
--                   (_:ys):_ -> Just ys104
--                   [] -> Nothing

matchRegex text re = case text =~ re :: [[String]] of
                       (_:ys):_ -> Just ys
                       [] -> Nothing

-- uses ByteString in order to circunvent a bug with regex-pcre
-- REMEMBER TO CHECK IF THE BUG HAS BEEN FIXED
matchRE re text = case (LB.Char8.pack text) =~ (LB.Char8.pack re) of
                    (_:ys):_ -> Just (map LB.Char8.unpack ys)
                    [] -> Nothing

-- matchRE re text = case match (makeRegex re :: Regex) text :: [[String]] of
--                     (_:xs):_ -> Just xs
--                     _ -> Nothing



matchREs res text =
  mapMaybe f res
  where
    f (re,f) = case matchOnceText (makeRegex re::Regex) text of
                 Just (_,match,_) -> Just $ f (map fst (elems match))
                 Nothing -> Nothing

splitRegex _ [] = []
splitRegex delim strIn =
  loop strIn
  where
    re = makeRegex delim :: Regex
    loop str = case matchOnceText re str of
                 Nothing -> [str]
                 Just (firstline, _, remainder)
                   | null remainder -> [firstline,""]
                   | otherwise      -> firstline : loop remainder







fst3 (x,_,_) = x
snd3 (_,x,_) = x
trd3 (_,_,x) = x

fst4 (x,_,_,_) = x
snd4 (_,x,_,_) = x
trd4 (_,_,x,_) = x
fth4 (_,_,_,x) = x





















tr set1 set2 x = maybe x (set2!!) (findIndex (==x) set1)

trfun f g x = if f x then g x else x

delrep _ [] = []
delrep f (x:xs) | f x = x : delrep f (dropWhile (==x) xs)
                | otherwise = x : delrep f xs

delund ('_':xs@('.':_)) = delund xs
delund ('_':xs@('-':_)) = delund xs
delund ('.':'_':xs) = delund ('.':xs)
delund ('-':'_':xs) = delund ('-':xs)
delund (x:xs) = x : delund xs
delund [] = []

normalizeFileName fileName =
  (\xs -> if null xs then "_" else xs) $
  (\xs -> case xs of { '-':ys -> ys; _ -> xs }) $
  delund $
  delrep (\x -> x == ' ' || x == '_') $
  map (\x -> if isAlphaNum x || elem x "._-" then x else '_') $
  map (tr "àáâãäåèéêëìíîïòóôõöùúûü" "aaaaaaeeeeiiiiooooouuuu") $
  map (tr "çñß¢Ð£Øø§µÝý¥¹²³" "cnbcdloosuyyy123") $
  map (trfun isUpper toLower) $
  filter (\x -> x /= '"' && x /= '\'') $
  trim $
  map (trfun isSpace (const ' ')) $
  fileName


buildFileName dir file ext =
  go "" 1
  where
    go complement counter =
      do let fileName = addExtension (combine dir (file ++ complement)) ext
         fileExist <- doesFileExist fileName
         if fileExist
         then go ('_' : show counter) (counter + 1)
         else do dirExist <- doesDirectoryExist fileName
                 if dirExist
                 then go ('_' : show counter) (counter + 1)
                 else return fileName









-- | A platform string is a string value from or for the operating system,
-- such as a file path or command-line argument (or environment variable's
-- name or value ?). On some platforms (such as unix) these are not real
-- unicode strings but have some encoding such as UTF-8. This alias does
-- no type enforcement but aids code clarity.
type PlatformString = String

-- | Convert a possibly encoded platform string to a real unicode string.
-- We decode the UTF-8 encoding recommended for unix systems
-- (cf http://www.dwheeler.com/essays/fixing-unix-linux-filenames.html)
-- and leave anything else unchanged.
fromPlatformString :: PlatformString -> String
fromPlatformString s = if UTF8.isUTF8Encoded s then UTF8.decodeString s else s

-- | Convert a unicode string to a possibly encoded platform string.
-- On unix we encode with the recommended UTF-8
-- (cf http://www.dwheeler.com/essays/fixing-unix-linux-filenames.html)
-- and elsewhere we leave it unchanged.
toPlatformString :: String -> PlatformString
toPlatformString = case os of
                     "unix" -> UTF8.encodeString
                     "linux" -> UTF8.encodeString
                     "darwin" -> UTF8.encodeString
                     _ -> id


os :: String
os = "linux"




fromFilePath s = if UTF8.isUTF8Encoded s
                 then UTF8.decodeString s
                 else s

toFilePath = case os of
               "unix" -> UTF8.encodeString
               "linux" -> UTF8.encodeString
               "darwin" -> UTF8.encodeString
               _ -> id









readM :: (Monad m, Read a) => String -> m a
readM s =
  case [x | (x,t) <- reads s, ("","")  <- lex t] of
    [x] -> return x
    []  -> fail "readM: no parse"
    _   -> fail "readM: ambiguous parse"




fullUrl url [] = url
fullUrl url vars = url ++ '?':(urlEncodeVars vars)


submitGETForm cookies url vars =
  do let url' = escapeURIString isAllowedInURI (fullUrl url vars)
     openURIString cookies url'





-- Authenticate in a site using a form and the POST method

authenticatePOST uri user pass =
  case mkPostRequest uri [("username",user),("password",pass)] of
    Left er   -> return $ Left er
    Right req -> do cookies <- browse $ do setAllowRedirects True
                                           request req
                                           getCookies
                    return $ Right $ cookies








class MyStringLike a where
  slReadFile :: FilePath -> IO a
  slWriteFile :: FilePath -> a -> IO ()

instance MyStringLike String where
  slReadFile = readFile
  slWriteFile = writeFile

instance MyStringLike LB.ByteString where
  slReadFile = LB.Char8.readFile
  slWriteFile = LB.Char8.writeFile

cacheDownload
  :: MyStringLike s =>
     IO (Either a s)
     -> String
     -> [(String, String)]
     -> IO (Either a s)
cacheDownload download url vars =
  do let cacheDir = "cache"
     res <- try (createDirectoryIfMissing True cacheDir)
     case res of
       Left ex ->
         do putStrLn (show (ex::IOException))
            download
       Right _ ->
         do let cacheFilePath = combine cacheDir ("cache." ++ replace "/" ":" (fullUrl url vars))
            let doDownload =
                  do src <- download
                     case src of
                       Left _ ->
                         return ()
                       Right doc ->
                         do putStr ("> Writing: " ++ cacheFilePath)
                            res <- try (slWriteFile cacheFilePath doc)
                            putStrLn " ."
                            case res of
                              Left ex -> putStrLn (show (ex::IOException))
                              Right _ -> return ()
                     return src
            exists <- doesFileExist cacheFilePath
            if exists
            then do putStr ("> Reading: " ++ cacheFilePath)
                    res <- try (slReadFile cacheFilePath)
                    putStrLn " ."
                    case res of
                      Left ex ->
                        do putStrLn (show (ex::IOException))
                           doDownload
                      Right x ->
                        return (Right x)
            else doDownload



-- | Handle errors.
handleE :: Monad m => (e -> m a) -> Either e a -> m a
handleE h (Left err) = h err
handleE _ (Right val) = return val




anyM p (x:xs) =
  do res <- p x
     if res
     then return res
     else anyM p xs
anyM _ [] = return False




ifM p a b =
  do x <- p
     if x then a else b





errHead f [] = error ("Empty list in " ++ f)
errHead _ (x:_) = x







date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = getCurrentTime >>= return . toGregorian . utctDay



-- Typically chop is called with some function that will consume an
-- initial prefix of the list and produce a value and the rest of the
-- list. The function is clearly related to unfoldr, but I find it more
-- convenient to use in a lot of cases.
--
-- In many cases it is used to chop a list into smaller pieces.
--
-- From Lennart Augustsson <lennart@augustsson.net> in
-- libraries@haskell.org, on 13 Dec 2010
--
chop :: ([a] -> (b, [a])) -> [a] -> [b]
chop _ [] = []
chop f as = b : chop f as'
  where (b, as') = f as








capitalise x =
  unwords (map capWord (words x))
  where
    capWord []     = []
    capWord (x:xs) = toUpper x : map toLower xs










tailDropWhile :: (a -> Bool) -> [a] -> [a]
tailDropWhile p xs = go id xs
  where
    go _ []          = []
    go k (x:xs)
         | p x       = go (k . (x:)) xs
         | otherwise = k (x : go id xs)




-- look up in an association list where the key is the second element of
-- the association, and not the first as is usual with lookup
lookup2 _ [] = Nothing
lookup2 x ((v,k):assocs) | x == k = Just v
                           | otherwise = lookup2 x assocs



-- Add common utility variants of trace
-- http://hackage.haskell.org/trac/ghc/ticket/7626

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just x) f = f x
whenJust Nothing _ = return ()


traceId :: String -> String
traceId x = trace x x

traceM :: (Monad m) => String -> m ()
traceM msg = trace msg $ return ()

traceShowId :: (Show a) => a -> a
traceShowId a = trace (show a) a

traceShowM :: (Show a, Monad m) => a -> m ()
traceShowM = traceM . show

traceMsgId :: Show a => String -> a -> a
traceMsgId msg x = trace (msg ++ show x ++ "\n") x



