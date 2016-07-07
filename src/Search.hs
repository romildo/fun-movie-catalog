{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Search where

import System.Exit (exitWith, ExitCode(ExitSuccess))
import System.Environment (getProgName, getArgs)
import System.FilePath (takeFileName, splitExtension, combine)
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents, createDirectoryIfMissing)
import Text.Regex.PCRE ((=~))
import Text.Printf (printf)
import Data.Char (isSpace, toLower)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (intersperse)
import qualified Data.ByteString.Lazy.Char8 as LB.Char8
import System.FilePath (takeExtension)
import System.IO (withFile, IOMode(WriteMode), hFlush, stdout)
import Control.Exception (IOException,try)
import Network.URI (isAllowedInURI, escapeURIString)
-- import Network.Curl.Download (openURI)
import Text.HTML.TagSoup (parseTags)
import Text.Parsec (parse)
import Control.Monad (forM, forM_)
import Text.HTML.TagSoup (Tag)
import System.Console.ANSI
import System.Console.GetOpt
import Data.Array.IArray ((//))
import qualified Codec.Binary.UTF8.String as UTF8

import Movie (Info(Title,Cover,File), Movie(..), Col(..), infoBounds, emptyInfo, emptyMovie, emptyCol)
import SiteConfig (SearchResult1(..), SiteConfig(..), webSearch)
import TagSoupParsec (TagParser(..))
import Util (replace, trim, openURI, openURIString, cacheDownload, buildFileName, normalizeFileName, matchREs, fromFilePath, readM)
import Latex (latexMovie, latexA, latexZ)

type Result = (String,String,String,[String])

sep x = isSpace x || elem x "-_,.()[]"

words1 f s =
  case dropWhile f s of
    [] -> []
    s' -> let (w,s'') = break f s'
          in w : words1 f s''

guess text =
  listToMaybe $ matchREs
    [ ( "^(.+)\\b([0-9]{4})\\b.*$",
        \ [_,title,year] -> (unwords (words1 sep title), year) )

    , ( "(?i)^(.+?)(?:\\b" ++ ending ++ "\\b|_" ++ ending ++ "_).*$",
        \ [_,title] -> (unwords (words1 sep title), "") )

    , ( "^(.*)$",
        \ [_,title] -> (unwords (words1 sep title), "") )
    ]
    text
  where
    ending = concat $ ["(?:\\[?"] ++ intersperse "|" es ++ ["\\]?)+"]
    es = [ "Dual(?:Audio)?", "720p?", "1080p?", "BDRip", "BluRay", "HDTV", "Vers[aã]o[ \\._-]?Extendida", "Extended", "MS", "DC" ]

getMovie sites useDefault imgDir (counter,filePath) =
  do putStrLn ""
     putStrLn $ printf "%i> %s" counter filePath
     let (fileName,fileExt) = splitExtension (takeFileName filePath)
     setSGR [SetConsoleIntensity BoldIntensity]
     putStrLn fileName
     setSGR [Reset]
     case guess (fromFilePath fileName) of
       Nothing -> putStrLn "cannot guess file name" >> return []
       Just (title,year) ->
         do setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull Blue]
            putStrLn $ printf "%s [%s]" title year
            setSGR [Reset]
            movies <- forM sites (searchSite useDefault imgDir (title,""))
            return $ foldl mergeMovie [(File,filePath)] movies

searchSite useDefault imgDir (title,original) site@SiteConfig{siteName,parserForSearch,urlForMovie,parsersForInfo} =
  do res <- go title
     case res of
       [] -> go original
       _  -> return res
  where
    go title =
      do src <- webSearch site (trim title)
         case src of
           Left err ->
             do putStrLn (show err)
                return []
           Right doc ->
             do writeFile ("search.tags.html") doc
                let tags = parseTags doc
                writeFile "search.tags.hs" (unlines (map show tags))
                case parse parserForSearch "tagsoup" tags of
                  Left err -> return []
                  Right [] -> return []
                  Right xs -> do putStrLn ("Search results: " ++ siteName)
                                 chosen <- chooseSearchResult useDefault xs
                                 case chosen of
                                   Nothing -> return []
                                   Just SR1{sr1URL=uriMovie} ->
                                     do putStrLn ("==> URI: " ++ uriMovie)
                                        movie <- fmap concat $ forM parsersForInfo $ getMovieSection (urlForMovie uriMovie)
                                        movie' <- downCover imgDir movie
                                        return movie'


chooseSearchResult useDefault xs =
  do forM_ (zip [(1::Int)..] xs) showMenuItem
     putStrLn $ printf "%2i. %s" (0::Int) "Skip"
     if null xs
     then return Nothing
     else if useDefault
          then return $ Just (head xs)
          else loop
  where
    loop =
      do putStr "Choice (ENTER selects first option): "
         hFlush stdout
         line <- getLine
         if null line
         then return $ Just (xs !! 0)
         else case readM line of
                Just n | n > 0 && n <= length xs -> return $ Just (xs !! (n-1))
                       | n == 0 -> return Nothing
                _ -> loop

    showMenuItem (i,SR1{sr1Title,sr1Original,sr1Year,sr1Media}) =
      do putStr $ printf "%2i. " i
         setSGR [SetColor Foreground Dull Green]
         putStr sr1Title
         setSGR [Reset]
         putStrLn $ printf " [%s] %s (%s)" sr1Year sr1Original sr1Media


getMovieSection buildURL (section,infoParsers) =
  do let uri0 = buildURL section
     let uri = escapeURIString isAllowedInURI uri0
     src <- cacheDownload (openURIString [] uri) uri []
     case src of
       Left err ->
         do putStrLn (show err)
            return []
       Right doc ->
         do let fname = replace "/" ":" ("movie-" ++ section ++ ".tags")
            writeFile (fname ++ ".html") doc
            let tags = parseTags doc
            writeFile (fname ++ ".hs") (unlines (map show tags))
            let f :: [TagParser String [(Info,String)]]
                f = infoParsers
            fmap concat $ forM infoParsers $ parseMovieField tags

downCover imgDir xs =
  do try (createDirectoryIfMissing True imgDir) :: IO (Either IOException ())
     forM xs $ \info ->
       case info of
         (Cover,url@(_:_)) -> downloadCover imgDir (fromMaybe "_" (lookup Title xs)) url >>= return . (Cover,)
         x -> return x



downloadCover tempDir title url =
  do fileName <- buildFileName tempDir (normalizeFileName title) (takeExtension url)
     withFile fileName WriteMode $ \h ->
       do src <- cacheDownload (openURI [] url) url []
          case src of
            Left _ -> return ""
            Right xs -> do LB.Char8.hPut h xs
                           return fileName


mergeMovie xs [] = xs
mergeMovie xs (info@(i,x):ms)
  | null y = mergeMovie xs ms
  | otherwise = case lookup i xs of
                  Nothing -> mergeMovie (info:xs) ms
                  Just _ -> mergeMovie xs ms
  where
    y = trim x


parseMovieField :: [Tag String] -> TagParser String [(Info,String)] -> IO [(Info,String)]
parseMovieField tags p =
  do let res = either (const []) id (parse p "tagsoup" tags)
     -- putStrLn (show res)
     return res


printMovieInfo (i,x) =
  do setSGR [SetConsoleIntensity BoldIntensity]
     putStr (show i)
     setSGR [Reset]
     putStr " => "
     setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull Blue]
     putStrLn x
     setSGR [Reset]


