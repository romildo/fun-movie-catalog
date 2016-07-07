{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import System (exitWith, ExitCode(ExitSuccess))
import System.Environment (getProgName, getArgs)
import System.FilePath (takeFileName, splitExtension, combine)
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents, createDirectoryIfMissing)
import Text.Regex.PCRE ((=~))
import Text.Printf (printf)
import Data.Char (isSpace, toLower)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (intersperse)
import Data.ByteString (hPut)
import System.FilePath (takeExtension)
import System.IO (withFile, IOMode(WriteMode), hFlush, stdout)
import Network.URI (isAllowedInURI, escapeURIString)
import Network.Curl.Download (openURI)
import Text.HTML.TagSoup (parseTags)
import Text.Parsec (parse)
import Control.Monad (forM, forM_)
import Text.HTML.TagSoup (Tag)
import System.Console.ANSI
import System.Console.GetOpt
import Data.Array.IArray ((//))
import qualified Codec.Binary.UTF8.String as UTF8

import Movie (Info(Title,Cover,File), Movie(..), Col(..), infoBounds, emptyInfo, emptyMovie, emptyCol)
import SiteConfig (SiteConfig(..))
import EPipoca (ePipoca)
import IMDB (imdb)
import AdoroCinema (adoroCinema)
import Interfilmes (interFilmes)
import DVDWorld (dvdWorld)
import AllCenter (allCenter)
import VideoNorte (videoNorte)
import CapasBrasil (capasbrasil)
import Manicomio (manicomio)
import TagSoupParsec (TagParser(..))
import Util (trim, openURIString, buildFileName, normalizeFileName, matchREs, fromFilePath, readM)
import Latex (latexMovie, latexA, latexZ)
import Search


data Options =
  Options
  { optSites :: [SiteConfig]
  , optInputs :: [FilePath]
  , optOutput :: FilePath
  , optImgDir :: FilePath
  , optBlank :: Bool
  , optDefault :: Bool
  }

defaultOptions =
  Options
  { optSites = []
  , optInputs = []
  , optOutput = "movies.tex"
  , optImgDir = "images"
  , optBlank = False
  , optDefault = False
  }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option ['V'] ["version"] (NoArg showVersion) "show version number"
  , Option ['s'] ["site"   ] (ReqArg addSite "STRING") "input directory"
  , Option ['o'] ["output" ] (ReqArg output "FILE") "output file"
  , Option ['i'] ["imgdir" ] (ReqArg (\x o -> return o{optImgDir=x}) "FILE") "image directory"
  , Option ['d'] ["default"] (NoArg  (\o -> return o{optDefault=True})) "accept default answers"
  ]

showVersion _ =
  do putStrLn "MovieCatalog 0.1"
     exitWith ExitSuccess

addSite arg opt@Options{optSites} =
  case arg of
    "imdb"        -> return opt{optSites = optSites ++ [imdb]}
    "adorocinema" -> return opt{optSites = optSites ++ [adoroCinema]}
    "epipoca"     -> return opt{optSites = optSites ++ [ePipoca]}
    "interfilmes" -> return opt{optSites = optSites ++ [interFilmes]}
    "dvdworld"    -> return opt{optSites = optSites ++ [dvdWorld]}
    "allcenter"   -> return opt{optSites = optSites ++ [allCenter]}
    "videonorte"  -> return opt{optSites = optSites ++ [videoNorte]}
    "capasbrasil "-> return opt{optSites = optSites ++ [capasbrasil]}
    "manicomio "  -> return opt{optSites = optSites ++ [manicomio]}
    _             -> do putStrLn ("unknown site: " ++ arg)
                        return opt

output arg opt =
  return opt{optOutput = arg}


searchDirs _ _ [] = return []
searchDirs opts exts (f:fs) =
  do exists <- doesFileExist f
     if exists
     then do let ext = takeExtension f
             if elem (map toLower ext) exts
                then do xs <- searchDirs opts exts fs
                        return (f:xs)
                else searchDirs opts exts fs
     else do exists <- doesDirectoryExist f
             if exists
             then do fs' <- getDirectoryContents f
                     let fs'' = filter (\f -> f /= "." && f /= "..") fs'
                     let fs''' = map (combine f) fs''
                     searchDirs opts exts (fs''' ++ fs)
             else searchDirs opts exts fs



main =
  do prog <- getProgName
     args <- getArgs
     opts <- case getOpt Permute options args of
               (actions, others, []) -> foldl (>>=) (return defaultOptions{optInputs=others}) actions
               (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
                 where
                   header = "Usage: " ++ prog ++ " [OPTION...] FILE..."
     filePaths <- searchDirs opts [".mkv",".avi"] (optInputs opts)
     movies <- forM (zip [1::Int ..] filePaths) (getMovie (optSites opts) (optDefault opts) (optImgDir opts))
     let col = emptyCol{titles = map f (zip [1::Int ..] movies)}
     writeFile (optOutput opts) (show col)
  where
    f (id,xs) = (emptyMovie id){info = emptyInfo // xs}

