{-# LANGUAGE NamedFieldPuns #-}

module SiteConfig where

import Data.List.Utils (split, join)
import Network.Stream (ConnError)
import Network.Browser (Cookie)

import TagSoupParsec (TagParser)
import Movie (Info, normalGenre)
import Util (cacheDownload, trim)

data SearchResult1 =
  SR1
  { sr1Title    :: String
  , sr1Original :: String
  , sr1Year     :: String
  , sr1Country  :: String
  , sr1Genres   :: String
  , sr1Media    :: String
  , sr1URL      :: String
  , sr1Image    :: Maybe String
  }
  deriving (Show)

emptySearchResult1 =
  SR1
  { sr1Title    = ""
  , sr1Original = ""
  , sr1Year     = ""
  , sr1Country  = ""
  , sr1Genres   = ""
  , sr1Media    = ""
  , sr1URL      = ""
  , sr1Image    = Nothing
  }


type SiteName = String  -- gives better error messages

data SiteConfig =
  SiteConfig
  { siteName           :: SiteName
  , cookies            :: [Cookie]
  , authenticate       :: Maybe (String -> String -> IO (Either ConnError [Cookie]))
  , searchFormUrl      :: String
  , searchFormData     :: String -> [(String,String)]
  , searchFormMethod   :: [Cookie] -> String -> [(String,String)] -> IO (Either ConnError String)
  -- , webSearch       :: String -> IO (Maybe String)
  , searchPreprocessor :: String -> String
  , searchNextPages    :: String -> [(String,String)]
  , urlForMovie        :: String -> String -> String
  , parserForSearch    :: TagParser String [SearchResult1]
  , parsersForInfo     :: [(String,[TagParser String [(Info,String)]])]
  }

instance Show SiteConfig where
  show SiteConfig{siteName=x} = "SiteConfig(" ++ x ++ ")"




webSearch SiteConfig{searchFormUrl, searchFormData, searchFormMethod, searchPreprocessor, searchNextPages, cookies} title
  | null title' = return $ Right []
  | otherwise   = do d <- download vars
                     case d of
                       Left err -> return $ Left err
                       Right src -> downloadNextPages (searchNextPages src) [src]
  where
    title' = trim title

    vars = searchFormData title'

    download vars =
      fmap (fmap searchPreprocessor) (cacheDownload (searchFormMethod cookies searchFormUrl vars)
                                                    searchFormUrl
                                                    vars)

    downloadNextPages []     srcs = return $ Right $ concat (reverse srcs)
    downloadNextPages (p:ps) srcs = do d <- download (p:vars)
                                       case d of
                                         Left err -> return $ Left err
                                         Right src -> downloadNextPages ps (src:srcs)


ret i x = return [(i,x)]



normalizeGenres text =
  join ", " (map normalGenre (split ", " text))
