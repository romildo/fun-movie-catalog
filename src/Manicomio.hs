{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}

module Manicomio where

import Prelude hiding (div,lex,span)
import Control.Applicative ((<$>),(<*>),(<*),(*>))
import Debug.Trace (trace)
import Text.HTML.TagSoup (parseTags, renderTags, sections, partitions, (~==), (~/=), Tag(TagText,TagOpen,TagClose,TagComment), fromTagText, innerText)
import Text.Printf ()
import Text.Regex.TDFA ((=~))
import Data.Maybe (fromMaybe, catMaybes)
import Data.Char (isSpace)
import Data.List (stripPrefix, intercalate)
import Data.List.Split (wordsBy)
import Text.Parsec
import Text.StringLike (strConcat)
import Data.Encoding (encodeString)
import Data.Encoding.UTF8 (UTF8(UTF8))
import Network.Browser (getCookies, request, browse, setAllowRedirects)

import Graphics.UI.Gtk hiding (Release)

import TagSoupParsec
import Util (trimLeft, trim, stripSuffix, replace, submitGETForm, matchRE, lookup2, tracefun, mkPostRequest, matchRegex)

import Movie (Info(..), defaultGenres, translateCountry)
import SiteConfig



manicomio =
  SiteConfig
  { siteName           = "ManicomioShare"
  , cookies            = []
  , authenticate       = Just auth
  , searchFormUrl      = baseURL ++ "/pesquisa.php"
  , searchFormData     = \title -> [("cat","127"), ("busca",encodeString UTF8 title)]
  , searchFormMethod   = submitGETForm
  , searchPreprocessor = id
  , searchNextPages    = const []
  , urlForMovie        = buildURL
  , parserForSearch    = search
  , parsersForInfo     = movieParsers
  }

baseURL = "http://www.manicomio-share.com"

buildURL url1 section =
  url1 ++ section

auth user pass =
  case mkPostRequest (baseURL ++ "/account-login.php")  [("username",user),("password",pass)] of
    Left er -> return $ Left er
    Right req -> do cookies <- browse $ do setAllowRedirects True
                                           request req
                                           getCookies
                    return $ Right $ cookies

search =
  many (try searchOne <|> parserZero)

searchOne =
  do skipTo $ reTag "<tr data-id=[0-9]+>" id
     [title, url] <- skipTo $ reTag "<a title=(.+) href=(.+)>" id
     let t:_:rest = case title =~ "([^[]+)( \\[([^]]+)\\])*( .+)?" :: [[String]] of
                      (_:ys):_ -> ys
                      [] -> title:"":[]
     return $ SR1 { sr1Title    = t
                  , sr1Original = ""
                  , sr1Year     = ""
                  , sr1Country  = ""
                  , sr1Genres   = ""
                  , sr1Media    = intercalate "; " (filter (not . null) (map trim rest))
                  , sr1URL      = url
                  , sr1Image    = Nothing
                  }

movieParsers =
  [ ]
