{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}

module CapasBrasil where

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
import Util (trimLeft, trim, stripSuffix, replace, submitGETForm, matchRE, lookup2, tracefun, mkPostRequest, matchRegex, postURIString, authenticatePOST)

import Movie (Info(..), defaultGenres, translateCountry)
import SiteConfig



capasbrasil =
  SiteConfig
  { siteName           = "CapasBrasil"
  , cookies            = []
  , authenticate       = Just auth
  , searchFormUrl      = baseURL ++ "/index.php?app=core&module=search&do=search&fromMainBar=1"
  , searchFormData     = \title -> [("search_app","gallery"), ("search_term",encodeString UTF8 title)]
  , searchFormMethod   = postURIString
  , searchPreprocessor = id
  , searchNextPages    = const []
  , urlForMovie        = buildURL
  , parserForSearch    = search
  , parsersForInfo     = movieParsers
  }

baseURL = "http://www.capasbrasil.com.br"

buildURL url1 section =
  url1 ++ section

auth user pass =
  authenticatePOST
    (baseURL ++ "/index.php?app=core&module=global&section=login&do=process")
    user
    pass

search =
  many (try searchOne <|> parserZero)

searchOne =
  skipTo $ do [url,title] <- reTag "<a href=(.+) title=(.+)>" id
              img <- reTag "<img class=galattach src=(.+)>" head
              tag "</img>"
              tag "</a>"
              return $ SR1 { sr1Title    = title
                           , sr1Original = ""
                           , sr1Year     = ""
                           , sr1Country  = ""
                           , sr1Genres   = ""
                           , sr1Media    = ""
                           , sr1URL      = url
                           , sr1Image    = Just img
                           }

movieParsers =
  [ ( ""
    , [ reTag "<a class=gal href=(.+)>" head >>= ret Cover
      ]
    )
  ]
