{-# LANGUAGE ExistentialQuantification #-}

module InternetSearch (internetSearch) where

import Network.URI (isAllowedInURI, escapeURIString)
import Network.Curl.Download (openURIWithOpts)
import Network.Curl.Opts (CurlOption(CurlEncoding))
import qualified Data.ByteString as B
import Text.HTML.TagSoup (parseTags, sections, (~==), Tag(TagText,TagOpen))
import Data.ByteString.Char8 (unpack)
import Title
import Component


openURIWithOptsString opts s = (fmap unpack) `fmap` openURIWithOpts opts s


internetSearch components =
  do let Just entry = lookup Title components
     title <- getText entry
     let uri = escapeURIString isAllowedInURI ("http://www.adorocinema.com/common/search/search_by_film/?criteria=" ++ title)
     src <- openURIWithOptsString [CurlEncoding "gzip"] uri
     case src of
       Left err -> print err
       Right doc -> do writeFile "test.html" doc
                       let tags = parseTags doc
                       let items = sections (~== "<div class=search-item>") tags
                       let found = search items
                       putStrLn (show found)

search [] = []
search (item:items) =
  let (TagOpen "a" xs:ks):others = sections (~== "<a class=titulofilme>") item
      (_:TagText original:_):_ = sections (~== "<p>") ks
      Just href = lookup "href" xs
      Just title = lookup "title" xs
      orig = reverse (tail (reverse (drop 15 original)))
  in (title, orig, href) : search items
