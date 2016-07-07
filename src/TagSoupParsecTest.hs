module Main(main) where

import Network.HTTP
import TagSoupParsec
import Text.HTML.TagSoup

pHaskellCounter = do skipTo $ tag "<li id=viewcount>"
                     x <- reTag ".* (.+) times\\." id
                     tag "</li>"
                     return x

openURL x = getResponseBody =<< simpleHTTP (getRequest x)

main = do src <- openURL "http://www.haskell.org/haskellwiki/Haskell"
          let tags = parseTags src
              count = tagParse pHaskellCounter tags
          putStrLn (show count)
