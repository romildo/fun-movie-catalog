module Main (main) where

import Text.Parsec hiding (satisfy)
import Text.HTML.TagSoup (parseTags, Tag(TagText), (~==))
-- import Text.HTML.Download (openURL)
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Data.Char (isDigit)
import Text.Regex.PCRE (Regex, makeRegex)
import Control.Applicative ((<$>))

import TagSoupParsec

main =
  do parseURI counter "http://www.haskell.org/haskellwiki/Haskell"
     -- parseURI adoroCinema "http://www.adorocinema.com/filmes/supremacia-bourne/ficha-tecnica-e-premios/"

openURL x = getResponseBody =<< simpleHTTP (getRequest x)

parseURI parser uri =
  do putStrLn ("Parsing " ++ uri)
     putStrLn "-----------------------------------------------------"
     src <- openURL uri
     let tags = parseTags src
     writeFile "tags.hs" (unlines (map show tags))
     putStrLn "-----------------------------------------------------"
     putStrLn (show (tagParse parser tags))
     putStrLn ""

counter :: TagParser String Int
counter =
  do skipTo (tag "<div id=footer>")
     skipTo (tag "<li id=viewcount>")
     read . filter isDigit <$> reTagText " ([0-9,]+) times." head

strongTag = between (tag "<STRONG>") (tag "</STRONG>")

adoroCinema =
  do skipTo $ strongTag $ tagText "roteiro:"
     reTagText "^ *(.+)$" head
