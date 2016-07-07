module Main where

import Text.HTML.Download (openURL)
import Text.HTML.TagSoup (parseTags)

main = do text <- openURL "http://sequence.complete.org/node/feed"
          tags <- parseTags text
          print tags
