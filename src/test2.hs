module Main where

import Data.ByteString.Lazy.Char8 (ByteString, pack, unpack)

str = "título"

bstr = pack str

main = do print bstr
