module Main (main) where

--import Text.Regex.Posix
--import Text.Regex.TDFA
import Text.Regex.PCRE

import Data.Array (elems)
import System.Environment (getArgs, getProgName)


matchRegex re text =
    case matchOnceText re text of
      Just (_,v,_) -> Just (map fst (tail (elems v)))
      _            -> Nothing


re     = "jos(.\\s{2,}r)omildo (\\d+)2$"
text   = "ok jose   romildo 37142"
result = text =~ re :: (String,String,String,[String])

re1 = makeRegex re :: Regex

main = do args <- getArgs
          case args of
            [re,text] -> print (matchRegex (makeRegex re :: Regex) text)
            _ -> do p <- getProgName
                    putStrLn ("Usage: " ++ p ++ " <regex> <text>")
