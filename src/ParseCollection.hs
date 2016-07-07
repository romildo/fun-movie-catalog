module Main (main) where

import System.Environment (getProgName, getArgs)
import Data.Char (isSpace)
import Text.ParserCombinators.Parsec

fAnd p1 p2 =
    \x -> p1 x && p2 x

fOr p1 p2 =
    \x -> p1 x || p2 x

isNewline x =
    x == '\n' && x == '\r'

lineSpaces =
    skipMany (satisfy (fAnd isSpace isNewline))

pair =
    do spaces
       x <- many (satisfy (fAnd (/= '/') (not.isNewline)))
       char '/'
       lineSpaces
       y <- many (satisfy (not.isNewline))
       newline
       return (takeWhile (not.isSpace) x,takeWhile (not.isSpace) y)

parseCollection =
    parseSection2 "genres"

-- parseCollection =
--     do ls <- parseLanguages
--        gs <- parseGenres
--        cs <- parseCompressions
--        ts <- parseTitles ls gs cs
--        return (ls,gs,cs,ts)

parseSection2 name =
    do spaces >> char '[' >> lineSpaces >> string name >> lineSpaces >> char ']' >> lineSpaces >> newline
       many $ try pair

main = do
  args <- getArgs
  case args of
    [fileName] ->
        do result <- parseFromFile parseCollection fileName
           case result of
             Left err ->
                 do putStr "parse error at "
                    print err
             Right collection ->
                 print collection
    _ ->
        do progName <- getProgName
           putStr "Usage: "
           putStr progName
           putStrLn " <file name>"
