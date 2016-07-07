{-# LANGUAGE NamedFieldPuns #-}

module Parser where

import Data.Char (isSpace, isDigit)
import Data.List (stripPrefix, partition)
import System.Environment (getProgName, getArgs)

import Movie (Movie, Col(..), emptyCol, mkMovie)
import Util (trim, trimLeft, trimRight)


isWhiteSpace x =
  x == ' ' || x == '\t'

isNewline x =
  x == '\n' || x == '\r'

isBlank :: Foldable t => t Char -> Bool
isBlank =
  all isWhiteSpace

skipBlankLines :: [(Int, String)] -> [(Int, String)]
skipBlankLines =
  dropWhile (\(_,xs) -> all isSpace xs)

collect :: (t -> Maybe (a, t)) -> t -> ([a], t)
collect f =
  goCollect []
  where
    goCollect ys input = case f input of
                           Just (y,input') -> goCollect (y:ys) input'
                           Nothing -> (reverse ys,input)

collectFold :: (t -> Maybe (a, t)) -> (a -> b -> b) -> b -> t -> (b, t)
collectFold f g xs =
  goCollectFold xs
  where
    goCollectFold ys input = case f input of
                               Just (y,input') -> goCollectFold (g y ys) input'
                               Nothing -> (ys,input)

parseCollection :: [(Int, String)] -> Either String Col
parseCollection input =
  let (genres,input1)            = parseSection "genres" parseSectionItem2 input
      (languages,input2)         = parseSection "languages" parseSectionItem2 input1
      (audiocompressions,input3) = parseSection "audiocompressions" parseSectionItem1 input2
      (titles,input4)            = parseMovies input3
  in case skipBlankLines input4 of
       [] ->
         Right emptyCol{genres, languages, audiocompressions, titles}
       (pos,line):_ ->
         Left ("Error parsing collection file at line " ++ show pos ++ ":\n" ++ line)
    
parseMovies :: [(Int, String)] -> ([Movie], [(Int, String)])
parseMovies =
  collect parseMovie

parseMovie :: [(Int, String)] -> Maybe (Movie, [(Int, String)])
parseMovie input =
  case skipBlankLines input of
    (_, '[':'t':'i':'t':'l':'e':' ':xs) : input' ->
      case span isDigit xs of
        (ident@(_:_), ']':xs') | isBlank xs' ->
                                   let (list,input'') = collect parseMovieField input'
                                   in Just (mkMovie (read ident) list,input'')
        _ -> Nothing
    _ -> Nothing

insertField (key,val) fss =
  goInsertField n fss []
  where
    (k1,k2) = partition isDigit (reverse key)
    (k,n) | null k1   = (key,0)
          | otherwise = (reverse k2,(read (reverse k1))::Int)
    goInsertField n [] yss = reverse yss ++ take n (repeat []) ++ [[(k,val)]]
    goInsertField 0 (xs:xss) yss = reverse yss ++ ((k,val):xs):xss
    goInsertField n (xs:xss) yss = goInsertField (n-1) xss (xs:yss)

indexedKey key =
  let (k1,k2) = partition isDigit (reverse key)
  in if null k1 || null k2 then Nothing
     else Just (reverse k2,(read (reverse k1))::Int)

parseMovieField :: [(Int, String)] -> Maybe ((String, String), [(Int, String)])
parseMovieField input =
  case skipBlankLines input of
    (_,xs):input' ->
        case break (== '=') xs of
          (a,'=':b) ->
            let (b',input'') = parseRestField [b] input'
            in Just ((trim a,trim b'),input'')
          _ -> Nothing
    _ -> Nothing
  where
    parseRestField xs ((_,' ':ys):input) = parseRestField (ys:xs) input
    parseRestField xs input = (unlines (reverse xs),input)

parseSection
  :: String
  -> ([(Int, String)] -> Maybe (a, [(Int, String)]))
  -> [(Int, String)]
  -> ([a], [(Int, String)])

parseSection name parseSectionItem lines =
  case skipBlankLines lines of
    (_,xs):lines' ->
      case dropWhile isSpace xs of
        '[':xs' ->
          case stripPrefix name (dropWhile isSpace xs') of
            Just xs'' ->
              case dropWhile isSpace xs'' of
                ']':xs''' | all isSpace xs''' -> collect parseSectionItem lines'
                          | otherwise -> ([],lines)
                _ -> ([],lines)
            _ -> ([],lines)
        _ -> ([],lines)
    _ -> ([],lines)

parseSectionItem1 lines =
  case skipBlankLines lines of
    (_,xs):lines' -> case trimLeft xs of
                       '[':_ -> Nothing
                       a -> Just (trimRight a,lines')
    _ -> Nothing

parseSectionItem2 lines =
  case skipBlankLines lines of
    (_,xs):lines' ->
      case break (=='/') xs of
        (a,'/':b) -> Just ((trim a,trim b),lines')
        (a,"") -> case trimLeft a of
                    '[':_ -> Nothing
                    a' -> Just ((trimRight a',""),lines')
        _ -> Nothing
    _ -> Nothing

readCollection :: FilePath -> IO (Either String Col)
readCollection fileName =
  do xs <- readFile fileName
     return (parseCollection (zip [1..] (lines xs)))


main :: IO ()
main =
  do args <- getArgs
     case args of
       [fileName] ->
         do res <- readCollection fileName
            case res of
              Left msg -> error msg
              Right col -> putStrLn (show col)
       _ ->
         do progName <- getProgName
            putStr "Usage: "
            putStr progName
            putStrLn " <file name>"
