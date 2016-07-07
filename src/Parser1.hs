module Parser where

import System.Environment (getProgName, getArgs)
import System.IO (openFile, IOMode(..), hGetContents)
import System.IO.Error (catch)
import Data.Char (isSpace,isDigit)
import Data.List (stripPrefix,partition)
import Text.Regex (mkRegex, matchRegex)

isWhiteSpace x =
    x == ' ' || x == '\t'

isNewline x =
    x == '\n' && x == '\r'

isBlank =
    all isWhiteSpace

trimLeft =
    dropWhile isSpace

trimRight =
    reverse . trimLeft . reverse

trim =
    trimRight . trimLeft

collect f =
    goCollect []
    where
      goCollect ys lines = case f lines of
                             Just (y,lines') -> goCollect (y:ys) lines'
                             Nothing -> (reverse ys,lines)

collectFold f g xs =
    goCollectFold xs
    where
      goCollectFold ys lines = case f lines of
                                 Just (y,lines') -> goCollectFold (g y ys) lines'
                                 Nothing -> (ys,lines)

parseCollection lines =
    (gs,ls,cs,ts,lines'''')
    where
      (gs,lines') = parseSection "genres" lines
      (ls,lines'') = parseSection "languages" lines'
      (cs,lines''') = parseSection "audiocompressions" lines''
      (ts,lines'''') = parseTitles lines'''

skipBlankLines =
    dropWhile (\(_,xs) -> all isSpace xs)


parseTitles =
    collect parseTitle

parseTitle lines =
    case skipBlankLines lines of
      (n,xs):lines' ->
          case matchRegex (mkRegex "^[ \t]*\\[[ \t]*title[ \t]+([0-9]+)[ \t]*\\][ \t]*$") xs of
            Just [id] -> Just (collectFold parseTitleField insertField [[("id",id)]] lines')
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


parseTitleField lines =
    case skipBlankLines lines of
      (n,xs):lines' ->
          case break (== '=') xs of
            (a,'=':b) ->
                let (b',lines'') = parseRestField [b] lines'
                in Just ((trim a,trim b'),lines'')
            _ -> Nothing
      _ -> Nothing
    where
      parseRestField xs ((n,' ':ys):lines) = parseRestField (ys:xs) lines
      parseRestField xs lines = (unlines (reverse xs),lines)

parseSection name lines =
    case skipBlankLines lines of
      (n,xs):lines' ->
          case dropWhile isSpace xs of
            '[':xs' ->
                case stripPrefix name (dropWhile isSpace xs') of
                  Just xs'' ->
                      case dropWhile isSpace xs'' of
                        ']':xs''' ->
                            if all isSpace xs'''
                            then collect parseSectionItem lines'
                            else ([],lines)
                        _ -> ([],lines)
                  _ -> ([],lines)
            _ -> ([],lines)
      _ -> ([],lines)

parseSectionItem lines =
    case skipBlankLines lines of
      (n,xs):lines' ->
          case break (=='/') xs of
            (a,'/':b) -> Just ((trim a,trim b),lines')
            (a,"") -> case trimLeft a of
                        '[':_ -> Nothing
                        a' -> Just ((trimRight a',""),lines')
            _ -> Nothing
      _ -> Nothing

numberItems xs =
    goNumberItems xs 1
    where
      goNumberItems [] _ = []
      goNumberItems (x:xs) n = (n,x) : goNumberItems xs (n+1)

readCollection fileName =
    do handle <- openFile fileName ReadMode
       xs <- hGetContents handle
       return (parseCollection (numberItems (lines xs)))

main = do
  args <- getArgs
  case args of
    [fileName] ->
        readCollection fileName >>= print
    _ ->
        do progName <- getProgName
           putStr "Usage: "
           putStr progName
           putStrLn " <file name>"
