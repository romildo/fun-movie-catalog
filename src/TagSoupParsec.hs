{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module TagSoupParsec where

import Prelude hiding (lex)
-- import Text.HTML.TagSoup.Parsec
import Control.Applicative ((<$>))
import Debug.Trace (trace)
import Text.Parsec hiding (satisfy)
import Text.HTML.TagSoup (Tag(TagText,TagOpen,TagClose,TagComment), (~==), parseTags, TagRep(toTagRep), innerText)
import Control.Monad.Identity (Identity)
import Control.Applicative ((<*),(*>))
import Text.StringLike (StringLike, strNull, fromString, toString, castString)
import Data.Char (toLower, isSpace)
import Data.Array (elems)
import Text.Regex.Base (RegexMaker(makeRegex), RegexLike(matchOnceText))
import Text.Regex.PCRE (Regex)
import qualified Data.ByteString.Char8 as B

import Util (matchRE)

type TagParser str a = Parsec [Tag str] () a


tagParse :: TagParser str a -> [Tag str] -> a
tagParse p ts =
  either ( error . show ) id $ parse p "tagsoup" ts

tagEater :: Show str => (Tag str -> Maybe a) -> TagParser str a
tagEater matcher =
  tokenPrim show (\pos t ts -> incSourceLine pos 1) matcher

anyTag :: Show str => TagParser str (Tag str)
anyTag = tagEater Just

satisfy :: Show str => (Tag str -> Bool) -> TagParser str (Tag str)
satisfy f =
  tagEater (\t -> if f t then Just t else Nothing)



tag :: (Show str, StringLike str, TagRep a) => a -> TagParser str (Tag str)
-- tag t = satisfy (~== t) <?> show t
tag t =
  tagEater (f (toTagRep t))
  where
    f (TagText x) u@(TagText y) | strNull x || x == y = Just u
                                | otherwise = Nothing
    f (TagClose x) u@(TagClose y) | strNull x || lowercase x == lowercase y = Just u
                                  | otherwise = Nothing
    f (TagOpen x xs) u@(TagOpen y ys) | (strNull x || lowercase x == lowercase y) && all g (lowercaseAL xs) = Just u
                                      | otherwise = Nothing
      where
        ys' = lowercaseAL ys
        g pair@(name,val) | strNull name = elem val (map snd ys')
                          | strNull val = elem name (map fst ys')
                          | otherwise = elem pair ys'
    f _ _ = Nothing

lowercaseAL :: StringLike a => [(a, b)] -> [(a, b)]
lowercaseAL assocs = map (\(name,val) -> (lowercase name, val)) assocs

tagText :: (Show str, StringLike str) => str -> TagParser str str
tagText str =
  tagEater $ \t -> case t of
                     TagText x | strNull str || x == str -> Just x
                     _ -> Nothing

skipTo :: Show str => TagParser str a -> TagParser str a
skipTo p = try p <|> (anyTag >> skipTo p)

parseText str p =
  do xs <- tagText str
     either (const parserZero) return $ parse p "tagsoup intenal" xs

reTagText re f =
  tagEater $ \t -> case t of
                     TagText text ->
                       case matchOnceText (makeRegex (B.pack re) :: Regex) (B.pack text) of
                         Just (_,m,_) -> Just (f (tail (map (B.unpack . fst) (elems m))))
                         Nothing -> Nothing
                     _ -> Nothing


reTag :: String -> ([String] -> a) -> TagParser String a
reTag re f =
  tagEater (fmap f . g (toTagRep re))
  where
    g (TagText x)    (TagText y)                                              = matchRE x y
    g (TagClose x)   (TagClose y)   | strNull x || lowercase x == lowercase y = Just []
    g (TagOpen x xs) (TagOpen y ys) | strNull x || lowercase x == lowercase y = h (lowercaseAL xs) []
                                    where
                                      ys' = lowercaseAL ys
                                      h ((name,re):rest) zs = case lookup name ys' of
                                                                Just val -> case matchRE re val of
                                                                              Just z -> h rest (z:zs)
                                                                              Nothing -> Nothing
                                                                Nothing -> Nothing
                                      h [] zs = Just (concat (reverse zs))
    g _ _ = Nothing


{-
myToTagRep :: String -> Tag String
myToTagRep x =
  case parseTags x of
    [a] -> a
    _ -> error $ "When using a TagRep it must be exactly one tag, you gave: " ++ x
-}





-- openTag name attribs =
--   tagEater f
--   where
--     f (TagOpen n atts)
--       | lowercase name == lowercase n = lookupAttribs [] attribs
--       where
--         lookupAttribs vs [] = Just (reverse vs)
--         lookupAttribs vs (k:ks) = case lookup k atts of
--                                     Just v -> lookupAttribs (v:vs) ks
--                                     Nothing -> Nothing
--     f _ = Nothing


-- make a string lowercase
lowercase :: StringLike s => s -> s
lowercase =
   fromString . map toLower . toString




-- from the tagsoup-parsec package

openTagMatch soughtName soughtAtrs match noMatch =
  tagEater $ \tag -> case tag of
                       t@(TagOpen tname atrs)
                         | lowercase tname == lowercase soughtName &&
                           atrsMatch (atrsToLower soughtAtrs) (atrsToLower atrs)
                           -> match t
                       t ->
                         noMatch t
  where
    atrsToLower = map (\(x,y) -> (lowercase x,y))
    atrsMatch xs1 xs2 = all (\(x1,y1) -> maybe False (y1==) (lookup x1 xs2)) xs1

closeTagMatch soughtName match noMatch =
  tagEater $ \tag -> case tag of
                       t@(TagClose tname) ->
                         if lowercase tname == lowercase soughtName
                         then match t
                         else noMatch t
                       t ->
                         noMatch t

openTag :: (Show str, StringLike str) => str -> [(str, str)] -> TagParser str (Tag str)
openTag soughtName atrs =
  openTagMatch soughtName atrs Just (\_ -> Nothing)

notOpenTag :: (Show str, StringLike str) => str -> [(str, str)] -> TagParser str (Tag str)
notOpenTag avoidName atrs =
  openTagMatch avoidName atrs (\ _ -> Nothing) Just

closeTag :: (Show str, StringLike str) => str -> TagParser str (Tag str)
closeTag soughtName =
  closeTagMatch soughtName Just (\_ -> Nothing)

notCloseTag :: (Show str, StringLike str) => str -> TagParser str (Tag str)
notCloseTag avoidName =
  closeTagMatch avoidName (\ _ -> Nothing) Just

wholeTag
  :: (Show str, StringLike str) =>
     str
     -> [(str, str)]
     -> TagParser str (Tag str, [Tag str], Tag str)
wholeTag soughtName atrs =
  do open <- openTag soughtName atrs
     ts <- many $ notCloseTag soughtName
     close <- closeTag soughtName
     return (open , ts , close)

innerTag
  :: (Show str, StringLike str) =>
     str
     -> [(str, str)]
     -> TagParser str a
     -> TagParser str a
innerTag name atrs p =
  between (openTag name atrs) (closeTag name) p












div = between (tag "<div>") (tag "</div>")

strong = between (tag "<STRONG>") (tag "</STRONG>")

par = between (tag "<p>") (tag "</p>")

h1 = between (tag "<h1>") (tag "</h1>")

h2 = between (tag "<h2>") (tag "</h2>")

h3 = between (tag "<h3>") (tag "</h3>")

h4 = between (tag "<h4>") (tag "</h4>")

a = between (tag "<a>") (tag "</a>")

ul = between (tag "<ul>") (tag "</ul>")

li = between (tag "<li>") (tag "</li>")

span = between (tag "<span>") (tag "</span>")

tr = between (tag "<tr>") (tag "</tr>")

td = between (tag "<td>") (tag "</td>")

font = between (tag "<font>") (tag "</font>")

b = between (tag "<b>") (tag "</b>")

i = between (tag "<i>") (tag "</i>")

u = between (tag "<u>") (tag "</u>")

br = tag "<br>" >> tag "</br>"

table =
  between (tag "<table>" >> many emptyTag) (tag "</table>") $
    many $
      lex $ between (tag "<tr>" >> many emptyTag) (tag "</tr>") $
        many $
          do (_,tags,_) <- lex $ wholeTag "td" []
             return $
               reverse (dropWhile isEmptyTag (reverse (dropWhile isEmptyTag tags)))

isEmptyTag (TagText xs) | all isSpace xs = True
isEmptyTag (TagComment _)                = True
isEmptyTag _                             = False


emptyTag :: TagParser [Char] ()
emptyTag =
  tagEater $ \t -> case t of
                     TagComment _ -> Just ()
                     TagText xs | all isSpace xs -> Just ()
                     _ -> Nothing


lex0 :: TagParser String a -> TagParser String a
lex0 p = many emptyTag *> p

lex :: TagParser String a -> TagParser String a
lex p = p <* many emptyTag

trimp :: TagParser String a -> TagParser String a
trimp p = many emptyTag *> p <* many emptyTag
