{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}

module IMDB where

import Prelude hiding (div,lex,span)
import Control.Applicative ((<$>),(<*>),(<*),(*>))
import Control.Monad (forM_)
import Debug.Trace (trace)
import Text.HTML.TagSoup (parseTags, renderTags, sections, partitions, (~==), (~/=), Tag(TagText,TagOpen,TagClose,TagComment), fromTagText, innerText)
import Text.Printf (printf)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Char (isSpace)
import Data.List (stripPrefix, intercalate)
import Data.List.Split (wordsBy)
import Text.Parsec
import Text.StringLike (strConcat)
import Data.Encoding (encodeString)
import Data.Encoding.UTF8 (UTF8(UTF8))

import Graphics.UI.Gtk hiding (Release)

import TagSoupParsec
import Util (trimLeft, trim, stripSuffix, replace, submitGETForm, matchRE, lookup2, tracefun, traceMsgId)

import Movie (Info(..), defaultGenres, translateCountry)
import SiteConfig



imdb =
  SiteConfig
  { siteName           = "IMDB"
  , cookies            = []
  , authenticate       = Nothing
  , searchFormUrl      = baseURL ++ "/find"
  , searchFormData     = \title -> [("s","tt"), ("q",encodeString UTF8 title)]
  , searchFormMethod   = submitGETForm
  , searchPreprocessor = id
  , searchNextPages    = const []
  , urlForMovie        = buildURL
  , parserForSearch    = search
  , parsersForInfo     = movieParsers
  }

baseURL = "http://www.imdb.com"

buildURL url1 section =
  url1 ++ section -- ++ "/"

search =
  many (try searchOne <|> try searchOneAlt <|> parserZero)

searchOne =
  skipTo $ do lex $ tag "<td class=primary_photo>"
              img <- lex $ a (reTag "<img src=(.+)>" head <* tag "</img>")
              lex $ tag "</td>"
              lex $ tag "<td class=result_text>"
              url <- lex $ reTag "<a href=(/title/tt[0-9]+)/.*>" head
              title <- tagText ""
              lex $ tag "</a>"
              [_,version,year,_,cat] <- reTag " *(\\(([^()]+)\\) *)?\\(([0-9]+)\\) *(\\(([^()]+)\\) *)?" id
              if cat == "Video Game"
                then parserZero
                else return $ SR1 { sr1Title    = title
                                  , sr1Original = ""
                                  , sr1Year     = year
                                  , sr1Country  = version -- TODO: add a version column ?????
                                  , sr1Genres   = ""
                                  , sr1Media    = cat
                                  , sr1URL      = buildURL baseURL url
                                  , sr1Image    = Just img
                                  }

searchOneAlt =
  do url <- skipTo $ reTag "<link rel=canonical href=http://www.imdb.com(/title/tt[0-9]+)/>" head
     skipTo $ tag "<h1>"
     title <- trim <$> tagText ""
     optional $ try $ lex $ innerTag "span" [] $ tagText ""
     year <- lex $ innerTag "span" [] $ between (tagText "") (tagText "") $
               innerTag "a" [] $ tagText ""
     return $ SR1 { sr1Title    = title
                  , sr1Original = ""
                  , sr1Year     = year
                  , sr1Country  = ""
                  , sr1Genres   = ""
                  , sr1Media    = ""
                  , sr1URL      = buildURL baseURL url
                  , sr1Image    = Nothing
                  }

movieParsers =
  [ ( "/"
    , [ do url <- skipTo $ reTag "<link rel=canonical href=(.+)>" head
           skipTo $ lex $ tag "<h1>"
           -- title <- trim <$> tagText ""
           title <- lex $ innerTag "span" [("itemprop","name")] $ tagText ""
           year <- try ( -- example: http://www.imdb.com/title/tt1470171
                         lex $ innerTag "span" [] $ reTag "[ \n]*\\([^0-9]*([0-9]+)\\)" head
                       )
                   <|>
                   try ( -- common case
                         lex $ innerTag "span" [] $ between (tagText "") (tagText "") $
                           innerTag "a" [] $ tagText ""
                       )
                   <|>
                       ( -- not so common case
                         do lex $ innerTag "span" [] $ tagText ""
                            lex $ innerTag "span" [] $ between (tagText "") (tagText "") $
                              innerTag "a" [] $ tagText ""
                       )
           orig <- option title $
             do tag "<br>" >> tag "</br>"
                tag "<span>"
                trim <$> reTagText "\n *\\\"(.*)\\\"\n" head
           return [(IMDB,url), (Title,title), (Year,year), (OriginalTitle,orig)]

      -- , do skipTo $ reTagText "\n[ ]*Ratings: " id
      --      r <- innerText <$> many1 (notCloseTag "a")
      --      let [a,_,"from",b,"users"] = wordsBy (\x -> x == ' ' || x == '/') r
      --      return [(Rating, intercalate ":" ["IMDB",a,b])]

      , do skipTo $ reTagText "[ \n]*Ratings:[ \n]*" id
           a <- skipTo $ innerTag "span" [("itemprop","ratingValue")] $ tagText ""
           b <- skipTo $ innerTag "span" [("itemprop","ratingCount")] $ tagText ""
           return [(Rating, intercalate ":" ["IMDB",a,b])]

      -- , do skipTo $ lex $ h4 $ lex $ reTagText "[ \n]*Director:[ \n]*" id
      --      ds <- sepBy1 (a $ tagText "") (tagText ",")
      --      return [(Director, intercalate ", " ds)]

      , do skipTo $ lex $ h4 $ lex $ reTagText "[ \n]*Director(s)?:[ \n]*" id
           xs <- sepBy1 (innerTag "span" [("itemprop","director")] $
                           lex0 $ innerTag "span" [("itemprop","name")] $
                             lex0 $ innerTag "a" [] $
                               tagText ""
                        )
                        (reTagText ",[ \n]*" id)
           return [(Director, intercalate ", " xs)]

      -- , do skipTo $ lex $ h4 $ lex $ reTag "[ \n]*Stars:[ \n]*" id
      --      ds <- sepBy1 (lex $ a $ tagText "") (choice [tagText ", ", tagText " and "])
      --      return [(Stars, intercalate ", " ds)]

      , do skipTo $ lex $ h4 $ lex $ reTagText "[ \n]*Stars:[ \n]*" id
           xs <- sepBy1 (innerTag "span" [("itemprop","actors")] $
                           lex0 $ innerTag "span" [("itemprop","name")] $
                             lex0 $ innerTag "a" [] $
                               tagText ""
                        )
                        (choice [reTagText ",[ \n]*" show, tagText " and "])
           return [(Stars, intercalate ", " xs)]

      , cast >>= ret Cast

      , do skipTo $ tagText "Technical Specs"
           skipTo $ tagText "Runtime:"
           len <- skipTo $ innerTag "time" [] $ reTagText "(.+) min" head
           return [(Length,len)]

      , skipTo $ do lex $ tag "<td id=img_primary>"
                    lex $ tag "<div class=image>"
                    lex $ tag "<a>"
                    cover <- reTag "<img src=(.*)>" head
                    return [(Cover,cover)]

      , do skipTo $ lex $ tagText "Genres:"
           gs <- innerText <$> many1 (notCloseTag "div")
           let translate x = fromMaybe x (lookup2 x defaultGenres)
           let genres = intercalate ", " (map (translate . trim) (wordsBy (=='|') gs))
           return [(Genres,genres)]

      -- , do skipTo $ lex $ innerTag "h2" [] $ tagText "Storyline"
      --      lex $ tag "<div>"
      --      tag "<p>"
      --      summary <- tagText ""
      --      return [(Synopsis,summary)]

      -- , do skipTo $ tag "<span itemprop=contentRating>"
      --      mpaa <- tagText ""
      --      tag "</span>"
      --      return [(Certification,mpaa)]

      -- , do date <- skipTo $ reTag "<time itemprop=datePublished datetime=(.+)>" head
      --      tagText ""
      --      tag "</time>"
      --      country <- reTag "[ \n]*\\((.*)\\)[ \n]*" head
      --      if country == "Brazil" || country == "Brasil"
      --        then return [(Release, translateCountry country ++ ":" ++ date)]
      --        else parserZero

      , do skipTo $ h4 $ tagText "Release Date:"
           [day,month,year,country] <- reTagText " *([0-9]+) ([a-zA-Z]+) ([0-9]+) \\((.+)\\)[ \n]*" id
           if country == "Brazil" || country == "Brasil"
             then return [(Release, printf "%s:%04s-%02s-%02s" (translateCountry country) year (monthNumber month) day)]
             else parserZero

      , do skipTo $ tagText "Country:"
           cs <- innerText <$> many1 (notCloseTag "div")
           return [(Country, intercalate ", " (map (translateCountry . trim) (wordsBy (=='|') cs)))]
      ]
    )
  , ( "/plotsummary"
    , [ skipTo $ lex $ innerTag "p" [("class","plotpar")] $
          do summary <- tagText ""
             (_,x,_) <- lex $ wholeTag "i" []
             return [(Synopsis, trim summary ++ "\n\n(" ++ trim (innerText x) ++ ")")]
      ]
    )
  , ( "/parentalguide"
    , [ do let section header = do skipTo $ innerTag "span" [] $ tagText header
                                   (_,ts,_) <- skipTo $ wholeTag "p" []
                                   return $ renderTags $ (TagOpen "h3" []:TagText header:TagClose "h3":ts)
           sex <- section "Sex & Nudity"
           vio <- section "Violence & Gore"
           pro <- section "Profanity"
           alc <- section "Alcohol/Drugs/Smoking"
           fri <- section "Frightening/Intense Scenes"
           return [(ParentalGuide,sex ++ vio ++ pro ++ alc ++ fri)]

      , do skipTo $ lex $ innerTag "h5" [] $ do innerTag "a" [] $ tagText "MPAA"
                                                tagText ":"
           mpaa <- innerTag "div" [] $ tagText ""
           return [(Certification,trim mpaa)]
      ]
    )
  ]


cast =
  do skipTo $ lex $ h2 $ tagText "Cast"
     _:tab <- table
     return $ intercalate "\n" $ filter (not . null) $
       flip map tab $ \row ->
         case row of
           [_,a,_,b] ->
             let name = trim (innerText a)
                 character = intercalate ", " (map trim (wordsBy (=='/') (innerText b)))
                 character' = unwords (filter (not . null) (map trim (lines character)))
             in name ++ " (" ++ character' ++ ")"
           _ ->
             ""

myInnerText ts =
  strConcat (go ts)
  where
    go [] = []
    go (TagText x:ts) = x : go ts
    go (TagOpen "br" _:TagClose "br":ts) = "\n" : go ts
    go (_:ts) = go ts


spanGroups _ [] = []
spanGroups f (x:xs) = (x:as) : spanGroups f bs
  where
    (as,bs) = break f xs


monthNumber "January"   = "01"
monthNumber "February"  = "02"
monthNumber "March"     = "03"
monthNumber "April"     = "04"
monthNumber "May"       = "05"
monthNumber "June"      = "06"
monthNumber "July"      = "07"
monthNumber "August"    = "08"
monthNumber "September" = "09"
monthNumber "October"   = "10"
monthNumber "November"  = "11"
monthNumber "December"  = "12"
