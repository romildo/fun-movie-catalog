{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}

module Interfilmes (interFilmes) where

import Prelude hiding (lex, div, span)
import Debug.Trace (trace)
import Text.HTML.TagSoup (innerText)
import Text.Printf (printf)
import Data.List (intercalate)
import Data.List.Split (wordsBy)
import Text.Parsec
import Control.Applicative ((<$>),(<*))

import TagSoupParsec
import Util (trim, replace, matchREs, postURIString, traceShowM)

import Movie (Info(..))
import SiteConfig(SiteConfig(..), SearchResult1(..), ret, normalizeGenres)

postURIString' uri xs =
  do res <- postURIString [] uri xs
     case res of
       Left _ -> return Nothing
       Right x -> return (Just x)

interFilmes =
  SiteConfig
  { siteName           = "InterFilmes"
  , cookies            = []
  , authenticate       = Nothing
  , searchFormUrl      = baseURL ++ "busca.html"
  , searchFormData     = \title -> [("search",title)]
  , searchFormMethod   = postURIString -- submitPOSTForm
  -- , webSearch       = \t -> submitPOSTForm (baseURL ++ "busca.html") [ "search=" ++ t ]
  , searchPreprocessor = id
  , searchNextPages    = const []
  , urlForMovie        = buildURL
  , parserForSearch    = search
  , parsersForInfo     = movieParsers
  }

baseURL = "http://www.interfilmes.com/"

buildURL url1 section = url1

search =
  many (try searchOne <|> parserZero)

head' xs = trace (show xs) (head xs)

searchOne =
  skipTo $ do lex $ tag "<tr>"
              lex $ tag "<td>"
              lex $ tag "<b>"
              link <- lex $ reTag "<a href=(filme_)(.+)-\\(.+\\)\\.html>" $ \ [a,b] -> a ++ "v1_" ++ b ++ ".html"
              lex $ tag "<font>"
              (title,original) <- fmap (splitTitles . innerText) $ manyTill anyTag (tag "</a>")
              img <- skipTo $ reTag "<img src=(FILMES/.+)>" head
              skipTo $ lex $ b $ tagText "Dados Técnicos"
              countries <- fmap (trim . replace " / " ", ") $ skipTo $ lex $ reTag "País de Origem: (.+)" head <* tag "<br>"
              genres <- lex $ reTag "Gênero: (.+)" head <* tag "<br>"
              len <- lex $ reTag "Tempo de Duração: (.+) minutos" head <* tag "<br>"
              year <- lex $ reTag "Ano de Lançamento: (.*)(\\s*)" head <* tag "<br>"
              return $ SR1 { sr1Title    = title
                           , sr1Original = original
                           , sr1Year     = year
                           , sr1Country  = countries
                           , sr1Genres   = replace " /" "," genres
                           , sr1Media    = ""
                           , sr1URL      = baseURL ++ link
                           , sr1Image    = Just (baseURL ++ img)
                           }
  where
    splitTitles = head . matchREs [ ("^(.+) \\((.+)\\)$", \ [_,a,b] -> (a,b))
                                  , ("^(.*)$", \ [_,a] -> (a,""))
                                  ]


movieParsers =
  [ ( "film"
    , [ cover
      , rating
      , skipTo $ itemprop "description" >>= ret Synopsis
      , field "Título no Brasil:" >>= ret Title
      , field "Título Original:" >>= ret OriginalTitle
      , field "País de Origem:" >>= ret Country . replace " / " ", "
      , skipTo $ itemprop "genre" >>= ret Genres . normalizeGenres
      , field "Classificação etária:" >>= ret Certification
      , skipTo $ itemprop "duration" >>= ret Length
      , field "Ano de Lançamento:" >>= ret Year
      , field "Estréia no Brasil:" >>= ret Release . ("Brasil: "++) . intercalate "-" . reverse . wordsBy (=='/')
      , field "Site Oficial:" >>= ret Site
      , field "Estúdio/Distrib.:" >>= ret Studio
      -- , do field "Direção:"
      --      fmap innerText (manyTill anyTag (tag "<br>")) >>= ret Director . replace " / " ", "
      , direction >>= ret Director
      , cast >>= ret Cast
      ]
    )
  ]

itemprop prop =
  do tag ("<span itemprop=" ++ prop ++ ">")
     text <- tagText ""
     tag "</span>"
     return text

rating =
  do a <- skipTo $ itemprop "ratingValue"
     b <- skipTo $ itemprop "ratingCount"
     traceShowM a
     return [ (Rating, intercalate ":" ["Interfilmes",show (2 * read a :: Float),b]) ]

cover =
  skipTo $ reTag "<img itemprop=image src=(.+)>" head >>= ret Cover . (baseURL++)

-- synopsis =
--   do skipTo $ do tag "<font>"
--                  u $ b $ tagText "Sinopse"
--                  lex $ tag "<br>"
--      fmap trim $ div $ tagText "" <* tag "<br>"

field label =
  skipTo $ do u $ reTag label id
              text@(_:_) <- trim <$> tagText ""
              return $ case text of
                         ('\160':'\n':xs) -> xs
                         ('\160':xs) -> xs
                         xs -> xs

-- cast =
--   do skipTo $ do tag "<font>"
--                  u $ b $ tagText "Elenco"
--                  tag "<br>"
--      fmap (concat . intersperse ", ") $ manyTill p (tag "</font>")
--   where
--     p = do x <- a $ tagText ""
--            y <- reTag "^ \\.\\.\\. (.+)$" head
--            tag "<br>"
--            return $ printf "%s (%s)" x y

cast =
  do skipTo $ tag "<span itemprop=actor>"
     cast <-
       many1 $
         do actor <- a $ span $ tagText ""
            role <- a $ tagText ""
            optional br
            return $ printf "%s (%s)" actor (dropWhile (=='.') role)
     return (intercalate "\n" cast)

direction =
  do skipTo $ lex $ tag "<span itemprop=director>"
     intercalate ", " <$> sepBy1 (a $ span $ tagText "") (tag " / ")
