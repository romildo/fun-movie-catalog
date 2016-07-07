{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}


module AdoroCinema where

import Prelude hiding (div,lex,span)
import Control.Applicative ((<$>), (<*))
import Control.Monad (guard)
import Text.HTML.TagSoup (Tag(TagText,TagOpen,TagClose), innerText)
import Text.Printf (printf)
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.List.Split (wordsBy)
import Data.Tuple.Utils (snd3)
import Text.Parsec
import Text.StringLike (strConcat)

import TagSoupParsec
import Util (trim, replace, submitGETForm, matchRE, traceShowM)

import Movie (Info(..))
import SiteConfig(SiteConfig(..), SearchResult1(..), ret, normalizeGenres)



adoroCinema =
  SiteConfig
  { siteName           = "AdoroCinema"
  , cookies            = []
  , authenticate       = Nothing
  , searchFormUrl      = baseURL ++ "/busca/1/"
  , searchFormData     = \title -> [("q",title)]
  , searchFormMethod   = submitGETForm
  -- , webSearch       = \t -> submitGETForm (baseURL ++ "/common/search/search_by_film/") ["criteria=" ++ t]
  , searchPreprocessor = \text -> replace "//<![CDATA[" "" text
  , searchNextPages    = \text -> case matchRE "<p class=\"extrazone\">[0-9]+ - ([0-9]+) de [0-9]+ resultados</p>" text of
                                    Just [x] -> [("p", show i) | i <- [2 .. read x]]
                                    _        -> []
  , urlForMovie        = buildURL
  , parserForSearch    = search
  , parsersForInfo     = movieParsers
  }

baseURL = "http://www.adorocinema.com"

buildURL url1 section =
  url1 ++ section ++ "/"

search =
  many (try searchOne <|> parserZero)

searchOne =
  skipTo $ do img <- td $ trimp $ a (reTag "<img src=(.+)>" head <* tag "</img>")
              tag "<td>"
              tag "<div>"
              lex $ tag "<div>"
              url <- lex $ reTag "<a href=(/filmes/filme-[0-9]+/)>" head
              title <- trim . innerText <$> manyTill anyTag (tag "</a>")
              originalTitle <- reTagText "[ \n]*\\((.*)\\)[ \n]*" head
              tag "<br>"
              lex $ tag "</br>"
              tag "<span>"
              -- year <- reTagText "\n(.*)" head
              year <- trim . innerText <$> manyTill anyTag (tag "</br>")
              -- tag "<br>"
              return $ SR1 { sr1Title    = title
                           , sr1Original = originalTitle
                           , sr1Year     = year
                           , sr1Country  = ""
                           , sr1Genres   = ""
                           , sr1Media    = ""
                           , sr1URL      = buildURL baseURL url
                           , sr1Image    = Just img
                           }

movieParsers =
  [ ( ""
    , [ skipTo $ h1 (tagText "") >>= ret Title

      , skipTo $ reTag "<img itemprop=image src=(.*)>" head >>= ret Cover

      , do skipTo $ innerTag "th" [] $ innerTag "div" [] $ tagText "Título original"
           innerTag "td" [] $ tagText "" >>= ret OriginalTitle

      , do skipTo $ tag "<span itemprop=genre>"
           tagText "" >>= ret Genres . normalizeGenres

      , do skipTo $ tag "<span itemprop=director>"
           innerText <$> many1 (notCloseTag "div") >>= (ret Director . trim)

      , do skipTo $ innerTag "span" [] $ tagText "Com"
           innerText <$> many1 (notCloseTag "div") >>= ret Cast

      , do skipTo $ lex $ innerTag "span" [] $ reTag "[ \n]*Nacionalidade[ \n]*" id
           many1 (notCloseTag "div") >>= (ret Country . intercalate ", ". map trim . wordsBy (==',') . innerText)

      , do let rating rater key =
                 let p = reTag ("[ \n]*" ++ key ++ "[ \n]*") id
                 in try (skipTo $ do lex $ innerTag "span" [] $ trimp $ choice [innerTag "a" [] p, p]
                                     x <- (takeWhile (not . isSpace) . trim . innerText . snd3) <$> wholeTag "div" []
                                     let r = show (2 * read (replace "," "." x) :: Float)
                                     guard (not (null x))
                                     return $ rater ++ (':' : r)
                        )
                    <|>
                    return ""
           rs <- sequence [ rating "AdoroCinema" "AdoroCinema"
                          , rating "AdoroCinema Imprensa" "Imprensa"
                          , rating "AdoroCinema Leitores" "Leitores"
                          ]
           case filter (not . null) rs of
             [] -> parserZero
             xs -> ret Rating (intercalate "\n" xs)

      , do skipTo $ innerTag "th" [] $ innerTag "div" [] $ tagText "Ano de produção"
           innerTag "td" [] $ innerTag "span" [] $ tagText "" >>= ret Year

      , do skipTo $ lex $ tag "<p itemprop=description>"
           many1 (notCloseTag "div") >>= (ret Synopsis . trim . innerText)

      ]
    )
  , ( "ficha-tecnica-e-premios"
    , [ do skipTo $ strong (tagText "título original:")
           reTagText " \\((.*)\\)" head >>= ret OriginalTitle

      , do skipTo $ strong (tagText "gênero:")
           reTagText " *(.*)" head >>= ret Genres

      , do skipTo $ strong (tagText "duração:")
           reTag "([0-9]+) min" head >>= ret Length

      , do skipTo $ strong (tagText "lançamento:")
           [a,b] <- reTagText " ([0-9]+) \\((.*)\\)" tail
           return [(Year,a), (Country,b)]

      , do skipTo $ strong (tagText "direção:")
           a (tagText "") >>= ret Director

      , do skipTo $ lex $ strong (tagText "site oficial:")
           a $ tagText "" >>= ret Site

      , do skipTo $ strong (tagText "estúdio:")
           x <- fmap (replace " / " ", ") $ reTagText " (.*)" head
           ret Studio x

      , do skipTo $ strong (tagText "distribuidora:")
           x <- fmap (replace " / " ", ") $ reTagText " (.*)" head
           ret Distributor x

      , do skipTo $ strong (tagText "roteiro:")
           reTagText " (.+)" head >>= ret Screenwriter

      , do skipTo $ strong (tagText "produção:")
           reTagText " (.*)" head >>= ret Producer

      , do skipTo $ strong (tagText "música:")
           reTagText " (.*)" head >>= ret Music

      , do skipTo $ strong (tagText "fotografia:")
           reTagText " (.*)" head >>= ret Photography

      , do skipTo $ strong (tagText "direção de arte:")
           reTagText " (.*)" head >>= ret ArtDirection

      , do skipTo $ strong (tagText "figurino:")
           reTagText " (.*)" head >>= ret Figurino

      , do skipTo $ strong (tagText "edição:")
           reTagText " (.*)" head >>= ret Editor

      , do skipTo $ strong (tagText "efeitos especiais:")
           x <- fmap (replace " / " ", ") $ tagText ""
           ret SpecialEffects x

      , do skipTo $ h2 (tagText "Sinopse")
           x <- try (tagText "" >> par (tagText "")) <|> fmap trim (tagText "")
           ret Synopsis x

      , let actor1 = do lex $ tag "<div>"
                        lex $ tag "<img>"
                        x <- lex $ h3 $ lex $ a $ tagText ""
                        y <- lex $ par $ reTag "\\((.*)\\)" head
                        lex $ tag "</div>"
                        return (x,y)
            actor2 = lex $ li $ do x <- fmap trim (tagText "")
                                   y <- lex $ span $ reTag "\\((.*)\\)" head
                                   return (x,y)
        in do skipTo $ lex $ h2 $ tagText "Elenco"
              --
              lex $ tag "<div class=atores-destaque>"
              as <- many actor1
              lex $ tag "</div>"
              --
              lex $ tag "<div class=atores-lista>"
              lex $ tag "<ul>"
              bs <- many actor2
              lex $ tag "</ul>"
              lex $ tag "</div>"
              --
              let res = unlines $ map (\(actor,role) -> printf "%s(%s)" actor role) (as ++ bs)
              ret Cast res

      , do skipTo $ lex $ h2 $ tag "Premiações"
           (_,ts,_) <- wholeTag "p" []
           ret Awards $ myInnerText ts
      ]
    )
  , ( "noticias-curiosidades"
    , [ let startItem (TagText ('-':' ':_)) = True
            startItem  _ = False
        in do skipTo $ lex $ h2 $ tagText "Curiosidades"
              (_,ts,_) <- wholeTag "p" []
              ret Curiosities $ unlines $ map innerText (spanGroups startItem ts)
      ]
    )
  ]



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

