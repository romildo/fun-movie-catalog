{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}

module EPipoca (ePipoca) where

import Prelude hiding (lex)
import Debug.Trace (trace)
import Text.HTML.TagSoup (innerText)
-- import Text.Printf (printf)
import Data.Char (isDigit)
import Data.List (intersperse, break, intercalate)
import Data.List.Split (wordsBy)
import Text.Parsec
import Control.Applicative ((<*))

-- import Graphics.UI.Gtk hiding (Release)

import TagSoupParsec
import Util (trimLeft, trim, replace, matchRE, snd3, submitGETForm)

import Movie (Info(..), normalCountry)
import SiteConfig(SiteConfig(..), SearchResult1(..), ret, normalizeGenres)



ePipoca =
  SiteConfig
  { siteName           = "e-Pipoca"
  , cookies            = []
  , authenticate       = Nothing
  , searchFormUrl      = baseURL ++ "/busca_mais.php"
  , searchFormData     = \title -> [("opc","filmes"), ("ordena","4"), ("busca",title)]
  , searchFormMethod   = submitGETForm
  -- , webSearch       = \t -> submitGETForm (baseURL ++ "/busca.php") ["opc=todos", "busca=" ++ t]
  , searchPreprocessor = id
  , searchNextPages    = \text -> case matchRE "Foram encontrados .b.([0-9]+) filmes./b.." text of
                                    Just [x] -> [("thisOffset", show i) | i <- [20, 40 .. read x]]
                                    _        -> []
  , urlForMovie        = buildURL
  , parserForSearch    = search
  , parsersForInfo     = movieParsers
  }

baseURL = "http://www.epipoca.com.br"

buildURL url1 section = replace "detalhes" section url1
  -- let (prefix,_,sufix) = searchSubstring url1 "detalhes"
  -- in prefix ++ section ++ suffix

search =
  many (try searchOne <|> parserZero)

searchOne =
  skipTo $ do image <- lex $ td $ a $ reTag "<img src=(.+)>" head
              lex $ tag "<td>"
              tag "<font class=titulo>"
              url <- reTag "<a href=(.+)>" head
              title <- tagText ""
              tag "</a>"
              tag "</font>"
              optional $ tagText ""
              tag "<br>"
              tag "("
              originalTitle <- i $ tagText ""
              xs <- manyTill (tag ", " >> a (tagText "")) (tag ")")
              let (xs1,xs2) = break (all isDigit) xs
              let countries = intercalate ", " xs1
              let year = if null xs2 then "" else head xs2
              let genres = if null xs2 then "" else concat (intersperse ", " (tail xs2))
              return $ SR1 { sr1Title    = title
                           , sr1Original = originalTitle
                           , sr1Year     = year
                           , sr1Country  = countries
                           , sr1Genres   = genres
                           , sr1Media    = ""
                           , sr1URL      = baseURL ++ "/" ++ url
                           , sr1Image    = Just (baseURL ++ "/" ++ image)
                           }


movieParsers =
  [ ( "detalhes"
    , [ skipTo $
        do tag "<font class=titulo>"
           t <- tagText ""
           tag "</font>"
           tag "<br>"
           ot <- reTag "\\((.+), " head
           xs <- many $ try (a (tagText "") <* tag ", ") <|> parserZero
           y <- a (tagText "")
           tag ")"
           tag "<br>"
           return [ (Title,t)
                  , (OriginalTitle,ot)
                  , (Country,concat (intersperse ", " (map normalCountry xs)))
                  , (Year,y)
                  ]

      , do skipTo $ b $ tag "Tipo: "
           tagText "" >>= ret Type . replace " / " ", "

      , do skipTo $ td $ b $ tag "Prêmios: "
           tagText "" >>= ret Awards

      , do skipTo $ b $ tag "Cotação: "
           cotacao <- reTag "([0-9,]+) \\(([0-9]+) votos\\)" id
           return [ (Rating, intercalate ":" ("e-Pipoca" : cotacao)) ]

      , skipTo $ do suffix <- b $ choice [ tag "Estréia: " >> return ""
                                         , tag "Lançamento do DVD: " >> return " (DVD)"
                                         ]
                    tagText "" >>= ret Release . ("Brasil:" ++) . (++ suffix) . intercalate "-" . reverse . wordsBy (=='.')

      , skipTo $ do url <- reTag "<img class=image src=(.+)>" head
                    font $ b $ tag "SINOPSE"
                    ts <- manyTill anyTag (tag "</td>")
                    return [ (Cover, baseURL ++ "/" ++ url)
                           , (Synopsis, innerText ts)
                           ]
      ]
    )
  , ( "ficha"
    , [ skipTo $
        do tag "<br>"
           b $ tag "Gênero: "
           ts <- manyTill anyTag (tag "<br>")
           return [(Genres, normalizeGenres (innerText ts))]

      , skipTo $
        do tag "<br>"
           b $ tag "Duração: "
           reTag "(.+) min\\." head >>= ret Length

      , skipTo $ do b $ tag "Produtora(s): "
                    ts <- manyTill anyTag (tag "<br>" <|> tag "<script>")
                    return [(Studio, innerText ts)]

      , skipTo $ do b $ tag "Distribuidora(s): "
                    ts <- manyTill anyTag (tag "<br>")
                    return [(Distributor, innerText ts)]

      , table12 "Diretor(es) / Directors" Director ", "
      , table12 "Roteirista(s) / Writers" Screenwriter ", "
      , table12 "Elenco / Cast" Cast "\n"
      , table12 "Fotógrafo(s) / Cinematographers" Photography ", "
      , table12 "Produtor(es) / Producers" Producer ", "
      , table12 "Compositor(es) / Original Music" Music ", "
      , table12 "Montagem / Editors" Editor ", "
      , table12 "Diretor(es) de Elenco / Casting Directors" CastingDirectors ", "
      , table12 "Desenhista(s) de Produção / Production Designers" ProductionDesign ", "
      , table12 "Direção de Arte / Art Directors" ArtDirection ", "
      , table12 "Decorador(es) / Set Decorators" SetDecorators ", "
      , table12 "Figurinista(s) / Costume Designers" Figurino ", "
      , table12 "Maquiagem / Make-up" MakeUp ", "
      , table12 "Supervisor(es) de Produção / Production Managers" ProductionManagers ", "
      ]
    )
  -- , ( "produtora"
  --   , [ list "Distribuidora(s)" Distributor
  --     ]
  --   )
  , ( "chave"
    , [ do skipTo (lex (tag "<br>" >> tag "<br>"))
           xs <- manyTill (a (tagText "") <* tagText "") (tag "</td>")
           return [(Keywords, concat (intersperse ", " xs))]
      ]
    )
  ]



tableN msg info sep p =
  do skipTo $ b $ tag msg
     skipTo $ tag "<table>"
     xs <- manyTill p (tag "</table>")
     return [(info, intercalate sep xs)]

-- table1 msg info sep =
--   tableN msg info sep $ tr $ td $ a $ reTag "([^\\(]+)( \\(.+\\))?" head

table2 msg info sep =
  tableN msg info sep $ tr $ td $ do x <- a $ reTag "([^\\(]+)( \\(.+\\))?" head
                                     y <- option "" $ reTag " \\.\\.\\. (.+)" head
                                     return $ if null y
                                              then x
                                              else x ++ " (" ++ y ++ ")"

table12 msg info sep =
  -- try (table2 msg info sep) <|> table1 msg info sep
  table2 msg info sep


list msg info =
  do skipTo $ do b $ tagText msg
                 tag "<ul>"
     xs <- manyTill (fmap (innerText . snd3) (wholeTag "li" [])) (tag "</ul>")
     return [(info, concat (intersperse "; " xs))]


