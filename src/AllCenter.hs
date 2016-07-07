module AllCenter where

import Prelude hiding (lex,span)
import Data.Char (toLower)
import Data.List.Utils (join, replace)
import Control.Applicative ((<*))
import Text.Parsec hiding (label)
import Network.URI (isAllowedInURI, escapeURIString)
import Debug.Trace (trace)

import Util (trim, submitGETForm)
import TagSoupParsec

import Movie (Info(..))
import SiteConfig


allCenter =
  SiteConfig
  { siteName           = "AllCenter"
  , cookies            = []
  , authenticate       = Nothing
  , searchFormUrl      = baseURL ++ "busca.asp"
  , searchFormData     = \title -> [ ("depId","3"), ("chave",title) ]
  , searchFormMethod   = submitGETForm
  -- , webSearch       = \t -> submitPOSTForm (baseURL ++ "vitrine.asp") [ "chave=" ++ escapeURIString isAllowedInURI t ]
  , searchPreprocessor = id
  , searchNextPages    = const []
  , urlForMovie        = \url1 section -> url1
  , parserForSearch    = search
  , parsersForInfo     = movieParsers
  }

baseURL = "http://www.allcenter.com.br/"


search =
  many (try searchOne <|> parserZero)

searchOne =
  skipTo $ do img <- lex $ td $ trimp $ a $ trimp $ reTag "<img src=(.+)>" head
              lex $ tag "<td>"
              lex $ tag "<div>"
              lex $ tag "<table>"
              lex $ tag "<tr>"
              lex $ tag "<td>"
              [url] <- lex $ reTag "<a href=(.*) title=.*>" id
              lex $ tag "<font>"
              lex $ tag "<b>"
              [_,title,_] <- lex $ reTagText "(DVD|Blu-ray)? *(.*)(\\(.*\\))?" id
              lex $ tag "</b>"
              lex $ tag "</font>"
              lex $ tag "</a>"
              lex $ tag "</td>"
              lex $ tag "</tr>"
              lex $ wholeTag "tr" []
              lex $ wholeTag "tr" []
              lex $ tag "</table>"
              return $ SR1 { sr1Title    = title
                           , sr1Original = ""
                           , sr1Year     = ""
                           , sr1Country  = ""
                           , sr1Genres   = ""
                           , sr1Media    = ""
                           , sr1URL      = baseURL ++ url
                           , sr1Image    = Just img
                           }

movieParsers =
  [ ( "main"
    , [ do skipTo $ tag "<font size=\"4\">"
           title <- reTag "(?i)^(?:DVD |Blu-ray )?(.+?)(?: \\(.*Duplo\\)|- Rental \\+ Poster)?$" head
           return [(Title,title)]

      , skipTo $ font $ reTag "(?i)Título Original:(.+?)(?: \\((?:Blu-Ray|DVD)\\))?$" head >>= ret OriginalTitle

      , skipTo $ do a $ label "(?:Filmes|Rental)"
                    tagText " - "
                    a $ font $ tagText "" >>= ret Genres . replace "/" ", "

      , skipTo $ tag "<td>" >> (reTag "<img src=(.+/foto/.+)>" head >>= ret Cover)

      , do skipTo $ tag "<div id=\"tcontent1\">"
           skipTo $ tag "<font>"
           optional $ tag "<p>"
           optional $ tag "<span>"
           optional $ tag "<span>"
           optional $ tag "<em>"
           optional $ lex $ wholeTag "strong" []
           optional $ lex $ tag "</em>"
           optional $ lex $ tag "</span>"
           optional $ lex $ tag "</span>"
           optional $ lex $ tag "</p>"
           optional $ tag "<p>"
           many (lex br)
           manyTill (fmap trim (tagText "") <* many (lex br))
                    (tag "</span>" <|> tag "</p>" <|> tag "</font>" <|> tag "<strong>")
             >>= ret Synopsis . join "\n"

      , skipTo $ do label "Ano de Lançamento:"
                    optional $ tag "<font>"
                    tagText "" >>= ret Release . trim

      , skipTo $ do label "Direção:"
                    let p1 = fmap (join ", ") $ many1 (lex $ a $ u $ tagText "")
                        p2 = fmap trim $ tagText ""
                    (try p1 <|> p2) >>= ret Director

      , skipTo $ do label "Atores/Artistas:"
                    cast <- many1 ((lex $ a $ u $ tagText "") <|> tagText "")
                    case join "\n" $ map trim cast of
                      "Animação" -> return []
                      cast' -> ret Cast cast'

      , skipTo $ do label "País\\s*/\\s*Ano de Produção:"
                    let p1 = do country <- a $ font $ tagText ""
                                year <- font $ reTag "^ - ([0-9]+)$" head
                                return [(Country,country),(Year,year)]
                        p2 = do [country,year] <- reTag "^\\s*(.+) - ([0-9]+)$" id
                                return [(Country,country),(Year,year)]
                    try p1 <|> p2

      , skipTo $ do label "Duração:"
                    optional $ tag "<font>"
                    reTag "(?i)^\\s*([0-9]+) minutos\\.?$" head >>= ret Length

      , skipTo $ do label "Faixa Etária:"
                    optional $ tag "<font>"
                    reTag "(?i)^\\s*(.+?)\\.?$" head >>= ret Certification . map toLower

      , skipTo $ do label "Formato d. Tela:"
                    optional (tag "<a>" >> tag "<font>")
                    tagText "" >>= ret Format . trim

      , skipTo $ do label "Distribuidora:"
                    a (font (tagText "")) >>= ret Distributor
      ]
    )
  ]

label text =
  try label1 <|> label2
  where
    re = "\\s*" ++ text ++ "\\s*"
    label1 = lex $ font $ reTag re id
    label2 = lex $ strong $ reTag re id

