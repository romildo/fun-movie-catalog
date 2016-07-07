module DVDWorld where

import Prelude hiding (lex,span)
import Data.Char (toLower)
import Data.List (intercalate)
import Data.List.Split (wordsBy)
import Data.List.Utils (split, join, replace)
import Control.Applicative ((<*))
import Text.Parsec hiding (label)
import Network.URI (isAllowedInURI, escapeURIString)
import Debug.Trace (trace)

import Util (trim, trimLeft, submitGETForm, postURIString, capitalise)

import TagSoupParsec

import Movie (Info(..))
import SiteConfig


dvdWorld =
  SiteConfig
  { siteName           = "DVDWorld"
  , cookies            = []
  , authenticate       = Nothing
  , searchFormUrl      = baseURL ++ "dvdworld.hts"
  , searchFormData     = \title -> [ ("opcao","titulo"), ("searchval",title), ("start","0"), ("tutu","search") ]
  , searchFormMethod   = submitGETForm
  -- , webSearch       = \t -> submitPOSTForm (baseURL ++ "dvdworld.hts?+search")
  --                                    [ "arg3=" ++ escapeURIString isAllowedInURI t, "arg4=titulo" ]
  , searchPreprocessor = id
  , searchNextPages    = const []
  , urlForMovie        = \url1 section -> url1
  , parserForSearch    = search
  , parsersForInfo     = movieParsers
  }

baseURL = "http://dvdworld.com.br/"


search =
  many (try searchOne <|> parserZero)

searchOne =
  skipTo $ do tag "<td>"
              tag "<font>"
              url <- reTag "<a href=(.+)>" head
              [title,media1,media2] <- reTag "(?i)^(.+?)(?:\\s*-\\s*\\((DVD(?: DUPLO)?|BLU-RAY(?: DUPLO)?|BOX.*)\\)(.*))?\\s*$" id
              lex $ tag "</a>"
              lex $ tag "</font>"
              tag "<br>"
              tag "<font>"
              [original,country,year] <- reTag "^(.*)\\s*- (.*) (\\d+)\\s*..$" id <|> return ["","",""]
              skipTo $ tag "<td>"
              skipTo $ tag "<td>"
              genres <- font $ tagText ""
              return $ SR1 { sr1Title    = capitalise title
                           , sr1Original = original
                           , sr1Year     = year
                           , sr1Country  = replace " / " ", " country
                           , sr1Genres   = replace " / " ", " genres
                           , sr1Media    = capitalise (media1++media2)
                           , sr1URL      = url
                           , sr1Image    = Nothing
                           }

movieParsers =
  [ ( "main"
    , [ do skipTo $ tag "<h1>"
           title <- reTag "(?i)^(.+?)(?:\\s*-\\s*\\((?:DVD(?: DUPLO)?|BLU-RAY(?: DUPLO)?|BOX.*)\\).*)?$" head
           cover <- skipTo $ reTag "<img src=(.+)>" head
           return [(Title,capitalise title),(Cover,cover)]

      , do skipTo $ b $ tagText "Título Original: "
           [original,country,year] <- reTag "^(.+?)\\s*-\\s+(.+?)\\s+(\\d+)$" id
           return [(OriginalTitle, original),(Country,country),(Year,year)]

      , do skipTo $ b $ tagText "Atores: "
           tagText "" >>= ret Cast . intercalate "\n" . map trim . wordsBy (==',')

      , do skipTo $ b $ tagText "Diretor:"
           tagText "" >>= ret Director . trimLeft

      , do skipTo $ b $ tagText "Sinopse:"
           sepBy (tagText "") (tag "<br>") >>= ret Synopsis . unlines . map trim

      , do skipTo $ b $ tagText "GÊNERO"
           skipTo $ tag "<td>"
           skipTo $ tag "<td>"
           font $ tagText "" >>= ret Genres . normalizeGenres

      , do skipTo $ b $ tagText "ESTÚDIO"
           skipTo $ tag "<td>"
           skipTo $ tag "<td>"
           font $ tagText "" >>= ret Studio

      , do skipTo $ b $ tagText "DURAÇÃO"
           skipTo $ tag "<td>"
           skipTo $ tag "<td>"
           font $ reTag "(.+) minutos" head >>= ret Length
      ]
    )
  ]
