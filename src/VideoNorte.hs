module VideoNorte where

import Prelude hiding (lex,span)
import Data.Char (toLower)
import Data.List.Utils (split, join, replace)
import Control.Applicative ((<*))
import Text.Parsec hiding (label)
import Network.URI (isAllowedInURI, escapeURIString)
import Debug.Trace (trace)
import Text.Regex.PCRE ((=~))
import qualified Data.ByteString.Char8 as B

import Util (trim, trimLeft, submitGETForm, capitalise)
import TagSoupParsec

import Movie (Info(..))
import SiteConfig


videoNorte =
  SiteConfig
  { siteName           = "VideoNorte"
  , cookies            = []
  , authenticate       = Nothing
  , searchFormUrl      = baseURL ++ "pesquisalocresult.cfm"
  , searchFormData     = \title -> [("Crit",title)]
  , searchFormMethod   = submitGETForm
  , searchPreprocessor = id
  , searchNextPages    = \text -> let xs = B.pack text =~ B.pack "<a href=\"[^&]+&Start=([0-9]+)\" class=\"LinkNav\"><b>[0-9]+</b></a>" :: [[B.ByteString]]
                                  in [ ("Start", B.unpack x) | [_,x] <- xs ]
  , urlForMovie        = \url1 section -> url1
  , parserForSearch    = search
  , parsersForInfo     = movieParsers
  }

baseURL = "http://www.videonorte.com.br/"


search =
  many (try searchOne <|> parserZero)

searchOne =
  skipTo $ do lex $ tag "<table>"
              lex $ tag "<tr>"
              lex $ tag "<td>"
              lex $ tag "<a>"
              cover <- lex $ reTag "<img src=(.+)>" head
              lex $ tag "</a>"
              lex $ tag "</td>"
              lex $ tag "</tr>"
              lex $ tag "<tr>"
              lex $ tag "<td>"
              url <- lex $ reTag "<a href=(.+)>" head
              title <- tagText ""
              lex $ tag "</a>"
              lex $ tag "</td>"
              lex $ tag "</tr>"
              lex $ tag "</table>"
              return $ SR1 { sr1Title    = fixTitle title
                           , sr1Original = ""
                           , sr1Year     = ""
                           , sr1Country  = ""
                           , sr1Genres   = ""
                           , sr1Media    = ""
                           , sr1URL      = baseURL ++ url
                           , sr1Image    = Just (baseURL ++ cover)
                           }

fixTitle text =
  let (_:ts):_ = B.pack text =~ B.pack "(?i)^\\s*(.+?)(?:, (o|a|os|as|um|uma|uns|umas|no|na|nos|nas))?\\s*$" :: [[B.ByteString]]
      [t1,t2] = map B.unpack ts
  in if null t2 then t1 else t2 ++ " " ++ t1

movieParsers =
  [ ( "main"
    , [ do skipTo $ lex $ a $ tagText "Resultado da Pesquisa"
           tag "<img>"
           cover <- skipTo $ reTag "<img src=(.+)>" head
           title <- skipTo $ lex $ strong $ tagText ""
           tag "<br>"
           original <- lex $ reTag "^\\s*\\((.+?)\\s*\\)$" head
           tag "<br>"
           [year,genres,length] <- lex $ reTag "^\\s*(.+?)\\.\\s*(.+?)\\.\\s*(.+?) min\\.\\s*$" id
           return [ (Title,fixTitle title)
                  -- , (OriginalTitle,original)  -- TODO: find out if this site keeps the original title information
                  , (Cover,baseURL++cover)
                  , (Year,year)
                  , (Genres,genres)
                  , (Length,length)
                  ]

      , do skipTo $ strong $ tagText "Distribuidora:"
           tagText "" >>= ret Distributor . trimLeft

      , do skipTo $ strong $ tagText "Direção:"
           tagText "" >>= ret Director . trimLeft

      , do skipTo $ strong $ tagText "Com:"
           reTag "^\\s*(.+?)\\.?\\s*$" head >>= ret Cast

      , do skipTo $ lex $ td $ tag "<img>" >> tagText "Sinopse"
           skipTo $ tag "</table>"
           tagText "" >>= ret Synopsis . trim

      , do skipTo $ strong $ tagText "Classificação Etária:"
           reTag "<img src=imagens/ic_restricao_(.+?).gif>" head >>= ret Certification . (++ " anos")
      ]
    )
  ]
