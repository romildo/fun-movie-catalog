{- Time-stamp: "2015-07-23 20:15:16 romildo" -}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Movie (
  Info(..), DiscInfo(..), CommentInfo(..)
  , Movie(..)
  , Col(..)
  , infoBounds, discInfoBounds, commentInfoBounds
  , infoMap
  , emptyInfo, emptyDiscInfo, emptyCommentInfo, emptyMovie
  , emptyCol
  , mkMovie
  , translateInfo, translateCountry
  , defaultGenres
  , defaultLanguages
  , defaultAudioCompressions
  , defaultScreens
  , defaultFormats
  , defaultCertifications
  , normalGenre, normalCountry
  ) where

import           Prelude hiding (lookup)

import           Data.Char (toLower, isDigit)
import           Data.List (partition,sort)
import qualified Data.List as List (lookup)
import qualified Data.Maybe as Maybe (fromMaybe)
import           Data.Ix (Ix(range))
import           Data.Array.IArray (Array, listArray, (//), assocs)
import           Data.Map (fromList, lookup)
import           Graphics.UI.Gtk (Pixbuf)
import           Debug.Trace (trace)


data Info
  = Title
  | OriginalTitle
  | AlternateTitle
  | Country
  | Year
  | Release
  | Genres
  | Director
  | Stars
  | Length
  | Type
  | Rating        -- IMDB rating
  | Score         -- other sites rating
  | Subtitles
  | Audios
  | Screen
  | Format
  | Certification
  | ParentalGuide
  | Theme
  | Contains
  | Keywords
  | Site
  | IMDB
  | InclusionDate
  | Studio
  | Distributor
  | Screenwriter
  | Producer
  | Music
  | Photography
  | ProductionDesign
  | ArtDirection
  | SetDecorators
  | Figurino
  | MakeUp
  | Editor
  | CastingDirectors
  | SpecialEffects
  | ProductionManagers
  | Ripper
  | Uploader
  | Cast
  | Synopsis
  | Awards
  | Curiosities
  | Notes
  | Cover
  | File
  | Location
  deriving (Eq,Bounded,Ord,Ix,Enum,Read,Show)

data DiscInfo
  = Source
  | Conversion
  | Bitrate
  | Reduction
  | ImageFormat
  | Size
  | URL
  | Links
  | Episodes
  | Extras
  deriving (Eq,Bounded,Ord,Ix,Show)

data CommentInfo
  = CommentTitle
  | CommentDescription
  | CommentAuthor
  | CommentContact
  | CommentDate
  | CommentScore
  | CommentBody
  deriving (Eq,Bounded,Ord,Ix,Show)
           
data Movie =
  M { cover    :: Maybe Pixbuf
    , ident    :: Int
    , info     :: Array Info String
    , images   :: [(String, String)]
    , discs    :: [Array DiscInfo String]
    , comments :: [Array CommentInfo String]
    , unknown  :: [(String, String)]
    }

instance Show Movie where
  showsPrec _ (M cover ident info images discs comments unknown) =
    showString "[title " . shows ident . showChar ']' . shownl .
    showl (showField showFieldNameIx) (assocs info) .
    showl (showField (showImgField "")) (zip [1..] (map fst images)) .
    showl (showField (showImgField "remote")) (zip [1..] (map snd images)) .
    showl showFieldArray (zip [1..] discs) .
    showl showFieldArray (zip [1..] comments) .
    showl (showField showString) unknown . shownl
    where
      showFieldNameIx i = showString (map toLower (show i))

      showFieldNameIxN n i = showFieldNameIx i . showString n

      showImgField label i = showString label . showString "img" . shows i

      showField _ (i,"") = id
      showField f (i,x) =
        f i .
        showString " = " .
        showMultiLines (lines x) .
        shownl
        where
          showMultiLines [] = id
          showMultiLines (y:ys) =
            showString y .
            showl (\z -> shownl . showChar ' ' . showString z) ys

      showFieldArray (n,array) =
          showl (showField (showFieldNameIxN (show n))) (assocs array)

infoBounds = (minBound::Info,maxBound::Info)

discInfoBounds = (minBound::DiscInfo,maxBound::DiscInfo)

commentInfoBounds = (minBound::CommentInfo,maxBound::CommentInfo)


infoMap = fromList (map (\x -> (map toLower (show x),x))
                        (range infoBounds))

discInfoMap = fromList (map (\x -> (map toLower (show x),x))
                            (range discInfoBounds))

commentInfoMap = fromList (map (\x -> (map toLower (show x),x))
                               (range commentInfoBounds))

emptyInfo = listArray infoBounds (repeat "")
emptyDiscInfo = listArray discInfoBounds (repeat "")
emptyCommentInfo = listArray commentInfoBounds (repeat "")
emptyMovie ident = M { cover = Nothing
                     , ident = ident
                     , info = emptyInfo
                     , images = []
                     , discs = []
                     , comments = []
                     , unknown = []
                     }


mkMovie ident fields = sepFields fields [] [] [] []
  where
    sepFields [] info discs comments unknown =
      M { cover = Nothing
        , ident = ident
        , info = emptyInfo // info
        , images = []
        , discs = map (emptyDiscInfo //) discs
        , comments = map (emptyCommentInfo //) comments
        , unknown = unknown
        }
    sepFields ((key,val):fs) info discs comments unknown =
      case lookup key infoMap of
        Just i -> sepFields fs ((i,val):info) discs comments unknown
        Nothing ->
          let (k2',k1') = partition isDigit (reverse key)
              k1 = reverse k1'
              k2 = read (reverse k2')
          in case lookup k1 discInfoMap of
               Just i -> sepFields fs info (ins (i,val) k2 discs) comments unknown
               Nothing ->
                 case lookup k1 commentInfoMap of
                   Just i -> sepFields fs info discs (ins (i,val) k2 comments) unknown
                   Nothing -> sepFields fs info discs comments ((key,val):unknown)
    ins x 1 []       = [[x]]
    ins x 1 (ys:yss) = (x:ys):yss
    ins x i []       = [] : ins x (i - 1) []
    ins x i (ys:yss) = ys : ins x (i - 1) yss



data Col = Col
  { genres            :: [(String,String)]
  , languages         :: [(String,String)]
  , audiocompressions :: [String]
  , titles            :: [Movie]
  , greatestMovieId   :: Int
  , imageDir          :: Maybe FilePath
  , filePath          :: Maybe FilePath
  }

instance Show Col where
  showsPrec _ Col{genres,languages,audiocompressions,titles} =
    showString "[genres]\n" . showl showPair genres . shownl .
    showString "[languages]\n" . showl showPair languages .shownl .
    showString "[audiocompressions]\n" . showl showSingle audiocompressions . shownl .
    showl shows titles
    where
      showSingle a = showString a . shownl
      showPair (a,b) = showString a . showChar '/' . showString b . shownl

showl f xs = foldr (.) id (map f xs)

shownl = showChar '\n'

emptyCol =
  Col
  { genres            = []
  , languages         = []
  , audiocompressions = []
  , titles            = []
  , greatestMovieId   = 0
  , imageDir          = Nothing
  , filePath          = Nothing
  }


translateInfo Title              = "Título"
translateInfo OriginalTitle      = "Título original"
translateInfo AlternateTitle     = "Título Alternativo"
translateInfo Country            = "País"
translateInfo Year               = "Ano"
translateInfo Release            = "Lançamento"
translateInfo Genres             = "Gêneros"
translateInfo Director           = "Diretores"
translateInfo Stars              = "Estrelando"
translateInfo Length             = "Duração"
translateInfo Type               = "Tipo"
translateInfo Rating             = "Avaliação IMDB"
translateInfo Score              = "Avaliação"
translateInfo Subtitles          = "Legendas"
translateInfo Audios             = "Áudios"
translateInfo Screen             = "Tela"
translateInfo Format             = "Formato de tela"
translateInfo Certification      = "Classificação"
translateInfo Theme              = "Tema"
translateInfo Contains           = "Contem"
translateInfo Keywords           = "Palavras-chave"
translateInfo Site               = "Site"
translateInfo IMDB               = "IMDB"
translateInfo InclusionDate      = "Data inclusão"
translateInfo Studio             = "Studio"
translateInfo Distributor        = "Distribuidores"
translateInfo Screenwriter       = "Roteiristas"
translateInfo Producer           = "Produtores"
translateInfo Music              = "Música"
translateInfo Photography        = "Fotografia"
translateInfo ProductionDesign   = "Desenhistas de produção"
translateInfo ArtDirection       = "Directores de arte"
translateInfo SetDecorators      = "SetDecorators"
translateInfo Figurino           = "Figurino"
translateInfo MakeUp             = "Maquiagem"
translateInfo Editor             = "Editores"
translateInfo CastingDirectors   = "Directores de elenco"
translateInfo SpecialEffects     = "Efeitos especiais"
translateInfo ProductionManagers = "Supervisores de produçãos"
translateInfo Ripper             = "Ripper"
translateInfo Uploader           = "Uploader"
translateInfo Cast               = "Elenco"
translateInfo Synopsis           = "Sinopse"
translateInfo Awards             = "Premiação"
translateInfo Curiosities        = "Curiosidades"
translateInfo Notes              = "Notas"
translateInfo Cover              = "Capa"
translateInfo File               = "Arquivo"
translateInfo Location           = "Localização"

translateCountry country = Maybe.fromMaybe country (List.lookup country countries)

countries =
  [
    ("Argentina",       "Argentina")
  , ("Australia",       "Austrália")
  , ("Austria",         "Áustria")
  , ("Belgium",         "Bélgica")
  , ("Bolivia",         "Bolívia")
  , ("Brazil",          "Brasil")
  , ("Bulgaria",        "Bulgária")
  , ("Canada",          "Canadá")
  , ("Chile",           "Chile")
  , ("China",           "China")
  , ("Colombia",        "Colômbia")
  , ("Croatia",         "Croácia")
  , ("Czech Republic",  "República Checa")
  , ("Denmark",         "Dinamarca")
  , ("Ecuador",         "Equador")
  , ("Egypt",           "Egito")
  , ("Estonia",         "Estônia")
  , ("Finland",         "Finlândia")
  , ("France",          "França")
  , ("Germany",         "Alemanha")
  , ("Greece",          "Grécia")
  , ("Hong Kong",       "Hong Kong")
  , ("Hungary",         "Hungria")
  , ("Iceland",         "Islândia")
  , ("India",           "Índia")
  , ("Indonesia",       "Indonésia")
  , ("Ireland",         "Irlanda")
  , ("Israel",          "Israel")
  , ("Italy",           "Itália")
  , ("Jamaica",         "Jamaica")
  , ("Japan",           "Japão")
  , ("Jordan",          "Jordânia")
  , ("Kazakhstan",      "Cazaquistão")
  , ("Kenya",           "Quênia")
  , ("Kuwait",          "Kuwait")
  , ("Latvia",          "Letônia")
  , ("Lebanon",         "Líbano")
  , ("Lithuania",       "Lituânia")
  , ("Malaysia",        "Malásia")
  , ("Mexico",          "México")
  , ("Netherlands",     "Holanda")
  , ("New Zealand",     "Nova Zelândia")
  , ("Norway",          "Noruega")
  , ("Oman",            "Omã")
  , ("Panama",          "Panamá")
  , ("Peru",            "Peru")
  , ("Philippines",     "Filipinas")
  , ("Poland",          "Polônia")
  , ("Portugal",        "Portugal")
  , ("Qatar",           "Catar")
  , ("Romania",         "Romênia")
  , ("Russia",          "Rússia")
  , ("Serbia",          "Sérvia")
  , ("Singapore",       "Singapura")
  , ("Slovakia",        "Eslováquia")
  , ("Slovenia",        "Slovênia")
  , ("South Africa",    "África do Sul")
  , ("South Korea",     "Coreia do Sul")
  , ("Spain",           "Espanha")
  , ("Sri Lanka",       "Sri Lanca")
  , ("Sweden",          "Suécia")
  , ("Switzerland",     "Suíça")
  , ("Taiwan",          "República da China")
  , ("Thailand",        "Tailândia")
  , ("Turkey",          "Turquia")
  , ("UK",              "Reino Unido")
  , ("USA",             "EUA")
  , ("Uruguay",         "Uruguai")
  , ("Venezuela",       "Venezuela")
  , ("Yugoslavia",      "Iugoslávia")
  ]

defaultGenres =
  sort
    [ ("Ação"              , "Action"    )
    , ("Animação"          , "Animation" )
    , ("Aventura"          , "Adventure" )
    , ("Biografia"         , "Biography" )
    , ("Comédia"           , "Comedy"    )
    , ("Drama"             , "Drama"     )
    , ("Esporte"           , "Sport"     )
    , ("Família"           , "Family"    )
    , ("Fantasia"          , "Fantasy"   )
    , ("Faroeste"          , "Western"   )
    , ("Ficção Científica" , "Sci-Fi"    )
    , ("Guerra"            , "War"       )
    , ("História"          , "History"   )
    , ("Mistério"          , "Mystery"   )
    , ("Musical"           , "Music"     )
    , ("Policial"          , "Crime"     )
    , ("Romance"           , "Romance"   )
    , ("Suspense"          , "Thriller"  )
    , ("Terror"            , "Horror"    )
    ]

defaultLanguages =
  sort
    [ ("Espanhol"  , "Spanish"              )
    , ("Inglês"    , "English"              )
    , ("Português" , "Brazilian Portuguese" )
    ]

defaultAudioCompressions =
  sort
    [ "DD 2.0"
    , "DD 5.1"
    , "Mono"
    ]

defaultScreens =
  sort
    [ "Fullscreen"
    , "Fullscreen (4:3) letterbox"
    , "Widescreen"
    , "Widescreen anamorphic (1.78:1)"
    , "Widescreen anamorphic (1.85:1)"
    , "Widescreen anamorphic (2.35:1)"
    , "Widescreen anamorphic (2.40:1)"
    ]

defaultFormats =
  sort
    [ "NTSC"
    , "PAL"
    ]

defaultCertifications =
  sort
    [ "10 anos"
    , "12 anos"
    , "14 anos"
    , "16 anos"
    , "Free"
    , "Parental"
    ]


normalGenre "Acao"              = "Ação"
normalGenre "Animacao"          = "Animação"
normalGenre "Comedia"           = "Comédia"
normalGenre "Epico"             = "Épico"
normalGenre "Ficcao"            = "Ficção Científica"
normalGenre "Ficção"            = "Ficção Científica"
normalGenre "Ficção científica" = "Ficção Científica"
normalGenre x                   = x

normalCountry "Inglaterra" = "Reino Unido"
normalCountry x            = x
