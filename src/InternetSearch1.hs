{-# LANGUAGE ExistentialQuantification #-}

module InternetSearch {- (internetSearch) -} where

import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Lazy.UTF8 (toString)
import Data.ByteString.Lazy.Char8 (unpack)
import Codec.Compression.GZip (decompress)
import Network.URI (isAllowedInURI, escapeURIString, parseURI)
import Network.HTTP (Request, mkRequest, RequestMethod(GET), simpleHTTP, insertHeader, HeaderName(HdrAcceptCharset, HdrAcceptEncoding, HdrContentEncoding), findHeader, rspBody)
import Network.Stream (ConnError(ErrorMisc))
import Text.HTML.TagSoup (parseTags, sections, partitions, (~==), (~/=), Tag(TagText,TagOpen), fromTagText, innerText)
import Text.Printf (printf)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Char (isSpace)

import Graphics.UI.Gtk hiding (Release)

import TagsoupParsec
import Util (trimLeft, trim)

import Title
import Component


-- steal from getRequest but change type
-- it's lazy bytestring because the GZip library specifies it
myGetRequest :: String -> Request LB.ByteString
myGetRequest s =
  case parseURI s of
    Nothing -> error "url syntax error"
    Just uri -> mkRequest GET uri

openURI uri =
  do src <- simpleHTTP (insertHeader HdrAcceptEncoding "gzip" (insertHeader HdrAcceptCharset "latin1" (myGetRequest uri)))
     return $ case src of
                Left err -> Left err
                Right rsp -> case findHeader HdrContentEncoding rsp of
                               Nothing -> Right (rspBody rsp)
                               Just "gzip" -> Right (decompress (rspBody rsp))
                               Just _ -> Left (ErrorMisc "TODO: other decompressions")


openURIString s = do x <- openURI s
                     return (fmap toString x)

internetSearch components =
  do let Just entry = lookup Title components
     title <- getText entry
     movies <- searchAdoroCinema title
     chooseMovie components movies


searchAdoroCinema title =
  do let uri = escapeURIString isAllowedInURI url
     src <- openURIString uri
     case src of
       Left err -> do print err
                      return []
       Right doc -> do writeFile "test.html" doc
                       let tags = parseTags doc
                       writeFile "tags.txt" (unlines (map show tags))
                       let items = sections (~== "<div class=search-item>") tags
                       return (search items)
  where
    url = "http://www.adorocinema.com/common/search/search_by_film/?criteria=" ++ title

search [] = []
search (item:items) =
  let (TagOpen "a" xs:ks):others = sections (~== "<a class=titulofilme>") item
      (_:TagText original:_):_ = sections (~== "<p>") ks
      Just href = lookup "href" xs
      Just title = lookup "title" xs
      orig = reverse (tail (reverse (drop 15 original)))
  in (title, orig, href) : search items


chooseMovie components movies =
    do d <- dialogNew
       set d [windowDefaultHeight := 300]
       windowSetTitle d "Movie Search Results"
       dialogAddButton d stockCancel ResponseReject
       ok <- dialogAddButton d stockOk ResponseAccept
       widgetGrabDefault ok
       u <- dialogGetUpper d
       --
       scrwin <- mkWA (scrolledWindowNew Nothing Nothing)
                      (boxPackStartDefaults u)
                      [ scrolledWindowHscrollbarPolicy := PolicyNever
                      , scrolledWindowVscrollbarPolicy := PolicyAutomatic
                      , scrolledWindowShadowType       := ShadowIn
                      , containerBorderWidth           := 2
                      ]
       model <- listStoreNew (map (\(x,y,_) -> (x,y)) movies)
       view <- mkWA (treeViewNewWithModel model)
                    (containerAdd scrwin)
                    [ treeViewHeadersVisible := True
                    ]

       r1 <- mkCol view model "Title"          cellRendererTextNew [] $ \(x,_) -> [ cellText := x ]
       r2 <- mkCol view model "Original title" cellRendererTextNew [] $ \(_,x) -> [ cellText := x ]
       --
       on view rowActivated (\_ _ -> dialogResponse d ResponseAccept)
       --
       widgetShowAll u
       resp <- dialogRun d
       case resp of
         ResponseAccept -> do (path,_) <- treeViewGetCursor view
                              case path of
                                [i] -> let (_,_,uri) = movies !! i
                                       in do getMovie components adoroCinema uri
                                             return ()
                                _   -> return ()
         _ -> return ()
       widgetDestroy d


getMovie :: a -> [(Info, [Tag String] -> String)] -> String -> IO ()
getMovie components site uri =
  do putStrLn ("==> GET MOVIE FROM " ++ show "???")
     putStrLn uri
     src <- openURIString uri
     case src of
       Left err -> print err
       Right doc -> do writeFile "test.html" doc
                       let tags = parseTags doc
                       --
                       mapM_ (\(i,f)-> printf "%s: %s\n" (show i) (show (f tags))) site
                       --
                       return ()



notFound = "<NOT FOUND>"


dropUntil3 p1 p2 p3 xs@(x:ys@(y:z:_))
  | p1 x && p2 y && p3 z = xs
  | otherwise = dropUntil3 p1 p2 p3 ys
dropUntil3 _ _ _ _ = []

dropUntil ps [] = []
dropUntil ps xs = go ps xs
  where
    go (q:qs) (y:ys) | q y = go qs ys
                     | otherwise = dropUntil ps (tail xs)
    go [] _ = xs
    go _ [] = []


adoroCinema =
  [
    ( Title,
      \tags ->
        case sections (~== "<h1>") tags of
          (_:TagText x:_):_ -> x
          _ -> notFound
    )
  , ( Cover,
      \tags ->
        let TagOpen _ xs = head $ filter (~== "<img>") $ dropWhile (~/= "<table id=film-details>") tags
        in fromMaybe notFound (lookup "src" xs)
    )
  , ( OriginalTitle,
      \tags ->
        case dropUntil3 (~== "<strong>") (~== TagText "título original") (~== "</strong>") tags of
          _:_:_:TagText (_:x):_ -> x
          _ -> notFound
    )
  , ( Genres,
      \tags ->
        case dropUntil3 (~== "<strong>") (~== TagText "gênero") (~== "</strong>") tags of
          _:_:_:TagText (_:x):_ -> x
          _ -> notFound
    )
  , ( Length,
      \tags ->
        case dropUntil3 (~== "<strong>") (~== TagText "duração:") (~== "</strong>") tags of
          _:_:_:TagText (_:x):_ -> case reverse x of
                                     'n':'i':'m':' ':y -> reverse y
                                     _ -> notFound
          _ -> notFound
    )
  , ( Country,
      \tags ->
        case dropUntil3 (~== "<strong>") (~== TagText "lançamento: ") (~== "</strong>") tags of
          _:_:_:TagText x:_ -> case dropWhile (/= '(') x of
                                 _:y -> case trimLeft isSpace (reverse y) of
                                          ')':z -> reverse z
                                          _ -> notFound
                                 _ -> notFound
          _ -> notFound
    )
  , ( Year,
      \tags ->
        case dropUntil3 (~== "<strong>") (~== TagText "lançamento: ") (~== "</strong>") tags of
          _:_:_:TagText x:_ -> trim isSpace (takeWhile (/= '(') x)
          _ -> notFound
    )
  , ( Director,
      \tags ->
        case dropUntil [(~== "<strong>"), (~== TagText "direção"), (~== "</strong>"), (~== TagText ""), (~== "<a>"), (~== TagText "")] tags of
          _:_:_:_:_:TagText x:_ -> x
          _ -> case dropUntil [(~== "<strong>"), (~== TagText "direção"), (~== "</strong>"), (~== TagText "")] tags of
                 _:_:_:TagText (_:x):_ -> trim isSpace x
                 _ -> notFound

    )
  , ( Site,
      \tags ->
        case dropUntil [(~== "<strong>"), (~== TagText "site oficial"), (~== "</strong>"), (~== TagText ""), (~== "<a>"), (~== TagText "")] tags of
          _:_:_:_:_:TagText x:_ -> x
          _ -> notFound
    )
  , ( Studio,
      \tags ->
        case dropUntil3 (~== "<strong>") (~== TagText "estúdio") (~== "</strong>") tags of
          _:_:_:TagText (_:x):_ -> x
          _ -> notFound
    )
  , ( Distributor,
      \tags ->
        case dropUntil3 (~== "<strong>") (~== TagText "distribuidora") (~== "</strong>") tags of
          _:_:_:TagText (_:x):_ -> x
          _ -> notFound
    )
  , ( Screenwriter,
      \tags ->
        case dropUntil3 (~== "<strong>") (~== TagText "roteiro") (~== "</strong>") tags of
          _:_:_:TagText (_:x):_ -> x
          _ -> notFound
    )
  , (Producer,
     \tags ->
       case dropUntil3 (~== "<strong>") (~== TagText "produção") (~== "</strong>") tags of
         _:_:_:TagText (_:x):_ -> x
         _ -> notFound
    )
  , ( Music,
      \tags ->
        case dropUntil3 (~== "<strong>") (~== TagText "música") (~== "</strong>") tags of
          _:_:_:TagText (_:x):_ -> x
          _ -> notFound
    )
  , ( Photography,
      \tags ->
        case dropUntil3 (~== "<strong>") (~== TagText "fotografia") (~== "</strong>") tags of
          _:_:_:TagText (_:x):_ -> x
          _ -> notFound
    )
  , ( ArtDirection,
      \tags ->
        case dropUntil3 (~== "<strong>") (~== TagText "direção de arte") (~== "</strong>") tags of
          _:_:_:TagText (_:x):_ -> x
          _ -> notFound
    )
  , ( Figurino,
      \tags ->
        case dropUntil3 (~== "<strong>") (~== TagText "figurino") (~== "</strong>") tags of
          _:_:_:TagText (_:x):_ -> x
          _ -> notFound
    )
  , ( Editor,
      \tags ->
        case dropUntil3 (~== "<strong>") (~== TagText "edição") (~== "</strong>") tags of
          _:_:_:TagText (_:x):_ -> x
          _ -> notFound
    )
  ,  ( SpecialEffects,
       \tags ->
         case dropUntil3 (~== "<strong>") (~== TagText "efeitos especiais") (~== "</strong>") tags of
           _:_:_:TagText (_:x):_ -> x
           _ -> notFound
     )
  , ( Synopsis,
      \tags ->
        case dropUntil [(~== "<div>"), (~== "<h4>"), (~== TagText "sinopse:"), (~== "</h4>"), (~== "</div>"), (~== TagText ""), (~== "<p>"), (~== TagText ""), (~== "</p>")] tags of
          _:_:_:_:_:_:_:TagText x:_ -> x
          _ -> case dropUntil [(~== "<div>"), (~== "<h4>"), (~== TagText "sinopse:"), (~== "</h4>"), (~== "</div>"), (~== TagText "")] tags of
                 _:_:_:_:_:TagText x:_ -> trim isSpace x
                 _ -> notFound

    )
  , ( Cast,
      \tags -> show $
        case dropUntil [(~== "<div>"), (~== "<h4>"), (~== TagText "elenco:"), (~== "</h4>"), (~== "</div>"), (~== TagText ""), (~== "<ul>")] tags of
          _:_:_:_:_:_:_:x ->
            let actor tags =
                  case dropUntil [(~== "<li>"), (~== "<a>"), (~== TagText ""), (~== "</a>"), (~== TagText ""), (~== "</li>") ] tags of
                    _:_:TagText x:_:TagText (' ':'(':y):_ -> Just (x, init y)
                    _ -> case dropUntil [(~== "<li>"), (~== TagText ""), (~== "</li>") ] tags of
                           _:TagText x:_ -> case span (/='(') (trim isSpace x) of
                                              (a,'(':b) -> Just (trim isSpace a, init b)
                                              ("","") -> Nothing
                                              (a,"") -> Just (a,"")
                           _ -> Nothing
            in catMaybes (map actor (partitions (~== "<li>") (takeWhile (~/= "</ul>") x)))
          _ -> []
    )
  , ( Awards,
      \tags -> show $
        case dropUntil [(~== "<div>"), (~== "<h4>"), (~== TagText "prêmios:"), (~== "</h4>"), (~== "</div>"), (~== TagText ""), (~== "<div>")] tags of
          _:_:_:_:_:_:_:x -> map innerText (partitions (~== "<p>") (takeWhile (~/= "</div>") x))
          _ -> []
    )
  , ( Curiosities,
      \tags -> show $
        case dropUntil [(~== "<div>"), (~== "<h4>"), (~== TagText "curiosidades"), (~== "</h4>"), (~== "</div>"), (~== TagText ""), (~== "<div>")] tags of
          _:_:_:_:_:_:_:x -> [innerText (takeWhile (~/= "</div>") x)]
          _ -> []
    )
  ]
