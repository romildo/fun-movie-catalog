{- Time-stamp: "2012-12-04 16:26:39 romildo" -}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DoAndIfThenElse #-}

module InternetSearch where

import Debug.Trace (trace)
import qualified Data.ByteString.Lazy.Char8 as LB.Char8
import Network.URI (isAllowedInURI, escapeURIString)
import Text.HTML.TagSoup (parseTags)
import Text.Printf (printf)
import Text.Parsec (parse)
import Graphics.UI.Gtk hiding (Release,after)
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Monad (forM, forM_, filterM, unless)
--import qualified Control.Exception as Ex
import Control.Exception (SomeException,try)
import System.Directory (doesFileExist, doesDirectoryExist, removeFile)
import System.IO (withFile, IOMode(WriteMode))
import System.FilePath (takeExtension, addExtension, combine)
import System.Process (runProcess)
import Data.IORef (IORef, readIORef)
import Data.List (findIndex)
import Data.Maybe (isJust, fromJust, fromMaybe, mapMaybe)
import Data.Char (isAlphaNum, isSpace, isUpper, toLower)
import Text.HTML.TagSoup (Tag)

import Util (replace, trim, after, openURI, openURIString, submitGETForm, findIndexM, buildFileName, normalizeFileName, cacheDownload)
import Movie (Movie, Info(..), Col(..))
import Component (mkW, mkWA, mkCol, myBoxPackStart, myBoxPackEnd, myBoxPackStartWitMnemonic, myTableAttach, myTableAttachDefaults, packInTableD, Component(getText,setText), errorMsg, myScrolledWindowNew, setBaseFromBg)
import SiteConfig (SearchResult1(..), SiteConfig(SiteConfig, urlForMovie, parserForSearch, parsersForInfo), webSearch)
import TagSoupParsec (TagParser(..))
import MovieFormType
import Prefs (Prefs(Prefs,browser))

import IMDB (imdb)
import AdoroCinema (adoroCinema)
import EPipoca (ePipoca)
import InterFilmes (interFilmes)
import DVDWorld (dvdWorld)
import AllCenter (allCenter)
import VideoNorte (videoNorte)


internetSearch parent ref prefsRef movieWidgets@MovieForm{mfInfo=components} =
  do movieTitle <- fmap trim $ getText (fromJust (lookup Title components))
     siteParsers <- newIORef Nothing
     --
     d <- dialogNew
     set d [windowTransientFor := parent, containerBorderWidth := 2]
     windowSetTitle d "Search Movie in WEB"
     closeButton <- dialogAddButton d stockClose ResponseClose
     widgetGrabDefault closeButton
     u <- dialogGetUpper d
     --
     titleEntry <- mkWA entryNew (myBoxPackStartWitMnemonic u PackNatural 4 "_Title")
                        [entryWidthChars := 40, entryText := movieTitle]
     --
     hBox <- mkW (hBoxNew False 2) (myBoxPackStart u PackGrow 2)
     vBox1 <- mkW (vBoxNew False 2) (myBoxPackStart hBox PackNatural 2)
     vBox2 <- mkW (vBoxNew False 2) (myBoxPackStart hBox PackGrow 2)
     --
     lab <- mkWA (labelNew (Just "Found movies")) (myBoxPackStart vBox2 PackNatural 2) [miscXalign := 0]
     scr <- mkWA (myScrolledWindowNew PolicyNever PolicyAutomatic) (myBoxPackStart vBox2 PackGrow 2) [containerBorderWidth := 2, widgetHeightRequest := 300]
     model <- listStoreNew []
     view <- mkWA (treeViewNewWithModel model) (containerAdd scr) [treeViewHeadersVisible := True]
     mkCol view model "Title"          cellRendererTextNew [] $ \SR1{sr1Title=x}    -> [ cellText := x ]
     mkCol view model "Original title" cellRendererTextNew [] $ \SR1{sr1Original=x} -> [ cellText := x ]
     mkCol view model "Country"        cellRendererTextNew [] $ \SR1{sr1Country=x}  -> [ cellText := x ]
     mkCol view model "Year"           cellRendererTextNew [] $ \SR1{sr1Year=x}     -> [ cellText := x ]
     mkCol view model "Genres"         cellRendererTextNew [] $ \SR1{sr1Genres=x}   -> [ cellText := x ]
     mkCol view model "Media"          cellRendererTextNew [] $ \SR1{sr1Media=x}    -> [ cellText := x ]
     box <- mkW (hBoxNew False 2) (myBoxPackStart vBox2 PackGrow 2)
     urlEntry <- mkWA entryNew (myBoxPackStart box PackGrow 2) [entryWidthChars := 60, entryEditable := False]
     urlButton <- mkWA (buttonNewWithMnemonic "_Browse") (myBoxPackStart box PackNatural 2) [widgetSensitive := False]
     setBaseFromBg urlEntry
     --
     mkWA (labelNew (Just "Sites")) (myBoxPackStart vBox1 PackNatural 2) [miscXalign := 0]
     getButton <- mkWA (buttonNewWithMnemonic "_Get Movie Data") (myBoxPackEnd vBox1 PackNatural 2) [widgetSensitive := False]
     mkWA (alignmentNew 0 0 1 1) (myBoxPackEnd vBox1 PackGrow 2) [widgetHeightRequest := 10]
     --
     on view cursorChanged $
        do (path,_) <- treeViewGetCursor view
           case path of
             [i] -> do SR1{sr1URL=url} <- listStoreGetValue model i
                       entrySetText urlEntry url
                       widgetSetSensitive urlButton True
                       widgetSetSensitive getButton True
             _ -> return ()

     onClicked urlButton $
       do url <- entryGetText urlEntry
          Prefs{browser} <- readIORef prefsRef
          if null browser
          then errorMsg parent "Web browser not defined in preferences"
          else runProcess browser [url] Nothing Nothing Nothing Nothing Nothing >> return ()

     onClicked getButton $
       do (path,_) <- treeViewGetCursor view
          case path of
            [i] -> do Just site <- readIORef siteParsers
                      mov <- listStoreGetValue model i
                      getMovie ref parent movieWidgets site mov
            _ -> return ()


     let searchSite siteName site@SiteConfig{parserForSearch} =
           do writeIORef siteParsers (Just site)
              title <- entryGetText titleEntry
              src <- webSearch site (trim title)
              case src of
                Left err ->
                  do errorMsg parent (show err)
                     return ()
                Right doc ->
                  do writeFile ("search.tags.html") doc
                     let tags = parseTags doc
                     writeFile "search.tags.hs" (unlines (map show tags))
                     case parse parserForSearch "tagsoup" tags of
                       Left err ->
                         errorMsg parent (show err)
                       Right movies ->
                         do listStoreClear model
                            labelSetText lab ("Found movies at " ++ filter (/='_') siteName)
                            forM_ movies $ listStoreAppend model
                            treeViewSetCursor view [0] Nothing
                            widgetSetSensitive getButton (length movies > 0)

     bs <- forM [ ("I_MDB", imdb)
                , ("_Adoro Cinema", adoroCinema)
                , ("All _Center", allCenter)
                , ("DVD _World", dvdWorld)
                , ("E-_Pipoca", ePipoca)
                , ("_InterFilmes", interFilmes)
                , ("Video _Norte", videoNorte)
                ] $
           \(label,site) ->
             do but <- mkWA (buttonNewWithMnemonic label) (myBoxPackStart vBox1 PackNatural 0)
                            [widgetSensitive := not (null movieTitle)]
                onClicked but (searchSite label site)
                return but
     --
     on titleEntry editableChanged $
       do title <- entryGetText titleEntry
          forM_ bs $ \b -> widgetSetSensitive b (not (null (trim title)))
     --
     widgetShowAll u
     dialogRun d
     widgetDestroy d


getMovie ref parent movieWidgets@MovieForm{mfInfo=components} SiteConfig{urlForMovie,parsersForInfo} SR1{sr1Title=title,sr1URL=uri} =
  do putStrLn ("==> TITLE: " ++ title)
     putStrLn ("==> URI:" ++ uri)
     movie <- fmap concat $ forM parsersForInfo $ getMovieSection ref parent (urlForMovie uri)
     mapM_ print movie
     --
     let mov = mapMaybe (\(i,x) -> fmap ((i,).(x,)) (lookup i components)) movie
     res <- showMovie parent mov
     forM_ res $ \(_,(x,c)) -> setText c x
     unless (isJust (lookup Cover res)) $
       maybe (return ()) (safeRM . fst) (lookup Cover mov)

getMovieSection ref parent buildURL (section,infoParsers) =
  do let uri = buildURL section
     putStrLn ("==> SECTION ==> " ++ uri)
     src <- cacheDownload (openURIString uri) uri []
     case src of
       Left err ->
         do errorMsg parent (show err)
            return []
       Right doc ->
         do let fname = replace "/" ":" ("movie-" ++ section ++ ".tags")
            writeFile (fname ++ ".html") doc
            let tags = parseTags doc
            writeFile (fname ++ ".hs") (unlines (map show tags))
            let f :: [TagParser String [(Info,String)]]
                f = infoParsers
            xss <- forM infoParsers $ parseMovieField tags
            print (show xss)
            let xs = concat xss
            let title = fromMaybe "" (lookup Title xs)
            --
            forM xs $ \info ->
              case info of
                (Cover,url@(_:_)) -> downloadCover ref title url >>= return . (Cover,)
                x -> return x

{-
getMovieSection1 ref parent baseURI (middlePath,infoParsers) =
  do let uri = baseURI ++ middlePath ++ "/"
     putStrLn ("==> GET MOVIE SECTION ==> " ++ uri)
     src <- openURIString uri
     case src of
       Left err ->
         do errorMsg parent (show err)
            return []
       Right doc ->
         do writeFile ("movie-" ++ middlePath ++ ".tags.html") doc
            let tags = parseTags doc
            writeFile ("movie-" ++ middlePath ++ ".tags.hs") (unlines (map show tags))
            xs <- forM infoParsers $ parseMovieField tags
            let title = fromMaybe "" (lookup Title xs)
            --
            forM xs $ \info ->
              case info of
                (Cover,url@(_:_)) -> downloadCover ref title url >>= return . (Cover,)
                x -> return x
-}

showMovie parent movie =
  do d <- dialogNew
     set d [windowTransientFor := parent]
     windowSetTitle d "Movie Information"
     dialogAddButton d stockCancel ResponseReject
     ok <- dialogAddButton d stockOk ResponseAccept
     widgetGrabDefault ok
     u <- dialogGetUpper d
     --
     let needArea = [ Cast, Synopsis, Awards, Curiosities, Notes ]
     --
     scr <- mkWA (myScrolledWindowNew PolicyAutomatic PolicyAlways) (myBoxPackStart u PackGrow 2)
                 [ containerBorderWidth := 2
                 , widgetWidthRequest := 800
                 , widgetHeightRequest := 600
                 ]
     table <- mkWA (tableNew 0 2 False) (scrolledWindowAddWithViewport scr) [ containerBorderWidth := 2 ]
     box <- mkWA (hBoxNew False 2) (myBoxPackStart u PackNatural 2) [containerBorderWidth := 2]

     butSelNew <- mkWA (buttonNewWithMnemonic "Select Only _New") (myBoxPackStart box PackNatural 2) []
     butSelAll <- mkWA (buttonNewWithMnemonic "_Select All") (myBoxPackStart box PackNatural 2) []
     butUnsAll <- mkWA (buttonNewWithMnemonic "_Unselect All") (myBoxPackStart box PackNatural 2) []

     let add (r,(i,(x,c))) =
           do y <- getText c
              align <- mkW (alignmentNew 0 0 0 0) (myTableAttach table 0 1 r (r+1) [Fill] [Fill] 0 0)
              but <- mkWA (checkButtonNewWithMnemonic ('_':show i)) (containerAdd align) [toggleButtonActive := null y]
              box <- mkWA (vBoxNew False 2) (myTableAttachDefaults table 1 2 r (r+1)) [containerBorderWidth := 2]
              if i == Cover
              then do boxH <- mkWA (hBoxNew False 2) (myBoxPackStart box PackNatural 0) [containerBorderWidth := 2]
                      frame <- mkW frameNew (myBoxPackStart boxH PackNatural 0)
                      mkWA (imageNewFromFile x) (containerAdd frame) [widgetTooltipText := Just x]
                      mkWA (imageNewFromFile y) (myBoxPackStart boxH PackNatural 0) [widgetTooltipText := Just y]
                      return ()
              else if elem i needArea
                   then do scr <- mkWA (myScrolledWindowNew PolicyAutomatic PolicyAlways) (myBoxPackStart box PackNatural 0)
                                       [containerBorderWidth := 2]
                           view <- mkWA textViewNew (containerAdd scr)
                                        [ widgetWidthRequest := 400
                                        , widgetHeightRequest := 16
                                        , textViewEditable := False
                                        , textViewWrapMode := WrapWord
                                        ]
                           buf <- textViewGetBuffer view
                           textBufferSetText buf x
                           scr <- mkWA (myScrolledWindowNew PolicyAutomatic PolicyAlways) (myBoxPackStart box PackNatural 0)
                                       [containerBorderWidth := 2]
                           view <- mkWA textViewNew (containerAdd scr)
                                        [ widgetWidthRequest := 400
                                        , widgetHeightRequest := 16
                                        , textViewEditable := False
                                        , textViewWrapMode := WrapWord
                                        ]
                           buf <- textViewGetBuffer view
                           textBufferSetText buf y
                           setBaseFromBg view
                   else do mkWA entryNew (myBoxPackStart box PackNatural 0)
                                [entryWidthChars := 60, entryEditable := False, entryText := x]
                           w <- mkWA entryNew (myBoxPackStart box PackNatural 0)
                                     [entryWidthChars := 60, entryEditable := False, entryText := y]
                           setBaseFromBg w
              return ((i,(x,c)),(but,null y))

     buts <- mapM add (zip [0..] movie)
     --
     onClicked butSelNew $ forM_ buts $ \(_,(b,wasEmpty)) -> toggleButtonSetActive b wasEmpty
     onClicked butSelAll $ forM_ buts $ \(_,(b,_)) -> toggleButtonSetActive b True
     onClicked butUnsAll $ forM_ buts $ \(_,(b,_)) -> toggleButtonSetActive b False
     --
     widgetShowAll u
     resp <- dialogRun d
     widgetDestroy d `after`
       case resp of
         ResponseAccept ->
           do ks <- filterM (toggleButtonGetActive . fst . snd) buts
              return (map fst ks)
         _ ->
           return []



parseMovieField :: [Tag String] -> TagParser String [(Info,String)] -> IO [(Info,String)]
parseMovieField tags p =
  do let res = either (const []) id (parse p "tagsoup" tags)
     putStrLn (show res)
     return res






downloadCover ref title url =
  do Col{imageDir=mTempDir} <- readIORef ref
     let tempDir = fromMaybe "." mTempDir
     fileName <- buildFileName tempDir (normalizeFileName title) (takeExtension url)
     putStrLn ("==> COVER: " ++ fileName)
     withFile fileName WriteMode $ \h ->
       do src <- cacheDownload (openURI url) url []
          case src of
            Left _ -> return ""
            Right xs -> do LB.Char8.hPut h xs
                           return fileName






safeRM path =
  do try (removeFile path) :: IO (Either SomeException ())
     return ()
