{- Time-stamp: "2013-01-19 15:45:51 romildo" -}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DoAndIfThenElse #-}

module InternetSearch where

import Debug.Trace (trace)
import qualified Data.ByteString.Lazy.Char8 as LB.Char8
-- import Network.URI (isAllowedInURI, escapeURIString)
import Text.HTML.TagSoup (parseTags)
-- import Text.Printf (printf)
import Text.Parsec (parse)
import Graphics.UI.Gtk hiding (Release,after)
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Monad (forM, forM_, unless, when)
--import qualified Control.Exception as Ex
import Control.Exception (SomeException,try)
import System.Directory (removeFile)
import System.IO (withFile, IOMode(WriteMode))
import System.FilePath (takeExtension)
import System.Process (runProcess)
import Data.IORef (readIORef)
-- import Data.List (findIndex)
import Data.Maybe (isJust, fromJust, fromMaybe, mapMaybe)
-- import Data.Char (isAlphaNum, isSpace, isUpper, toLower)
import Text.HTML.TagSoup (Tag)

import Util (replace, trim, after, openURI, openURIString, buildFileName, normalizeFileName, cacheDownload)
import Movie (Info(..), Col(..))
import Component (mkW, mkWA, mkCol, myBoxPackStart, myBoxPackEnd, myBoxPackStartWithMnemonic, myTableAttach, Component(getText,setText), errorMsg, myScrolledWindowNew, setBaseFromBg, restoreBase)
import SiteConfig (SearchResult1(..), SiteConfig(SiteConfig, urlForMovie, parserForSearch, parsersForInfo), webSearch)
import TagSoupParsec (TagParser)
import MovieFormType
import Prefs (Prefs(Prefs,browser))

import IMDB (imdb)
import AdoroCinema (adoroCinema)
import EPipoca (ePipoca)
import Interfilmes (interFilmes)
import DVDWorld (dvdWorld)
import AllCenter (allCenter)
import VideoNorte (videoNorte)
import CapasBrasil (capasbrasil)
import Manicomio (manicomio)


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
     titleEntry <- mkWA entryNew (myBoxPackStartWithMnemonic u PackNatural 4 "_Title")
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

     bs <- forM [ ("IM_DB", imdb)
                , ("_Adoro Cinema", adoroCinema)
                , ("All _Center", allCenter)
                , ("DVD _World", dvdWorld)
                , ("E-_Pipoca", ePipoca)
                , ("_Interfilmes", interFilmes)
                , ("Video _Norte", videoNorte)
                , ("Capas _Brasil", capasbrasil)
                , ("_Manicomio", manicomio)
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
     showMovie parent mov
     unless (isJust (lookup Cover mov)) $
       maybe (return ()) (safeRM . fst) (lookup Cover mov)

getMovieSection ref parent buildURL (section,infoParsers) =
  do let uri = buildURL section
     putStrLn ("==> SECTION ==> " ++ uri)
     src <- cacheDownload (openURIString [] uri) uri []
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
     let mergeable = [ Rating ]
     let needArea = [ Cast, Synopsis, Awards, Curiosities, Notes ]
     --
     scr <- mkWA (myScrolledWindowNew PolicyAutomatic PolicyAutomatic) (myBoxPackStart u PackGrow 2)
                 [ containerBorderWidth := 2
                 , widgetWidthRequest := 800
                 , widgetHeightRequest := 600
                 ]
     table <- mkWA (tableNew 0 3 False) (scrolledWindowAddWithViewport scr) [ containerBorderWidth := 2 ]

     -- box <- mkWA (hBoxNew False 2) (myBoxPackStart u PackNatural 2) [containerBorderWidth := 2]
     -- butSelNew <- mkWA (buttonNewWithMnemonic "Select Only _New") (myBoxPackStart box PackNatural 2) []
     -- butSelAll <- mkWA (buttonNewWithMnemonic "_Select All") (myBoxPackStart box PackNatural 2) []
     -- butUnsAll <- mkWA (buttonNewWithMnemonic "_Unselect All") (myBoxPackStart box PackNatural 2) []

     let add (r,(i,(x,c))) =
           do y <- getText c
              align <- mkW (alignmentNew 0 0 0 0) (myTableAttach table 0 1 r (r+2) [Fill] [Fill] 0 0)
              label <- mkWA (labelNewWithMnemonic ('_':show i)) (containerAdd align) []
              align <- mkW (alignmentNew 0 0 0 0) (myTableAttach table 2 3 (r+0) (r+1) [Fill] [Fill] 0 0)
              buttonA <- mkWA checkButtonNew (containerAdd align) [toggleButtonActive := null y || elem i mergeable]
              align <- mkW (alignmentNew 0 0 0 0) (myTableAttach table 2 3 (r+1) (r+2) [Fill] [Fill] 0 0)
              buttonB <- mkWA checkButtonNew (containerAdd align) [toggleButtonActive := not (null y)]
              (w1,w2) <-
                if i == Cover
                then do w1 <- mkWA (imageNewFromFile x) (myTableAttach table 1 2 (r+0) (r+1) [Fill] [Fill] 0 0) [widgetTooltipText := Just x]
                        w2 <- mkWA (imageNewFromFile y) (myTableAttach table 1 2 (r+1) (r+2) [Fill] [Fill] 0 0) [widgetTooltipText := Just y]
                        return (toWidget w1,toWidget w2)
                else if elem i needArea
                     then do scr <- mkWA (myScrolledWindowNew PolicyAutomatic PolicyAlways) (myTableAttach table 1 2 (r+0) (r+1) [Fill] [Fill] 0 0)
                                         [containerBorderWidth := 2]
                             view1 <- mkWA textViewNew (containerAdd scr)
                                           [ -- widgetWidthRequest := 400
                                           -- , 
                                             widgetHeightRequest := 20
                                           , textViewEditable := False
                                           , textViewWrapMode := WrapWord
                                           ]
                             buf <- textViewGetBuffer view1
                             textBufferSetText buf x
                             scr <- mkWA (myScrolledWindowNew PolicyAutomatic PolicyAlways) (myTableAttach table 1 2 (r+1) (r+2) [Fill] [Fill] 0 0)
                                         [containerBorderWidth := 2]
                             view2 <- mkWA textViewNew (containerAdd scr)
                                           [ -- widgetWidthRequest := 400
                                           -- , 
                                             widgetHeightRequest := 20
                                           , textViewEditable := False
                                           , textViewWrapMode := WrapWord
                                           ]
                             buf <- textViewGetBuffer view2
                             textBufferSetText buf y
                             return (toWidget view1,toWidget view2)
                     else do w1 <- mkWA entryNew (myTableAttach table 1 2 (r+0) (r+1) [Fill] [Fill] 0 0) [entryWidthChars := 70, entryEditable := False, entryText := x ]
                             w2 <- mkWA entryNew (myTableAttach table 1 2 (r+1) (r+2) [Fill] [Fill] 0 0) [entryWidthChars := 70, entryEditable := False, entryText := y ]
                             return (toWidget w1,toWidget w2)
              when (not (null y || elem i mergeable)) $ setBaseFromBg w1
              when (null y) $  setBaseFromBg w2
              mkW hSeparatorNew (myTableAttach table 0 3 (r+2) (r+3) [Fill] [Fill] 0 0)
              --
              forM_ (zip [buttonA,buttonB] [w1,w2]) $ \(b,w) ->
                on b toggled $ do active <- toggleButtonGetActive b
                                  (if active then restoreBase else setBaseFromBg) w
              --
              return ((i,(x,c)),([buttonA,buttonB],null y))

     buts <- mapM add (zip [0,3..] movie)
     --
     -- onClicked butSelNew $ forM_ buts $ \(_,(b,wasEmpty)) -> toggleButtonSetActive b wasEmpty
     -- onClicked butSelAll $ forM_ buts $ \(_,(b,_)) -> toggleButtonSetActive b True
     -- onClicked butUnsAll $ forM_ buts $ \(_,(b,_)) -> toggleButtonSetActive b False
     --
     widgetShowAll u
     resp <- dialogRun d
     widgetDestroy d `after`
       case resp of
         ResponseAccept ->
           forM_ buts $ \((i,(x,c)),([buttonA,buttonB],_)) ->
             do a <- toggleButtonGetActive buttonA
                b <- toggleButtonGetActive buttonB
                if (a && b)
                   then getText c >>= setText c . mergeContents i x
                   else if a
                        then setText c x
                        else if not b
                             then setText c ""
                             else return ()
         _ ->
           return ()

mergeContents i new old = new ++ "\n" ++ old


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
       do src <- cacheDownload (openURI [] url) url []
          case src of
            Left _ -> return ""
            Right xs -> do LB.Char8.hPut h xs
                           return fileName






safeRM path =
  do try (removeFile path) :: IO (Either SomeException ())
     return ()




{-
sitesForSearchPanel pack =
  do vbox1 <- mkW (vBoxNew False 6) pack
     label <- mkWA (labelNewWithMnemonic "<b>_Sites For Searching</b>") (myBoxPackStart vbox1 PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     box1 <- mkW (hBoxNew False 6) (myBoxPackStart vbox1 PackNatural 0)
     model3 <- listStoreNew [ (True, "IMDB", imdb)
                            , (True, "Adoro Cinema", adoroCinema)
                            , (True, "e-pipoca", ePipoca)
                            , (True, "Interfilmes", interFilmes)
                            , (True, "DVD World", dvdWorld)
                            , (True, "All Center", allCenter)
                            , (True, "Video Norte", videoNorte)
                            ]
     view3 <- mkWA (treeViewNewWithModel model3) (myBoxPackStart box1 PackGrow 0) [treeViewHeadersVisible := False]
     labelSetMnemonicWidget label view3
     rMark3 <- mkCol view3 model3 "_Mark" cellRendererToggleNew [cellToggleActivatable := True] $ \(x,_,_) -> [ cellToggleActive := x ]
     mkCol view3 model3 "Site" cellRendererTextNew [] $ \(_,x,_) -> [ cellText := x ]
     box2 <- mkW (vBoxNew False 6) (myBoxPackStart box1 PackNatural 0)
     upButton <- mkWA buttonNew (myBoxPackStart box2 PackNatural 0) [buttonImage :=> imageNewFromStock stockGoUp IconSizeButton]
     downButton <- mkWA buttonNew (myBoxPackStart box2 PackNatural 0) [buttonImage :=> imageNewFromStock stockGoDown IconSizeButton]
     sitesEditButton <- mkWA buttonNew (myBoxPackEnd box2 PackNatural 0) [buttonImage :=> imageNewFromStock stockEdit IconSizeButton]
     --
     on view3 cursorChanged $
       do (path,_) <- treeViewGetCursor view3
          case path of
            [i] -> do n <- listStoreGetSize model3
                      widgetSetSensitive upButton (i > 0)
                      widgetSetSensitive downButton (i < n - 1)
            _   -> return ()
     --
     onClicked upButton $
       do (path,_) <- treeViewGetCursor view3
          case path of
            [i] -> listStoreSwapRows model3 view3 i (i - 1)
            _   -> return ()
     --
     onClicked downButton $
       do (path,_) <- treeViewGetCursor view3
          case path of
            [i] -> listStoreSwapRows model3 view3 i (i + 1)
            _   -> return ()
      --
     onClicked sitesEditButton $
       do sites <- fmap (map trd3) $ listStoreToList model3
          prefs@Prefs{infoSearch} <- readIORef prefsRef
          config <- searchPreferences parent sites infoSearch
          case config of
            Just xs -> writeIORef prefsRef prefs{infoSearch=xs}
            Nothing ->
              return ()
     --
-}
