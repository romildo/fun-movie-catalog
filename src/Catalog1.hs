{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Catalog where

import Graphics.UI.Gtk hiding (Release, after)
-- import Graphics.UI.Gtk.WebKit.WebView (webViewNew, webViewLoadUri)
import Text.Printf (printf)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Char (toLower)
import Data.Maybe (isJust, isNothing, fromJust, catMaybes, fromMaybe, mapMaybe)
import Data.Either (partitionEithers)
import Data.List (find, findIndex, sortBy, stripPrefix, partition, nub, (\\))
import Data.List.Utils (split, join)
import qualified Data.Map as Map
import Data.Array.IArray (Array, listArray, assocs, (!), (//))
import System.FilePath (takeFileName, takeExtension, splitExtension, combine)
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.Process (runProcess)
import Text.HTML.TagSoup (parseTags)
import Text.Parsec (parse)
import Control.Monad (forM, forM_, foldM_, when, unless)
import Data.Tree (Tree(Node,rootLabel,subForest), Forest, flatten)
import Control.Concurrent (yield, forkIO, killThread)
import Text.EditDistance (defaultEditCosts, levenshteinDistance)
import Debug.Trace (trace)
import qualified Data.ConfigFile as CF

import Movie (Info(Title,OriginalTitle,Year,Cover,File,InclusionDate,Genres), Movie(..), Col(..), infoBounds, infoMap, emptyInfo, emptyMovie, emptyCol)
import MovieForm (movieFormGetMovie, movieFormSetMovie)
import SiteConfig (SearchResult1(..), SiteConfig(..), webSearch)
import IMDB (imdb)
import AdoroCinema (adoroCinema)
import EPipoca (ePipoca)
import InterFilmes (interFilmes)
import DVDWorld (dvdWorld)
import AllCenter (allCenter)
import VideoNorte (videoNorte)
import TagSoupParsec (TagParser(..))
import Util (fst3, snd3, trd3, snd4, after, trim, fromFilePath, anyM, date, findIndexM)
import Latex (latexMovie, latexA, latexZ)
import Search (guess, mergeMovie, getMovieSection, downCover)
import Component (mkW, mkWA, mkCol, myBoxPackStart, myBoxPackEnd, myBoxPackStartWitMnemonic, myTableAttach, packInTableD, myScrolledWindowNew, errorMsg, radioButtonNewWithMnemonicGroup, setBaseFromBg, listStoreSwapRows)
import Prefs


data Mv =
  Mv
  { mvTitle    :: String
  , mvOriginal :: String
  , mvYear     :: String
  , mvCountry  :: String
  , mvFile     :: String
  , mvTitleCol :: Maybe Int
  , mvFileCol  :: Maybe Int
  , mvMark     :: Bool
  , mvNumber   :: Int
  , mvResult   :: Forest SearchResult
  , mvFound    :: Bool
  , mvData     :: [(Info,String)]
  }

data SearchResult =
  SR
  { srTitle    :: String
  , srOriginal :: String
  , srYear     :: String
  , srCountry  :: String
  , srGenres   :: String
  , srMedia    :: String
  , srMark     :: Bool
  , srURL      :: String
  , srSite     :: SiteConfig
  }
  deriving (Show)

data Action = AScan
            | ASearch
            | ADownload

catalog parent model0 view0 movieForm colRef prefsRef =
  do -- get movie currently displayed in the movie form ...
     movie <- movieFormGetMovie movieForm (emptyMovie 1)
     -- .. and update the collection model and view
     (path,_) <- treeViewGetCursor view0
     unless (null path) $
       listStoreSetValue model0 (head path) movie
     -- get all movies in the collection
     allMovies <- listStoreToList model0
     --
     -- the dialog
     d <- dialogNew
     set d [windowTitle := "Movie Catalog", windowTransientFor := parent, containerBorderWidth := 6]
     -- dialog action area
     a <- dialogGetActionArea d
     cancelButton <- dialogAddButton d stockCancel ResponseReject
     stopButton <- mkW (buttonNewFromStock stockStop) (myBoxPackStart a PackNatural 0)
     scanButton <- mkW (buttonNewWithMnemonic "S_can Directory") (myBoxPackStart a PackNatural 0)
     searchButton <- mkW (buttonNewWithMnemonic "Search Movies in _WEB") (myBoxPackStart a PackNatural 0)
     downloadButton <- mkW (buttonNewWithMnemonic "_Download Movie Data") (myBoxPackStart a PackNatural 0)
     okButton <- dialogAddButton d stockOk ResponseAccept
     mapM_ (flip widgetSetSensitive False) [stopButton, searchButton, downloadButton, okButton]
     -- dialog main (upper) area
     u <- dialogGetUpper d
     boxSetSpacing u 2
     hbox <- mkW (hBoxNew False 6) (myBoxPackStart u PackGrow 0)
     vbox1 <- mkW (vBoxNew False 3) (myBoxPackStart hbox PackNatural 0)
     vbox2 <- mkW (vBoxNew False 3) (myBoxPackStart hbox PackGrow 0)
     progress <- mkWA progressBarNew (myBoxPackEnd vbox1 PackNatural 0) []
     progressBarSetText progress "No action"
     actionLabel <- mkWA (labelNew (Just "<b>No Action Done</b>")) (myBoxPackEnd vbox1 PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     mkW (hSpaceNew 6) (myBoxPackEnd vbox1 PackNatural 0)
     --
     -- DIRECTORY PANEL
     --
     mkWA (labelNew (Just "<b>Directory</b>")) (myBoxPackStart vbox1 PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     box <- mkW (hBoxNew False 2) (myBoxPackStart vbox1 PackNatural 0)
     label <- mkWA (labelNewWithMnemonic "_Path") (myBoxPackStart box PackNatural 0) [miscXalign := 0]
     chooser <- mkWA (fileChooserButtonNew "Open Directory" FileChooserActionSelectFolder) (myBoxPackStart box PackGrow 0) [fileChooserShowHidden := True]
     labelSetMnemonicWidget label chooser
     --
     box <- mkW (hBoxNew False 2) (myBoxPackStart vbox1 PackNatural 0)
     mkWA (labelNew (Just "Extensions")) (myBoxPackStart box PackNatural 0) [miscXalign := 0]
     extButtons <- forM ["._mkv", "._avi"] $ \ext ->
                     mkWA (checkButtonNewWithMnemonic ext) (myBoxPackStart box PackNatural 0) [toggleButtonActive := True]
     replButton <- mkWA (checkButtonNewWithMnemonic "_Replace Only Empty Fields") (myBoxPackStart vbox1 PackNatural 0) [toggleButtonActive := True]
     --
     mkW (hSpaceNew 6) (myBoxPackStart vbox1 PackNatural 0)
     label <- mkWA (labelNewWithMnemonic "<b>Movie File _List</b>") (myBoxPackStart vbox1 PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     swin <- mkWA (myScrolledWindowNew PolicyAutomatic PolicyAutomatic) (myBoxPackStart vbox1 PackGrow 0) [widgetHeightRequest := 200]
     model1 <- listStoreNew []
     view1 <- mkWA (treeViewNewWithModel model1) (containerAdd swin) [treeViewHeadersVisible := True]
     labelSetMnemonicWidget label view1
     mkCol view1 model1 "#" cellRendererTextNew [] $ \x -> [ cellText := show (mvNumber x) ]
     rMark1 <- mkCol view1 model1 "Mar_k" cellRendererToggleNew [cellToggleActivatable := True] $ \x -> [ cellToggleActive := mvMark x ]
     rTitle <- mkCol view1 model1 "_Title" cellRendererTextNew [cellTextEditable := True] $ \x -> [ cellText := mvTitle x ]
     rYear <- mkCol view1 model1 "_Year" cellRendererTextNew [cellTextEditable := True] $ \x -> [ cellText := mvYear x ]
     mkCol view1 model1 "Tit" cellRendererToggleNew [] $ \x -> [ cellToggleActive := isNothing (mvTitleCol x) ]
     mkCol view1 model1 "Fil" cellRendererToggleNew [] $ \x -> [ cellToggleActive := isNothing (mvFileCol x) ]
     mkCol view1 model1 "Dow" cellRendererToggleNew [] $ \x -> [ cellToggleActive := not (null (mvData x)) ]
     rOrig <- mkCol view1 model1 "_Original Title" cellRendererTextNew [cellTextEditable := True] $ \x -> [ cellText := mvOriginal x ]
     box <- mkW (hBoxNew False 2) (myBoxPackStart vbox1 PackNatural 0)
     label <- mkWA (labelNewWithMnemonic "_File Path") (myBoxPackStart box PackNatural 0) [miscXalign := 0]
     filePathEntry <- mkWA entryNew (myBoxPackStart box PackGrow 0) [entryEditable := False]
     labelSetMnemonicWidget label filePathEntry
     --
     box <- mkW (hBoxNew False 2) (myBoxPackStart vbox1 PackNatural 0)
     allButton <- mkWA (buttonNewWithMnemonic "Select _All") (myBoxPackStart box PackNatural 0) []
     newTitleButton <- mkWA (buttonNewWithMnemonic "Select _New Titles") (myBoxPackStart box PackNatural 0) []
     newFilePathButton <- mkWA (buttonNewWithMnemonic "Select New Fi_les") (myBoxPackStart box PackNatural 0) []
     unsAllButton <- mkWA (buttonNewWithMnemonic "_Unselect All") (myBoxPackEnd box PackNatural 0) []
     --
     mkW (hSpaceNew 6) (myBoxPackStart vbox1 PackNatural 0)
     label <- mkWA (labelNewWithMnemonic "<b>Failed To _Parse File List</b>") (myBoxPackStart vbox1 PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     swin <- mkW (myScrolledWindowNew PolicyAutomatic PolicyAutomatic) (myBoxPackStart vbox1 PackNatural 0)
     model2 <- listStoreNew []
     view2 <- mkWA (treeViewNewWithModel model2) (containerAdd swin) [treeViewHeadersVisible := True]
     labelSetMnemonicWidget label view2
     mkCol view2 model2 "File Path" cellRendererTextNew [] $ \x -> [ cellText := x ]
     --
     -- SITES PANEL
     --
     mkW (hSpaceNew 6) (myBoxPackStart vbox1 PackNatural 0)
     label <- mkWA (labelNewWithMnemonic "<b>_Sites For Searching</b>") (myBoxPackStart vbox1 PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     box1 <- mkW (hBoxNew False 6) (myBoxPackStart vbox1 PackNatural 0)
     swin <- mkW (myScrolledWindowNew PolicyAutomatic PolicyAutomatic) (myBoxPackStart box1 PackGrow 0)
     model3 <- listStoreNew [ (True, "IMDB", imdb)
                            , (True, "Adoro Cinema", adoroCinema)
                            , (True, "e-pipoca", ePipoca)
                            , (True, "Interfilmes", interFilmes)
                            , (True, "DVD World", dvdWorld)
                            , (True, "All Center", allCenter)
                            , (True, "Video Norte", videoNorte)
                            ]
     view3 <- mkWA (treeViewNewWithModel model3) (containerAdd swin) [treeViewHeadersVisible := False]
     labelSetMnemonicWidget label view3
     rMark3 <- mkCol view3 model3 "_Mark" cellRendererToggleNew [cellToggleActivatable := True] $ \(x,_,_) -> [ cellToggleActive := x ]
     mkCol view3 model3 "Site" cellRendererTextNew [] $ \(_,x,_) -> [ cellText := x ]
     box2 <- mkW (vBoxNew False 6) (myBoxPackStart box1 PackNatural 0)
     upButton <- mkWA buttonNew (myBoxPackStart box2 PackNatural 0) [buttonImage :=> imageNewFromStock stockGoUp IconSizeButton]
     downButton <- mkWA buttonNew (myBoxPackStart box2 PackNatural 0) [buttonImage :=> imageNewFromStock stockGoDown IconSizeButton]
     sitesEditButton <- mkWA buttonNew (myBoxPackEnd box2 PackNatural 0) [buttonImage :=> imageNewFromStock stockEdit IconSizeButton]
     --
     -- RESULTS PANEL
     --
     label <- mkWA (labelNewWithMnemonic "<b>Search _Results</b>") (myBoxPackStart vbox2 PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     model4 <- treeStoreNew []
     swin <- mkW (myScrolledWindowNew PolicyAutomatic PolicyAutomatic) (myBoxPackStart vbox2 PackGrow 0)
     view4 <- mkWA (treeViewNewWithModel model4) (containerAdd swin) [treeViewHeadersVisible := True]
     labelSetMnemonicWidget label view4
     treeViewExpandAll view4
     --style <- widgetGetStyle view2
     let color1 = "yellow" --color1 <- styleGetBackground style StateNormal
     let color2 = "white" --color2 <- styleGetLight style StateNormal
     let bg url marked = cellBackground := if not (null url) && marked then color1 else color2
     let vis x = cellVisible := not (null x)
     mkCol view4 model4 "Title" cellRendererTextNew [] $ \x -> [ cellTextMarkup := Just (srTitle x), bg (srURL x) (srMark x) ]
     rMark4 <- mkCol view4 model4 "Mark" cellRendererToggleNew [cellToggleActivatable := True, cellToggleRadio := True] $ \x -> [ cellToggleActive := srMark x, vis (srURL x), cellXAlign := 0.0, bg (srURL x) (srMark x) ]
     mkCol view4 model4 "Original Title" cellRendererTextNew [] $ \x -> [ cellText := srOriginal x, bg (srURL x) (srMark x) ]
     mkCol view4 model4 "Year" cellRendererTextNew [] $ \x -> [ cellText := srYear x, bg (srURL x) (srMark x) ]
     mkCol view4 model4 "Country" cellRendererTextNew [] $ \x -> [ cellText := srCountry x, bg (srURL x) (srMark x) ]
     mkCol view4 model4 "Media" cellRendererTextNew [] $ \x -> [ cellText := srMedia x, bg (srURL x) (srMark x) ]
     box <- mkW (hBoxNew False 6) (myBoxPackStart vbox2 PackNatural 0)
     urlEntry <- mkW entryNew (myBoxPackStartWitMnemonic box PackGrow 0 "URL")
     urlButton <- mkWA (buttonNewWithMnemonic "_Browse") (myBoxPackStart box PackNatural 0) [widgetSensitive := False]
     setBaseFromBg urlEntry
     --
     -- SIGNAL HANDLERS
     --
     childThreadRef <- newIORef Nothing
     --
     let buttonsSetSensitive =
           do anyExts <- anyM toggleButtonGetActive extButtons
              widgetSetSensitive scanButton anyExts
              movies <- listStoreToList model1
              sites <- listStoreToList model3
              widgetSetSensitive searchButton (any mvMark movies && any fst3 sites)
              let f Mv{mvMark,mvResult} =
                    mvMark && any srMark (concatMap flatten mvResult)
              widgetSetSensitive downloadButton (any f movies)
              widgetSetSensitive okButton (any mvMark movies)
     --
     let getSites =
           listStoreToList model3 >>= return . map trd3 . filter fst3
     --
     let updateSearchResults xs =
           do treeStoreClear model4
              treeStoreInsertForest model4 [] 0 xs
              treeViewExpandAll view4
     --
     onClicked allButton $
       do n <- listStoreGetSize model1
          forM_ [0..n-1] $ \i ->
            do x <- listStoreGetValue model1 i
               unless (mvMark x) $
                 listStoreSetValue model1 i x{mvMark=True}
          buttonsSetSensitive
     --
     onClicked unsAllButton $
       do n <- listStoreGetSize model1
          forM_ [0..n-1] $ \i ->
            do x <- listStoreGetValue model1 i
               when (mvMark x) $
                 listStoreSetValue model1 i x{mvMark=False}
          buttonsSetSensitive
     --
     onClicked newTitleButton $
       do n <- listStoreGetSize model1
          forM_ [0..n-1] $ \i ->
            do x <- listStoreGetValue model1 i
               when (not (mvMark x) && isNothing (mvTitleCol x)) $
                 listStoreSetValue model1 i x{mvMark=True}
          buttonsSetSensitive
     --
     onClicked newFilePathButton $
       do n <- listStoreGetSize model1
          forM_ [0..n-1] $ \i ->
            do x <- listStoreGetValue model1 i
               when (not (mvMark x) && isNothing (mvFileCol x)) $
                 listStoreSetValue model1 i x{mvMark=True}
          buttonsSetSensitive
     --
     on view1 cursorChanged $
       do ([i],_) <- treeViewGetCursor view1
          x@Mv{mvFile,mvResult} <- listStoreGetValue model1 i
          entrySetText filePathEntry mvFile
          updateSearchResults mvResult
     --
     on rMark1 cellToggled $ \p ->
       do let [i] = stringToTreePath p
          x@Mv{mvMark=m} <- listStoreGetValue model1 i
          listStoreSetValue model1 i x{mvMark=not m}
          buttonsSetSensitive
     --
     on rTitle edited $ \[i] str ->
       do let str' = trim str
          x <- listStoreGetValue model1 i
          listStoreSetValue model1 i x{mvTitle=str',mvTitleCol=findInCol allMovies Title str'}
     --
     on rOrig edited $ \[i] str ->
       do x <- listStoreGetValue model1 i
          listStoreSetValue model1 i x{mvYear=trim str}
     --
     on rYear edited $ \[i] str ->
       do x <- listStoreGetValue model1 i
          listStoreSetValue model1 i x{mvCountry=trim str}
     --
     onClicked scanButton $
       do childThread <- forkIO $
            do maybeFilePath <- fileChooserGetFilename chooser
               putStrLn (show maybeFilePath)
               when (isJust maybeFilePath) $
                 do widgetSetSensitive stopButton True
                    widgetSetSensitive cancelButton False
                    widgetSetSensitive scanButton False
                    widgetSetSensitive searchButton False
                    widgetSetSensitive downloadButton False
                    widgetSetSensitive okButton False
                    labelSetMarkup actionLabel "<b>Scanning ...</b>"
                    progressBarSetText progress ""
                    progressBarSetFraction progress 0
                    exts <- forM extButtons $ \b ->
                              do active <- toggleButtonGetActive b
                                 if active
                                   then buttonGetLabel b >>= return . Just . filter (/='_')
                                   else return Nothing
                    fs <- searchDirs progress (catMaybes exts) [fromJust maybeFilePath]
                    let xs = map parseFilePath fs
                    let (xs1,xs2) = partitionEithers xs
                    listStoreClear model2
                    forM_ xs1 $ listStoreAppend model2
                    treeViewSetCursor view2 [0] Nothing
                    listStoreClear model1
                    forM_ (zip [1::Int ..] xs2) $ listStoreAppend model1 . \(i,(title,original,year,file)) ->
                      Mv{ mvNumber   = i
                        , mvTitle    = title
                        , mvOriginal = original
                        , mvYear     = year
                        , mvCountry  = ""
                        , mvFile     = file
                        , mvMark     = True
                        , mvTitleCol = findInCol allMovies Title title
                        , mvFileCol  = findInCol allMovies File file
                        , mvResult   = []
                        , mvFound    = False
                        , mvData     = []
                        }
                    treeViewSetCursor view1 [0] Nothing
                    labelSetMarkup actionLabel "<b>Finished Scanning</b>"
                    progressBarSetText progress ""
                    progressBarSetFraction progress 1
                    widgetSetSensitive stopButton False
                    widgetSetSensitive cancelButton True
                    buttonsSetSensitive
          writeIORef childThreadRef (Just (AScan,childThread))
     --
     on rMark3 cellToggled $ \p ->
       do let [i] = stringToTreePath p
          (a,b,c) <- listStoreGetValue model3 i
          listStoreSetValue model3 i (not a,b,c)
          buttonsSetSensitive
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
     let search =
           do progressBarSetFraction progress 0
              movies <- fmap (filter (mvMark . snd) . zip [0::Int ..]) $ listStoreToList model1
              sites <- getSites
              forM_ (zip [1::Int ..] movies) $ \(count,(i,movie)) ->
                do let msg1 = printf "[%i:%i] %s (%s)" count (length movies) (mvTitle movie) (mvYear movie)
                   progressBarSetText progress msg1
                   forM_ (zip [1::Int ..] sites) $ \(j,site@SiteConfig{siteName}) ->
                     do let msg2 = printf "[%i:%i] %s" j (length sites) siteName
                        progressBarSetText progress (msg1 ++ "\n" ++ msg2)
                        res <- searchSite parent movie site
                        let res' = case (sortLike (mvTitle movie, mvYear movie) res) of
                                     m:ms -> m{srMark=True} : ms
                                     [] -> []
                        let x = SR
                                { srTitle    = "<b>"++siteName++"</b>"
                                , srOriginal = ""
                                , srYear     = ""
                                , srCountry  = ""
                                , srGenres   = ""
                                , srMedia    = ""
                                , srMark     = False
                                , srURL      = ""
                                , srSite     = site
                                }
                        let node = Node x (map (flip Node []) res')
                        y@Mv{mvResult=r} <- listStoreGetValue model1 i
                        let mvResult = r ++ [node]
                        listStoreSetValue model1 i y{mvFound = True, mvResult}
                        (path,_) <- treeViewGetCursor view1
                        case path of
                          [k] | i == k -> updateSearchResults mvResult
                          _ -> return ()
                        --putStr "."
                   --putStr ";"
                   progressBarSetFraction progress (fromIntegral count / fromIntegral (length movies))
     --
     let download1 n counter i =
           do Prefs{infoSearch} <- readIORef prefsRef
              movie@Mv{mvMark,mvNumber,mvTitle,mvYear,mvResult,mvFile} <- listStoreGetValue model1 i
              if mvMark
              then do let msg1 = printf "[%i:%i] %s (%s)" counter n mvTitle mvYear
                      progressBarSetText progress msg1
                      when (null mvResult) $ putStrLn msg1
                      ms <- forM mvResult $ \Node{subForest} ->
                        do let xs = filter (\Node{rootLabel=SR{srMark}} -> srMark) subForest
                           forM (zip [1::Int ..] xs) $ \(j,Node{rootLabel=SR{srTitle,srYear,srURL,srSite}}) ->
                             do let msg2 = printf "[%i:%i:%s] %s (%s)" j (length xs) (siteName srSite) srTitle srYear
                                progressBarSetText progress (msg1 ++ "\n" ++ msg2)
                                putStr (msg1 ++ " - " ++ msg2)
                                ys <- downloadMovieInfo srSite srURL
                                return $ fmap (map (\(i,x) -> (i,(siteName srSite,x)))) ys
                      now <- fmap (\(year,month,day) -> printf "%i/%i/%i" year month day) date
                      let m = [(File,mvFile),(InclusionDate,now)] ++ map (tieField infoSearch) (mergeAL (concat (catMaybes (concat ms))))
                      listStoreSetValue model1 i movie{mvData=m}
                      progressBarSetFraction progress (fromIntegral counter / fromIntegral n)
                      return (counter + 1)
              else return counter
     --
     let download =
           do progressBarSetFraction progress 0
              movies <- listStoreToList model1
              n <- listStoreGetSize model1
              foldM_ (download1 (length (filter mvMark movies))) (1::Int) [0 .. n-1]
     --
     onClicked searchButton $
       do childThread <- forkIO $
            do labelSetMarkup actionLabel "<b>Searching ...</b>"
               widgetSetSensitive stopButton True
               widgetSetSensitive cancelButton False
               widgetSetSensitive scanButton False
               widgetSetSensitive searchButton False
               widgetSetSensitive downloadButton False
               widgetSetSensitive okButton False
               search
               labelSetMarkup actionLabel "<b>Finished searching</b>"
               widgetSetSensitive stopButton False
               widgetSetSensitive cancelButton True
               buttonsSetSensitive
          writeIORef childThreadRef (Just (ASearch,childThread))
     --
     onClicked downloadButton $
       do childThread <- forkIO $
            do labelSetMarkup actionLabel "<b>Downloading ...</b>"
               widgetSetSensitive stopButton True
               widgetSetSensitive cancelButton False
               widgetSetSensitive scanButton False
               widgetSetSensitive searchButton False
               widgetSetSensitive downloadButton False
               widgetSetSensitive okButton False
               download
               labelSetMarkup actionLabel "<b>Finished downloading</b>"
               widgetSetSensitive stopButton False
               widgetSetSensitive cancelButton True
               buttonsSetSensitive
          writeIORef childThreadRef (Just (ADownload,childThread))
     --
     on rMark4 cellToggled $ \p ->
       do let [i,j] = stringToTreePath p
          Node{subForest} <- treeStoreGetTree model4 [i]
          treeStoreChange model4 [i,j] $ \x -> x{srMark=True}
          case findIndex (srMark . rootLabel) subForest of
            Just k -> do treeStoreChange model4 [i,k] $ \x -> x{srMark=False}
                         return ()
            Nothing -> return ()
          maybeIter <- treeModelGetIterFirst model4
          let loop Nothing = return []
              loop (Just iter) = do path <- treeModelGetPath model4 iter
                                    t <- treeStoreGetTree model4 path
                                    maybeIter <- treeModelIterNext model4 iter
                                    ts <- loop maybeIter
                                    return (t:ts)

          forest <- loop maybeIter
          (k,_) <- treeViewGetCursor view1
          unless (null k) $
            do movie <- listStoreGetValue model1 (head k)
               listStoreSetValue model1 (head k) movie{mvResult=forest}
          buttonsSetSensitive
     --
     on view1 cursorChanged $
       do ([i],_) <- treeViewGetCursor view1
          Mv{mvTitle,mvOriginal,mvYear,mvFile,mvResult} <- listStoreGetValue model1 i
          treeStoreClear model4
          treeStoreInsertForest model4 [] 0 mvResult
          treeViewExpandAll view4
     --
     on view4 cursorChanged $
       do (path,_) <- treeViewGetCursor view4
          SR{srURL} <- treeStoreGetValue model4 path
          entrySetText urlEntry srURL
          widgetSetSensitive urlButton (not (null srURL))
     --
     onClicked urlButton $
       do url <- entryGetText urlEntry
          Prefs{browser} <- readIORef prefsRef
          if null browser
          then errorMsg parent "Web browser not defined in preferences"
          else runProcess browser [url] Nothing Nothing Nothing Nothing Nothing >> return ()
     --
     onClicked stopButton $
       do Just (action,childThread) <- readIORef childThreadRef
          killThread childThread
          yield
          widgetSetSensitive stopButton False
          widgetSetSensitive cancelButton True
          buttonsSetSensitive
          labelSetMarkup actionLabel $
            case action of
              AScan     -> "<b>Stopped Scanning</b>"
              ASearch   -> "<b>Stopped Searching</b>"
              ADownload -> "<b>Stopped Downloading</b>"
     --
     widgetShowAll u
     response <- dialogRun d
     when (response == ResponseAccept) $
       do n <- listStoreGetSize model1
          forM_ [0::Int .. n-1] $ \i ->
            do movie@Mv{mvMark,mvFileCol,mvData} <- listStoreGetValue model1 i
               when mvMark $
                 case mvFileCol of
                   Just k ->
                     do movie0@M{info=info0} <- listStoreGetValue model0 k
                        replace <- toggleButtonGetActive replButton
                        let mvData' | replace   = filter (null . (info0!) . fst) mvData
                                    | otherwise = mvData
                        listStoreSetValue model0 k movie0{info = info0 // mvData'}
                   Nothing ->
                     do col@Col{greatestMovieId=id} <- readIORef colRef
                        listStoreAppend model0 (emptyMovie (id+1)){info = emptyInfo // mvData}
                        writeIORef colRef col{greatestMovieId=id+1}
     widgetDestroy d





searchDirs _ _ [] = return []
searchDirs progress exts (f:fs) =
  do progressBarPulse progress
     exists <- doesFileExist f
     if exists
     then do let ext = takeExtension f
             if elem (map toLower ext) exts
                then do xs <- searchDirs progress exts fs
                        return (f:xs)
                else searchDirs progress exts fs
     else do exists <- doesDirectoryExist f
             if exists
             then do fs' <- getDirectoryContents f
                     let fs'' = filter (\f -> f /= "." && f /= "..") fs'
                     let fs''' = map (combine f) fs''
                     searchDirs progress exts (fs''' ++ fs)
             else searchDirs progress exts fs


parseFilePath filePath =
  case splitExtension (takeFileName filePath) of
    (fileName@(_:_),fileExt) ->
      case guess (fromFilePath fileName) of
        Nothing -> Left filePath
        Just (title,year) -> Right (title,"",year,filePath)
    _ -> Left filePath


findInCol movies field value =
  findIndex (f) movies
  where
    f movie = info movie ! field == value
      where
        movie' = trace (printf "%s\n%s\n%s\n\n" value (info movie ! field) (show (info movie ! field == value))) movie


searchSite parent Mv{mvTitle=title} site@SiteConfig{parserForSearch} =
  do src <- webSearch site (trim title)
     case src of
       Left err ->
         do errorMsg parent (show err)
            return []
       Right doc ->
         do writeFile ("search.tags.html") doc
            let tags = parseTags doc
            writeFile "search.tags.hs" (unlines (map show tags))
            case parse parserForSearch "tagsoup" tags of
              Left err -> return []
              Right xs ->
                return $ flip map xs $ \SR1{sr1Title=srTitle,sr1Original=srOriginal,sr1Country=srCountry,sr1Year=srYear,sr1Genres=srGenres,sr1Media=srMedia,sr1URL=srURL} ->
                  SR{srTitle,srOriginal,srYear,srCountry,srGenres,srMedia,srURL,srMark=False,srSite=site}


sortLike (title,year) =
  map snd .
  sortBy (\(a,_) (b,_) -> compare a b) .
  map (\m@SR{srTitle,srYear} -> (levenshteinDistance defaultEditCosts (title++year) (srTitle++srYear), m))



downloadMovieInfo SiteConfig{parsersForInfo,urlForMovie} uriMovie =
  do -- putStr $ printf "downloading %s\n" uriMovie
     movie <- fmap concat $ forM parsersForInfo $ getMovieSection (urlForMovie uriMovie)
     movie' <- downCover "images" movie
     forM_ (filter (null . snd) movie') $ putStr . (' ':) . show . fst
     return $ Just movie'





hSpaceNew height =
  do label <- labelNew Nothing
     set label [widgetHeightRequest := height]
     return label

vSpaceNew width =
  do label <- labelNew Nothing
     set label [widgetWidthRequest := width]
     return label




searchPreferences parent sites choices =
  do d <- dialogNew
     set d [windowTitle := "Search Sites Setup", windowTransientFor := parent, containerBorderWidth := 6]
     -- dialog action area
     a <- dialogGetActionArea d
     cancelButton <- dialogAddButton d stockCancel ResponseReject
     okButton <- dialogAddButton d stockOk ResponseAccept
     -- dialog main (upper) area
     u <- dialogGetUpper d
     boxSetSpacing u 6
     swin <- mkWA (myScrolledWindowNew PolicyNever PolicyAutomatic) (myBoxPackStart u PackGrow 0) [widgetHeightRequest := 400]
     table <- mkWA (tableNew 0 3 False) (scrolledWindowAddWithViewport swin) [containerBorderWidth := 6]
     widgetShowAll swin
     ws <- forM [minBound::Info ..] $ \i ->
       do let r = fromEnum i
          label <- mkWA (labelNewWithMnemonic ('_' : show i)) (myTableAttach table 0 1 r (r+1) [Fill] [Fill] 0 0) [miscXalign := 0]
          widgetShow label
          combo <- mkW comboBoxNewText (myTableAttach table 1 2 r (r+1) [Fill] [Fill] 0 0)
          widgetShow combo
          labelSetMnemonicWidget label combo
          box <- mkWA (hBoxNew False 2) (myTableAttach table 2 3 r (r+1) [Fill] [Fill] 0 0) [containerBorderWidth := 2]
          widgetShow box
          leftButton <- mkWA buttonNew (myBoxPackStart box PackNatural 0)
                             [buttonImage :=> imageNewFromStock stockGoBack IconSizeButton, widgetSensitive := False]
          rightButton <- mkWA buttonNew (myBoxPackEnd box PackNatural 0)
                              [buttonImage :=> imageNewFromStock stockGoForward IconSizeButton, widgetSensitive := length sites > 1]
          let bsSiteNames = case lookup i choices of
                              Just (IPPreferred bsSiteNames) -> bsSiteNames
                              _ -> map siteName sites
          bs <- fmap (zip bsSiteNames) $ radioButtonNewWithMnemonicGroup bsSiteNames
          cursorRef <- newIORef ([],bs)
          forM_ bs $ \(_,b) ->
            do boxPackStart box b PackNatural 0
               toggleButtonSetMode b False
               on b toggled $
                 do (left,right) <- readIORef cursorRef
                    let (left',right') = break ((==b) . snd) (reverse left ++ right)
                    writeIORef cursorRef (reverse left',right')
                    widgetSetSensitive leftButton (length left' > 0)
                    widgetSetSensitive rightButton (length right' > 1)
          onClicked leftButton $
            do (p:ps,q:qs) <- readIORef cursorRef
               writeIORef cursorRef (ps,q:p:qs)
               boxReorderChild box (snd q) (length ps + 2)
               widgetSetSensitive leftButton (not (null ps))
               widgetSetSensitive rightButton True
          onClicked rightButton $
            do (ps,q1:q2:qs) <- readIORef cursorRef
               writeIORef cursorRef (q2:ps,q1:qs)
               boxReorderChild box (snd q1) (length ps + 3)
               widgetSetSensitive leftButton True
               widgetSetSensitive rightButton (not (null qs))
          mapM_ (comboBoxAppendText combo) ["First", "Last", "Combine", "Prefered"]
          on combo changed $
            do j <- comboBoxGetActive combo
               if j == 3
               then do widgetShowAll box
               else do widgetHideAll box
          comboBoxSetActive combo $ case lookup i choices of
                                      Just IPFirst -> 0
                                      Just IPLast -> 1
                                      Just IPCombine -> 2
                                      Just (IPPreferred _) -> 3
                                      Nothing -> 0
          return (i,combo,cursorRef)
     --
     --widgetShowAll u
     response <- dialogRun d
     result <- if response == ResponseAccept
               then fmap Just $ forM ws $ \(i,combo,cursorRef) ->
                      do k <- comboBoxGetActive combo
                         case k of
                           0 -> return (i,IPFirst)
                           1 -> return (i,IPLast)
                           2 -> return (i,IPCombine)
                           3 -> do (left,right) <- readIORef cursorRef
                                   return (i,IPPreferred (map fst (reverse left ++ right)))
               else return Nothing
     widgetDestroy d
     return result



mergeAL [] = []
mergeAL ((k,v):xs) = let (xs1,xs2) = partition ((==k) . fst) xs
                     in (k, v : map snd xs1) : mergeAL xs2

tieField infoPrefs (i,xs) =
  (i,y)
  where
    y = case lookup i infoPrefs of
          Just IPFirst -> snd (head xs)
          Just IPLast -> snd (last xs)
          Just IPCombine -> combineValues (map snd xs)
          Just (IPPreferred ks) -> case mapMaybe (\k -> lookup k xs) ks of
                                     x:_ -> x
                                     _ -> ""
          Nothing -> snd (head xs)

combineValues xs = join "," (nub (concat (map (split ",") xs)))






{-
viewURL parent url =
  do d <- dialogNew
     set d [windowTitle := "View URL", windowTransientFor := parent, containerBorderWidth := 6]
     a <- dialogGetActionArea d
     closeButton <- dialogAddButton d stockClose ResponseClose
     u <- dialogGetUpper d
     swin <- mkWA (myScrolledWindowNew PolicyAutomatic PolicyAutomatic) (myBoxPackStart u PackGrow 0) [widgetHeightRequest := 400]
     view <- mkW webViewNew (containerAdd swin)
     webViewLoadUri view url
     widgetShowAll u
     dialogRun d
     widgetDestroy d
-}
