{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiWayIf #-}

module Catalog where

import System.Glib.GObject (objectRef)
import Graphics.UI.Gtk hiding (Release, after)
-- import Graphics.UI.Gtk.WebKit.WebView (webViewNew, webViewLoadUri)
import Text.Printf (printf)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Char (toLower)
import Data.Maybe (isJust, isNothing, fromJust, catMaybes, fromMaybe, mapMaybe)
import Data.Either (partitionEithers)
import Data.List (intercalate, find, findIndex, sortBy, stripPrefix, partition, nub, (\\))
import Data.List.Split (wordsBy)
import Data.List.Utils (split, join)
import qualified Data.Map as Map
import Data.Array.IArray (Array, listArray, assocs, (!), (//))
import System.FilePath (takeFileName, takeExtension, splitExtension, combine)
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.Process (runProcess)
import Text.HTML.TagSoup (parseTags)
import Text.Parsec (parse)
import Control.Exception (IOException, try)
import Control.Applicative ((<$>))
import Control.Monad (filterM, forM, forM_, foldM, foldM_, when, unless)
import Data.Tree (Tree(Node,rootLabel,subForest), Forest, flatten)
import Control.Concurrent (yield, forkIO, killThread)
import Text.EditDistance (defaultEditCosts, levenshteinDistance)
import Debug.Trace (trace)
import qualified Data.ConfigFile as CF

import Movie (Info(Title,OriginalTitle,Year,Cover,File,InclusionDate,Genres,Rating,Cast,Synopsis,Awards,Curiosities,Notes,Country,Release,Certification), Movie(..), Col(..), infoBounds, infoMap, emptyInfo, emptyMovie, emptyCol)
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
import Util (fst3, snd3, trd3, snd4, after, trim, fromFilePath, anyM, date, findIndexM, partitionM, ifM)
import Latex (latexMovie, latexA, latexZ)
import Search (guess, mergeMovie, getMovieSection, downCover)
import Component (mkW, mkWA, mkCol, myBoxPackStart, myBoxPackEnd, myBoxPackStartWitMnemonic, myTableAttach, packInTableD, myScrolledWindowNew, myNotebookAppendPage, errorMsg, radioButtonNewWithMnemonicGroup, restoreBase, setBaseFromBg, listStoreSwapRows, getText, setText)
import Prefs


data Mv =
  Mv
  { mvTitle          :: String
  , mvOriginal       :: String
  , mvYear           :: String
  , mvCountry        :: String
  , mvFile           :: String
  , mvTitleCol       :: Maybe Int
  , mvFileCol        :: Maybe Int
  , mvMark           :: Bool
  , mvNumber         :: Int
  , mvResult         :: Forest SearchResult
  , mvFound          :: Bool
  , mvInfo           :: [(Info,[(Bool,(String,String))])]
  , mvShowWidgets    :: IO ()
  , mvGetFromWidgets :: IO [(Info,String)]
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
     paned <- mkW hPanedNew (myBoxPackStart u PackGrow 0)
     vbox1 <- mkW (vBoxNew False 3) (panedAdd1 paned)
     ntbk <- mkWA notebookNew (panedAdd2 paned) [containerBorderWidth := 2]
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
     rMark1 <- mkCol view1 model1 "S" cellRendererToggleNew [cellToggleActivatable := True] $ \x -> [ cellToggleActive := mvMark x ]
     mkCol view1 model1 "T" cellRendererToggleNew [] $ \x -> [ cellToggleActive := isNothing (mvTitleCol x) ]
     mkCol view1 model1 "F" cellRendererToggleNew [] $ \x -> [ cellToggleActive := isNothing (mvFileCol x) ]
     -- mkCol view1 model1 "D" cellRendererToggleNew [] $ \x -> [ cellToggleActive := isJust (mvRetrieve x) ]
     rTitle <- mkCol view1 model1 "_Title" cellRendererTextNew [cellTextEditable := True] $ \x -> [ cellText := mvTitle x ]
     rYear <- mkCol view1 model1 "_Year" cellRendererTextNew [cellTextEditable := True] $ \x -> [ cellText := mvYear x ]
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
     label <- mkWA (labelNewWithMnemonic "<b>Failed to _Parse File List</b>") (myBoxPackStart vbox1 PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     swin <- mkW (myScrolledWindowNew PolicyAutomatic PolicyAutomatic) (myBoxPackStart vbox1 PackNatural 0)
     model2 <- listStoreNew []
     view2 <- mkWA (treeViewNewWithModel model2) (containerAdd swin) [treeViewHeadersVisible := False]
     labelSetMnemonicWidget label view2
     mkCol view2 model2 "File Path" cellRendererTextNew [] $ \x -> [ cellText := x ]
     --
     -- SITES PANEL
     --
     mkW (hSpaceNew 6) (myBoxPackStart vbox1 PackNatural 0)
     label <- mkWA (labelNewWithMnemonic "<b>_Sites for Searching</b>") (myBoxPackStart vbox1 PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     box1 <- mkW (hBoxNew False 6) (myBoxPackStart vbox1 PackNatural 0)
     swin <- mkW (myScrolledWindowNew PolicyNever PolicyNever) (myBoxPackStart box1 PackGrow 0)
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
     vbox2 <- mkW (vBoxNew False 3) (myNotebookAppendPage ntbk "_Search Results")
     -- label <- mkWA (labelNewWithMnemonic "<b>Search _Results</b>") (myBoxPackStart vbox2 PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
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
     -- COLLECTED DATA PANEL
     --
     (setupCollectedData,clearCollectedData) <- showMovie (myNotebookAppendPage ntbk "_Collected data")
     --
     -- PROGRESS PANEL
     --
     mkW (hSpaceNew 6) (myBoxPackStart vbox1 PackNatural 0)
     label <- mkWA (labelNewWithMnemonic "<b>_Progress</b>") (myBoxPackStart vbox1 PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     scrwin <- mkWA (myScrolledWindowNew PolicyAutomatic PolicyAlways) (myBoxPackStart vbox1 PackNatural 0) [containerBorderWidth := 2]
     view <- mkWA textViewNew (containerAdd scrwin) [widgetHeightRequest := 96, textViewEditable := False, textViewWrapMode := WrapWordChar]
     labelSetMnemonicWidget label view
     setBaseFromBg view
     buf <- textViewGetBuffer view
     insert <- textBufferGetInsert buf
     --
     -- SIGNAL HANDLERS
     --
     let log msg =
           do textBufferInsertAtCursor buf msg
              textViewScrollMarkOnscreen view insert
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
          x@Mv{mvFile,mvResult,mvShowWidgets} <- listStoreGetValue model1 i
          entrySetText filePathEntry mvFile
          updateSearchResults mvResult
          mvShowWidgets
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
               when (isJust maybeFilePath) $
                 do widgetSetSensitive stopButton True
                    widgetSetSensitive cancelButton False
                    widgetSetSensitive scanButton False
                    widgetSetSensitive searchButton False
                    widgetSetSensitive downloadButton False
                    widgetSetSensitive okButton False
                    textBufferSetText buf ""
                    log "Started scanning"
                    exts <- forM extButtons $ \b ->
                              do active <- toggleButtonGetActive b
                                 if active
                                   then buttonGetLabel b >>= return . Just . filter (/='_')
                                   else return Nothing
                    fs <- searchDirs log (catMaybes exts) [fromJust maybeFilePath]
                    let xs = map parseFilePath fs
                    let (xs1,xs2) = partitionEithers xs
                    listStoreClear model2
                    forM_ xs1 $ listStoreAppend model2
                    treeViewSetCursor view2 [0] Nothing
                    listStoreClear model1
                    forM_ (zip [1::Int ..] xs2) $ listStoreAppend model1 . \(i,(title,original,year,file)) ->
                      Mv{ mvNumber         = i
                        , mvTitle          = title
                        , mvOriginal       = original
                        , mvYear           = year
                        , mvCountry        = ""
                        , mvFile           = file
                        , mvMark           = True
                        , mvTitleCol       = findInCol allMovies Title title
                        , mvFileCol        = findInCol allMovies File file
                        , mvResult         = []
                        , mvFound          = False
                        , mvInfo           = []
                        , mvShowWidgets    = clearCollectedData
                        , mvGetFromWidgets = return []
                        }
                    treeViewSetCursor view1 [0] Nothing
                    log "\nFinished Scanning"
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
            Nothing -> return ()
     --
     let search =
           do movies <- fmap (filter (mvMark . snd) . zip [0::Int ..]) $ listStoreToList model1
              sites <- getSites
              forM_ (zip [1::Int ..] movies) $ \(count,(i,movie)) ->
                do let msg1 = printf "[%i:%i] %s (%s)" count (length movies) (mvTitle movie) (mvYear movie)
                   log ("\n" ++ msg1)
                   forM_ (zip [1::Int ..] sites) $ \(j,site@SiteConfig{siteName}) ->
                     do let msg2 = printf "[%i:%i] %s" j (length sites) siteName
                        log ("\n\t" ++ msg2)
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
     --
     let download1 n counter i =
           do Prefs{infoSearch} <- readIORef prefsRef
              movie@Mv{mvMark,mvNumber,mvTitle,mvYear,mvResult,mvFile,mvFileCol} <- listStoreGetValue model1 i
              if mvMark
              then do movieInCollection <-
                        case mvFileCol of
                          Just k ->
                            do M{info} <- listStoreGetValue model0 k
                               return $ filter (not . null . snd) (assocs info)
                          Nothing ->
                            return []
                      let msg1 = printf "[%i:%i] %s (%s)" counter n mvTitle mvYear
                      log ("\n" ++  msg1)
                      when (null mvResult) $ putStrLn msg1
                      ms0 <-
                        forM mvResult $ \Node{subForest=resultsFromSite} ->
                          do let xs = filter (\Node{rootLabel=SR{srMark}} -> srMark) resultsFromSite
                             forM xs $ \Node{rootLabel=SR{srTitle,srYear,srURL,srSite}} ->
                               do let msg2 = printf "[%s] %s (%s)" (siteName srSite) srTitle srYear
                                  log ("\n\t" ++ msg2)
                                  putStrLn (msg1 ++ " - " ++ msg2)
                                  ys <- downloadMovieInfo srSite srURL
                                  return $ map (\(i,x) -> (i,(siteName srSite,x))) ys
                      let ms = mergeAL (concat (concat ms0))
                      now <- fmap (\(year,month,day) -> printf "%i/%i/%i" year month day) date
                      let m = [(File,mvFile),(InclusionDate,now)] ++ map (tieField infoSearch) ms
                      replace <- toggleButtonGetActive replButton
                      let ms' = organizeFields infoSearch (not replace) movieInCollection ms
                      (showWidgets,getFromWidgets) <- setupCollectedData ms'
                      listStoreSetValue model1 i movie{mvInfo=ms',mvShowWidgets=showWidgets,mvGetFromWidgets=getFromWidgets}
                      --
                      (path,_) <- treeViewGetCursor view1
                      case path of
                        [k] | i == k -> showWidgets
                        _ -> return ()
                      --
                      return (counter + 1)
              else return counter
     --
     let download =
           do movies <- listStoreToList model1
              n <- listStoreGetSize model1
              foldM_ (download1 (length (filter mvMark movies))) (1::Int) [0 .. n-1]
     --
     onClicked searchButton $
       do childThread <- forkIO $
            do textBufferSetText buf ""
               log "Started searching"
               widgetSetSensitive stopButton True
               widgetSetSensitive cancelButton False
               widgetSetSensitive scanButton False
               widgetSetSensitive searchButton False
               widgetSetSensitive downloadButton False
               widgetSetSensitive okButton False
               search
               log "\nFinished searching"
               widgetSetSensitive stopButton False
               widgetSetSensitive cancelButton True
               buttonsSetSensitive
          writeIORef childThreadRef (Just (ASearch,childThread))
     --
     onClicked downloadButton $
       do childThread <- forkIO $
            do textBufferSetText buf ""
               log "Started downloading"
               widgetSetSensitive stopButton True
               widgetSetSensitive cancelButton False
               widgetSetSensitive scanButton False
               widgetSetSensitive searchButton False
               widgetSetSensitive downloadButton False
               widgetSetSensitive okButton False
               download
               log "\nFinished downloading"
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
          log $
            case action of
              AScan     -> "\nStopped Scanning"
              ASearch   -> "\nStopped Searching"
              ADownload -> "\nStopped Downloading"
     --
     widgetShowAll u
     response <- dialogRun d
     when (response == ResponseAccept) $
       do n <- listStoreGetSize model1
          forM_ [0::Int .. n-1] $ \i ->
            do movie@Mv{mvMark,mvFileCol,mvGetFromWidgets} <- listStoreGetValue model1 i
               when mvMark $
                 do mvData <- mvGetFromWidgets
                    case mvFileCol of
                      Just k ->
                        do movie0@M{info=info0} <- listStoreGetValue model0 k
                           listStoreSetValue model0 k movie0{info = info0 // mvData}
                      Nothing ->
                        do col@Col{greatestMovieId=id} <- readIORef colRef
                           listStoreAppend model0 (emptyMovie (id+1)){info = emptyInfo // mvData}
                           writeIORef colRef col{greatestMovieId=id+1}
     widgetDestroy d



searchDirs log exts filePaths =
  go filePaths
  where
    go [] = return []
    go filePaths =
      do (dirs,rest) <- partitionM doesDirectoryExist filePaths
         (files,rest) <- partitionM doesFileExist rest
         let fs1 = filter (flip elem exts . map toLower . takeExtension) files
         log (" " ++ replicate (length fs1) '.')
         forM_ rest $ \f -> log ("\n! " ++ f)
         fs2 <-
           fmap concat $ forM dirs $ \dir ->
             do log ("\n" ++ dir)
                tried <- try (getDirectoryContents dir)
                case tried of
                  Left ex -> log ("\nEXCEPTION: " ++ show (ex :: IOException)) >> return []
                  Right fs -> go $ map (combine dir) (filter (\f -> f /= "." && f /= "..") fs)
         return $ fs1 ++ fs2


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


downloadMovieInfo :: SiteConfig -> String -> IO [(Info, String)]
downloadMovieInfo SiteConfig{parsersForInfo,urlForMovie} uriMovie =
  do movie <- concat <$> forM parsersForInfo (getMovieSection (urlForMovie uriMovie))
     movie' <- downCover "images" movie
     -- forM_ (filter (null . snd) movie') $ putStr . (' ':) . show . fst
     return movie'






needArea = [ Cast, Synopsis, Awards, Curiosities, Notes ]

showMovie pack =
  do scr <- mkWA (myScrolledWindowNew PolicyAutomatic PolicyAutomatic) pack [containerBorderWidth := 2]
     hadj <- scrolledWindowGetHAdjustment scr
     vadj <- scrolledWindowGetVAdjustment scr
     viewport <- viewportNew hadj vadj
     containerAdd scr viewport
     --
     let clearMv =
           do children <- containerGetChildren viewport
              forM_ children $ containerRemove viewport

     let mkFieldWidgets table (r,mvWidgets) (i,xs) =
           do align <- mkW (alignmentNew 0 0 0 0) (myTableAttach table 0 1 r (r+2) [Fill] [Fill] 0 0)
              label <- mkWA (labelNewWithMnemonic ('_':show i)) (containerAdd align) []
              ws <- forM (zip [0..] xs) $ \(k,(selected,(siteName,x))) ->
                      do align <- mkW (alignmentNew 0 0 0 0) (myTableAttach table 1 2 (r+k) (r+k+1) [Fill] [Fill] 0 0)
                         b <- mkWA (checkButtonNewWithLabel siteName) (containerAdd align) [toggleButtonActive := selected]
                         let pack = myTableAttach table 2 3 (r+k) (r+k+1) [Fill] [Fill] 0 0
                         w <-
                           if | i == Cover ->
                                toWidget <$> mkWA (imageNewFromFile x) pack [widgetTooltipText := Just x]
                              | elem i needArea ->
                                do scr <- mkWA (myScrolledWindowNew PolicyAutomatic PolicyAutomatic) pack [containerBorderWidth := 2]
                                   w <- mkWA textViewNew (containerAdd scr) [widgetHeightRequest := 128, textViewEditable := False, textViewWrapMode := WrapWord]
                                   buf <- textViewGetBuffer w
                                   textBufferSetText buf x
                                   return (toWidget w)
                              | otherwise ->
                                toWidget <$> mkWA entryNew pack [entryWidthChars := 70, entryEditable := False, entryText := x ]
                         unless selected $ setBaseFromBg w
                         on b toggled $
                           do active <- toggleButtonGetActive b
                              (if active then restoreBase else setBaseFromBg) w
                         return (b,x)
              --
              let r' = r + max 1 (length ws)
              mkW hSeparatorNew (myTableAttach table 0 3 r' (r'+1) [Fill] [Fill] 0 0)
              widgetShowAll table
              return (r'+1, (i,ws):mvWidgets)

     let showMv table =
           do clearMv
              containerAdd viewport table

     let getMv mvWidgets =
           forM mvWidgets $ \(i,widgets) ->
             fmap (mergeMvInfo i . catMaybes) $ forM widgets $ \(b,x) ->
                                                  ifM (toggleButtonGetActive b)
                                                      (return (Just x))
                                                      (return Nothing)

     -- let showMv _ = return () :: IO ()
     -- let getMv _ = return [] :: IO [(Info,String)]

     let setupMv mvData =
           do table <- tableNew 0 3 False
              set table [containerBorderWidth := 2]
              (_,mvWidgets) <- foldM (mkFieldWidgets table) (0,[]) mvData
              return (showMv table, getMv mvWidgets) :: IO (IO (), IO [(Info,String)])

     return (setupMv,clearMv)


mergeMvInfo :: Info -> [String] -> (Info,String)
mergeMvInfo i xs = (i, f i xs)
  where f Cast = sep '\n'
        f Country = sep ','
        f Rating = sep '\n'
        f Release = sep ','
        f Certification = sep ','
        f _ = concat

sep :: Char -> [String] -> String
sep s = intercalate [s] . nub . (map trim) . concat . (map (wordsBy (==s)))










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


mergeAL :: Eq a => [(a,b)] -> [(a,[b])]
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


organizeFields
  :: [(Info, InfoPref)]
     -> Bool
     -> [(Info, String)]
     -> [(Info, [(String, String)])]
     -> [(Info, [(Bool, (String, String))])]

organizeFields _ _ prior [] = map (\(i,x) -> (i,[(True,("prior",x))])) prior
organizeFields infoPrefs replace prior ((i,xs):fields) =
  case break ((i==) . fst) prior of
    (prior1,(_,p):prior2) -> (i,f (i,xs) ("prior",p)) : organizeFields infoPrefs replace (prior1++prior2) fields
    _ -> (i,g (i,xs)) : organizeFields infoPrefs replace prior fields
  where
    f (i,xs) p | replace = g (i,xs) ++ [(False,p)]
               | Just IPCombine <- lookup i infoPrefs = map (True,) (xs ++ [p])
               | otherwise = map (False,) xs ++ [(True,p)]
    g (i,xs) = case lookup i infoPrefs of
                 Just IPFirst -> let x:rest = xs in (True,x) : map (False,) rest
                 Just IPLast -> let x:rest = reverse xs in reverse ((True,x) : map (False,) rest)
                 Just IPCombine -> map (True,) xs
                 Just (IPPreferred ks) -> map (\x@(s,_) -> (elem s ks,x)) xs
                 Nothing -> let x:rest = xs in (True,x) : map (False,) rest








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
