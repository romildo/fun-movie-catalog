{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}

module Catalog where

import Graphics.UI.Gtk hiding (Release, after)
import Text.Printf (printf)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Char (toLower)
import Data.Maybe (isJust, fromJust, catMaybes, fromMaybe)
import Data.Either (partitionEithers)
import Data.List (find, findIndex, sortBy)
import Data.Array.IArray (assocs, (!), (//))
import System.FilePath (takeExtension)
import System.FilePath (takeFileName, splitExtension, combine)
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
import Text.HTML.TagSoup (parseTags)
import Text.Parsec (parse)
import Control.Monad (forM, forM_, when, unless)
import Data.Tree (Tree(Node,rootLabel,subForest), Forest)
import Control.Concurrent (yield, forkIO, killThread)
import Text.EditDistance (defaultEditCosts, levenshteinDistance)

import Movie (Info(Title,OriginalTitle,Year,Cover,File), Movie(..), Col(..), infoBounds, emptyInfo, emptyMovie, emptyCol)
import MovieForm (movieFormGetMovie, movieFormSetMovie)
import SiteConfig (SiteConfig(..))
import EPipoca (ePipoca)
import AdoroCinema (adoroCinema)
import InterFilmes (interFilmes)
import AllCenter (allCenter)
import TagSoupParsec (TagParser(..))
import Util (fst3, snd3, trd3, snd4, after, trim, fromFilePath)
import Latex (latexMovie, latexA, latexZ)
import Search (guess, mergeMovie, getMovieSection, downCover)
import Component (mkW, mkWA, mkCol, myBoxPackStart, myBoxPackEnd, myBoxPackStartWitMnemonic, myTableAttach, packInTableD, myScrolledWindowNew, errorMsg)


data Mv =
  Mv
  { mvTitle    :: String
  , mvOriginal :: String
  , mvYear     :: String
  , mvCountry  :: String
  , mvFile     :: String
  , mvTitleNew :: Bool
  , mvFileNew  :: Bool
  , mvMark     :: Bool
  , mvNumber   :: Int
  , mvResult   :: Forest SearchResult
  , mvFound    :: Bool
  }

data SearchResult =
  SR
  { srTitle    :: String
  , srOriginal :: String
  , srYear     :: String
  , srCountry  :: String
  , srMark     :: Bool
  , srURL      :: String
  , srSite     :: SiteConfig
  }


directoryPage allMovies assistant =
  do vbox0 <- vBoxNew False 6
     set vbox0 [containerBorderWidth := 6]
     mkWA (labelNew (Just "Give information about media files for the movies.")) (myBoxPackStart vbox0 PackNatural 0) [miscXalign := 0]
     hbox <- mkW (hBoxNew False 6) (myBoxPackStart vbox0 PackGrow 0)
     vbox1 <- mkW (vBoxNew False 6) (myBoxPackStart hbox PackNatural 0)
     vbox2 <- mkW (vBoxNew False 6) (myBoxPackStart hbox PackGrow 0)
     --
     label <- mkWA (labelNewWithMnemonic "_Directory") (myBoxPackStart vbox1 PackNatural 0) [miscXalign := 0]
     chooser <- mkWA (fileChooserButtonNew "Open Directory" FileChooserActionSelectFolder) (myBoxPackStart vbox1 PackNatural 0) [fileChooserShowHidden := True]
     labelSetMnemonicWidget label chooser
     --
     mkWA (labelNewWithMnemonic "Extensions") (myBoxPackStart vbox1 PackNatural 0) [miscXalign := 0]
     extButtons <- forM ["._mkv", "._avi"] $ \ext ->
                     mkWA (checkButtonNewWithMnemonic ext) (myBoxPackStart vbox1 PackNatural 0) [toggleButtonActive := True]
     --
     scanButton <- mkWA (buttonNewWithMnemonic "_Scan Directory") (myBoxPackStart vbox1 PackNatural 0) []
     stopButton <- mkWA (buttonNewWithMnemonic "Stop Scannin_g") (myBoxPackStart vbox1 PackNatural 0) [widgetSensitive := False]
     progress <- mkWA progressBarNew (myBoxPackStart vbox1 PackNatural 0) []
     --
     unsAllButton <- mkWA (buttonNewWithMnemonic "_Unselect All Titles") (myBoxPackEnd vbox1 PackNatural 0) []
     newFilePathButton <- mkWA (buttonNewWithMnemonic "Select New Fi_les") (myBoxPackEnd vbox1 PackNatural 0) []
     newTitleButton <- mkWA (buttonNewWithMnemonic "Select _New Titles") (myBoxPackEnd vbox1 PackNatural 0) []
     allButton <- mkWA (buttonNewWithMnemonic "Select _All Titles") (myBoxPackEnd vbox1 PackNatural 0) []
     --
     label <- mkWA (labelNewWithMnemonic "Movie File List") (myBoxPackStart vbox2 PackNatural 0) [miscXalign := 0]
     swin <- mkW (myScrolledWindowNew PolicyNever PolicyAutomatic) (myBoxPackStart vbox2 PackGrow 0)
     model <- listStoreNew []
     view <- mkWA (treeViewNewWithModel model) (containerAdd swin) [treeViewHeadersVisible := True]
     labelSetMnemonicWidget label view
     mkCol view model "#" cellRendererTextNew [] $ \x -> [ cellText := show (mvNumber x) ]
     r1 <- mkCol view model "Mar_k" cellRendererToggleNew [cellToggleActivatable := True] $ \x -> [ cellToggleActive := mvMark x ]
     r2 <- mkCol view model "_Title" cellRendererTextNew [cellTextEditable := True] $ \x -> [ cellText := mvTitle x ]
     r3 <- mkCol view model "_Original Title" cellRendererTextNew [cellTextEditable := True] $ \x -> [ cellText := mvOriginal x ]
     r4 <- mkCol view model "_Year" cellRendererTextNew [cellTextEditable := True] $ \x -> [ cellText := mvYear x ]
     mkCol view model "New Title" cellRendererToggleNew [] $ \x -> [ cellToggleActive := mvTitleNew x ]
     mkCol view model "New File" cellRendererToggleNew [] $ \x -> [ cellToggleActive := mvFileNew x ]
     filePathEntry <- mkW entryNew (myBoxPackStart vbox2 PackNatural 0)
     --
     label <- mkWA (labelNewWithMnemonic "Failed To _Parse File List") (myBoxPackStart vbox2 PackNatural 0) [miscXalign := 0]
     swin <- mkW (myScrolledWindowNew PolicyNever PolicyAutomatic) (myBoxPackStart vbox2 PackGrow 0)
     model2 <- listStoreNew []
     view2 <- mkWA (treeViewNewWithModel model2) (containerAdd swin) [treeViewHeadersVisible := True]
     labelSetMnemonicWidget label view2
     mkCol view2 model2 "File Path" cellRendererTextNew [] $ \x -> [ cellText := x ]
     --
     let page = vbox0
     assistantAppendPage assistant page
     assistantSetPageType assistant page AssistantPageContent
     assistantSetPageTitle assistant page "Select Movie Media Files"
     assistantSetPageComplete assistant page False
     --
     let setPageComplete =
           do xs <- listStoreToList model
              assistantSetPageComplete assistant page $ any mvMark xs
     --
     onClicked allButton $
       do n <- listStoreGetSize model
          forM_ [0..n-1] $ \i ->
            do x <- listStoreGetValue model i
               unless (mvMark x) $
                 do listStoreSetValue model i x{mvMark=True}
                    setPageComplete
     --
     onClicked unsAllButton $
       do n <- listStoreGetSize model
          forM_ [0..n-1] $ \i ->
            do x <- listStoreGetValue model i
               when (mvMark x) $
                 do listStoreSetValue model i x{mvMark=False}
                    setPageComplete
     --
     onClicked newTitleButton $
       do n <- listStoreGetSize model
          forM_ [0..n-1] $ \i ->
            do x <- listStoreGetValue model i
               when (not (mvMark x) && mvTitleNew x) $
                 do listStoreSetValue model i x{mvMark=True}
                    setPageComplete
     --
     onClicked newFilePathButton $
       do n <- listStoreGetSize model
          forM_ [0..n-1] $ \i ->
            do x <- listStoreGetValue model i
               when (not (mvMark x) && mvFileNew x) $
                 do listStoreSetValue model i x{mvMark=True}
                    setPageComplete
     --
     on view cursorChanged $
       do ([i],_) <- treeViewGetCursor view
          x <- listStoreGetValue model i
          entrySetText filePathEntry (mvFile x)
     --
     on r1 cellToggled $ \p ->
       do let [i] = stringToTreePath p
          x@Mv{mvMark=m} <- listStoreGetValue model i
          listStoreSetValue model i x{mvMark=not m}
          setPageComplete
     --
     on r2 edited $ \[i] str ->
       do let str' = trim str
          x <- listStoreGetValue model i
          listStoreSetValue model i x{mvTitle=str',mvTitleNew=newInCol allMovies Title str'}
     --
     on r3 edited $ \[i] str ->
       do x <- listStoreGetValue model i
          listStoreSetValue model i x{mvYear=trim str}
     --
     on r4 edited $ \[i] str ->
       do x <- listStoreGetValue model i
          listStoreSetValue model i x{mvCountry=trim str}
     --
     childThreadRef <- newIORef Nothing
     --
     onClicked scanButton $
       do childThread <- forkIO $
            do maybeFilePath <- fileChooserGetFilename chooser
               when (isJust maybeFilePath) $
                 do widgetSetSensitive stopButton True
                    widgetSetSensitive scanButton False
                    progressBarSetText progress "Scanning ..."
                    exts <- forM extButtons $ \b ->
                              do active <- toggleButtonGetActive b
                                 if active
                                   then buttonGetLabel b >>= return . Just . filter (/='_')
                                   else return Nothing
                    fs <- searchDirs progress (catMaybes exts) [fromJust maybeFilePath]
                    xs <- forM fs parseFilePath
                    let (xs1,xs2) = partitionEithers xs
                    listStoreClear model2
                    forM_ xs1 $ listStoreAppend model2
                    treeViewSetCursor view2 [0] Nothing
                    listStoreClear model
                    forM_ (zip [1::Int ..] xs2) $ listStoreAppend model . \(i,(title,original,year,file)) ->
                      Mv{ mvNumber   = i
                        , mvTitle    = title
                        , mvOriginal = original
                        , mvYear     = year
                        , mvCountry  = ""
                        , mvFile     = file
                        , mvMark     = True
                        , mvTitleNew = newInCol allMovies Title title
                        , mvFileNew  = newInCol allMovies File file
                        , mvResult   = []
                        , mvFound    = False
                        }
                    treeViewSetCursor view [0] Nothing
                    setPageComplete
                    progressBarSetText progress "Finished Scanning"
                    progressBarSetFraction progress 1
                    widgetSetSensitive stopButton False
                    widgetSetSensitive scanButton True
          writeIORef childThreadRef (Just childThread)
     --
     onClicked stopButton $
       do Just childThread <- readIORef childThreadRef
          killThread childThread
          yield
          progressBarSetText progress "Stopped Scanning"
          widgetSetSensitive stopButton False
          widgetSetSensitive scanButton True
     --
     return (vbox0,model)

sitesPage assistant =
  do vbox <- vBoxNew False 6
     set vbox [containerBorderWidth := 6]
     mkWA (labelNewWithMnemonic "Choose the sites where movies will be searched.") (myBoxPackStart vbox PackNatural 0) [miscXalign := 0]
     box <- mkW (hBoxNew False 6) (myBoxPackStart vbox PackNatural 0)
     swin <- mkW (myScrolledWindowNew PolicyAutomatic PolicyAutomatic) (myBoxPackStart box PackGrow 0)
     model <- listStoreNew [(True, "Adoro Cinema", adoroCinema), (True, "e-pipoca", ePipoca), (True, "Interfilmes", interFilmes)]
     view <- mkWA (treeViewNewWithModel model) (containerAdd swin) [treeViewHeadersVisible := False]
     r <- mkCol view model "Selected" cellRendererToggleNew [cellToggleActivatable := True] $ \(x,_,_) -> [ cellToggleActive := x ]
     mkCol view model "Site" cellRendererTextNew [] $ \(_,x,_) -> [ cellText := x ]
     box1 <- mkW (vBoxNew False 6) (myBoxPackStart box PackNatural 0)
     upButton <- mkWA buttonNew (myBoxPackStart box1 PackNatural 0) [buttonImage :=> imageNewFromStock stockGoUp IconSizeButton]
     downButton <- mkWA buttonNew (myBoxPackStart box1 PackNatural 0) [buttonImage :=> imageNewFromStock stockGoDown IconSizeButton]
     --
     let page = vbox
     assistantAppendPage assistant page
     assistantSetPageType assistant page AssistantPageContent
     assistantSetPageTitle assistant page "Search Sites"
     assistantSetPageComplete assistant page True
     --
     on r cellToggled $ \p ->
       do let [i] = stringToTreePath p
          (a,b,c) <- listStoreGetValue model i
          listStoreSetValue model i (not a,b,c)
          sites <- listStoreToList model
          assistantSetPageComplete assistant page (any fst3 sites)
     --
     on view cursorChanged $
       do ([i],_) <- treeViewGetCursor view
          n <- listStoreGetSize model
          widgetSetSensitive upButton (i > 0)
          widgetSetSensitive downButton (i < n - 1)
     --
     onClicked upButton $
       do ([i],_) <- treeViewGetCursor view
          listStoreSwap model view i (i - 1)
     --
     onClicked downButton $
       do ([i],_) <- treeViewGetCursor view
          listStoreSwap model view i (i + 1)
     --
     let getSites =
           listStoreToList model >>= return . map trd3 . filter fst3
     --
     return (vbox, getSites)

downloadPage getSites model assistant =
  do vbox0 <- vBoxNew False 6
     set vbox0 [containerBorderWidth := 6]
     --
     let page = vbox0
     assistantAppendPage assistant page
     assistantSetPageType assistant page AssistantPageContent
     assistantSetPageTitle assistant page "Select Information Movie Sources"
     assistantSetPageComplete assistant page False
     --
     mkWA (labelNewWithMnemonic "Choose the source of movie information.") (myBoxPackStart vbox0 PackNatural 0) [miscXalign := 0]
     hbox <- mkW (hBoxNew False 6) (myBoxPackStart vbox0 PackGrow 0)
     vbox1 <- mkW (vBoxNew False 6) (myBoxPackStart hbox PackNatural 0)
     vbox2 <- mkW (vBoxNew False 6) (myBoxPackStart hbox PackGrow 0)
     -- movie list
     label <- mkWA (labelNewWithMnemonic "<b>_Movie List</b>") (myBoxPackStart vbox1 PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     swin <- mkW (myScrolledWindowNew PolicyNever PolicyAutomatic) (myBoxPackStart vbox1 PackGrow 0)
     view <- mkWA (treeViewNewWithModel model) (containerAdd swin) [treeViewHeadersVisible := True]
     labelSetMnemonicWidget label view
     mkCol view model "#" cellRendererTextNew [] $ \x -> [ cellText := show (mvNumber x), cellSensitive := mvMark x ]
     mkCol view model "Title" cellRendererTextNew [] $ \x -> [ cellText := mvTitle x, cellSensitive := mvMark x ]
     mkCol view model "Year" cellRendererTextNew [] $ \x -> [ cellText := mvYear x, cellSensitive := mvMark x ]
     box <- mkW (hBoxNew False 6) (myBoxPackStart vbox1 PackNatural 0)
     entry1 <- mkW entryNew (myBoxPackStartWitMnemonic box PackGrow 0 "Title")
     box <- mkW (hBoxNew False 6) (myBoxPackStart vbox1 PackNatural 0)
     entry2 <- mkW entryNew (myBoxPackStartWitMnemonic box PackGrow 0 "Original Title")
     box <- mkW (hBoxNew False 6) (myBoxPackStart vbox1 PackNatural 0)
     entry3 <- mkW entryNew (myBoxPackStartWitMnemonic box PackGrow 0 "Year")
     box <- mkW (hBoxNew False 6) (myBoxPackStart vbox1 PackNatural 0)
     entry4 <- mkW entryNew (myBoxPackStartWitMnemonic box PackGrow 0 "File")
     --
     box <- mkW (hBoxNew False 6) (myBoxPackStart vbox1 PackNatural 0)
     searchButton <- mkW (buttonNewWithMnemonic "_Search Movies") (myBoxPackStart box PackNatural 0)
     downloadButton <- mkW (buttonNewWithMnemonic "_Download Movie Data") (myBoxPackStart box PackNatural 0)
     stopButton <- mkWA (buttonNewFromStock stockStop) (myBoxPackStart box PackNatural 0) [widgetSensitive := False]
     -- progress
     actionLabel <- mkWA (labelNew (Just "<b>No Action Done</b>")) (myBoxPackStart vbox1 PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     progress <- mkWA progressBarNew (myBoxPackStart vbox1 PackNatural 0) []
     -- search result
     label <- mkWA (labelNewWithMnemonic "<b>Search _Results</b>") (myBoxPackStart vbox2 PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     model2 <- treeStoreNew []
     swin <- mkW (myScrolledWindowNew PolicyNever PolicyAutomatic) (myBoxPackStart vbox2 PackGrow 0)
     view2 <- mkWA (treeViewNewWithModel model2) (containerAdd swin) [treeViewHeadersVisible := True]
     labelSetMnemonicWidget label view2
     treeViewExpandAll view2
     --style <- widgetGetStyle view2
     let color1 = "yellow" --color1 <- styleGetBackground style StateNormal
     let color2 = "white" --color2 <- styleGetLight style StateNormal
     let bg url marked = cellBackground := if not (null url) && marked then color1 else color2
     let vis x = cellVisible := not (null x)
     mkCol view2 model2 "Title" cellRendererTextNew [] $ \x -> [ cellTextMarkup := Just (srTitle x), bg (srURL x) (srMark x) ]
     r2 <- mkCol view2 model2 "Mark" cellRendererToggleNew [cellToggleActivatable := True, cellToggleRadio := True] $ \x -> [ cellToggleActive := srMark x, vis (srURL x), cellXAlign := 0.0, bg (srURL x) (srMark x) ]
     mkCol view2 model2 "Original Title" cellRendererTextNew [] $ \x -> [ cellText := srOriginal x, bg (srURL x) (srMark x) ]
     mkCol view2 model2 "Year" cellRendererTextNew [] $ \x -> [ cellText := srYear x, bg (srURL x) (srMark x) ]
     mkCol view2 model2 "Country" cellRendererTextNew [] $ \x -> [ cellText := srCountry x, bg (srURL x) (srMark x) ]
     box <- mkW (hBoxNew False 6) (myBoxPackStart vbox2 PackNatural 0)
     urlEntry <- mkW entryNew (myBoxPackStartWitMnemonic box PackGrow 0 "URL")
     --
     let updateDetails i =
           do Mv{mvResult=xs} <- listStoreGetValue model i
              treeStoreClear model2
              treeStoreInsertForest model2 [] 0 xs
              treeViewExpandAll view2
     --
     let search =
           do movies <- listStoreToList model
              sites <- getSites
              forM_ (zip [0::Int ..] movies) $ \(i,movie) ->
                do let msg = printf "[%i/%i] %s (%s)" (i+1) (length movies) (mvTitle movie) (mvYear movie)
                   progressBarSetText progress msg
                   forM_ (zip [0::Int ..] sites) $ \(j,site@SiteConfig{siteName}) ->
                     do let msg1 = msg ++ printf " - {%i/%i} %s" (j+1) (length sites) siteName
                        progressBarSetText progress msg1
                        res <- searchSite movie site
                        let res' = case (sortLike (mvTitle movie) res) of
                                     m:ms -> m{srMark=True} : ms
                                     [] -> []
                        let x = SR
                                { srTitle    = "<b>"++siteName++"</b>"
                                , srOriginal = ""
                                , srYear     = ""
                                , srCountry  = ""
                                , srMark     = False
                                , srURL      = ""
                                , srSite     = site
                                }
                        let node = Node x (map (flip Node []) res')
                        y@Mv{mvResult=r} <- listStoreGetValue model i
                        listStoreSetValue model i y{mvFound = True, mvResult = r ++ [node]}
                        (path,_) <- treeViewGetCursor view
                        case path of
                          [p] | i == p -> updateDetails i
                          _ -> return ()
                        putStr "."
                   putStr ";"
                   progressBarSetFraction progress ((fromIntegral i + 1) / fromIntegral (length movies))
     --
     resultIORef <- newIORef Nothing
     let download =
           do movies <- listStoreToList model
              result <- forM movies $ \movie@Mv{mvNumber,mvTitle,mvYear,mvResult,mvFile} ->
                do let msg = printf "[%i/%i] %s (%s)" mvNumber (length movies) mvTitle mvYear
                   progressBarSetText progress msg
                   putStrLn msg
                   ms <- forM mvResult $ \Node{subForest} ->
                     do let xs = filter (\Node{rootLabel=SR{srMark}} -> srMark) subForest
                        forM (zip [1::Int ..] xs) $ \(j,Node{rootLabel=SR{srURL,srSite}}) ->
                          do let msg1 = msg ++ printf "{%i/%i} %s" j (length xs) (siteName srSite)
                             progressBarSetText progress msg1
                             putStrLn msg1
                             downloadMovieInfo srSite srURL
                   let m = foldl mergeMovie [(File,mvFile)] (catMaybes (concat ms))
                   progressBarSetFraction progress (fromIntegral mvNumber / fromIntegral (length movies))
                   return m
              writeIORef resultIORef $ Just $ flip map (zip [1..] result) $ \(id,xs) ->
                (emptyMovie id){info=emptyInfo//xs}
     --
     childThreadRef <- newIORef Nothing
     --
     onClicked searchButton $
       do childThread <- forkIO $
            do labelSetMarkup actionLabel "<b>Searching ...</b>"
               widgetSetSensitive searchButton False
               widgetSetSensitive downloadButton False
               widgetSetSensitive stopButton True
               search
               labelSetMarkup actionLabel "<b>Finished searching</b>"
               widgetSetSensitive searchButton False
               widgetSetSensitive downloadButton True
               widgetSetSensitive stopButton False
          writeIORef childThreadRef (Just (True,childThread))
     --
     onClicked downloadButton $
       do childThread <- forkIO $
            do labelSetMarkup actionLabel "<b>Downloading ...</b>"
               widgetSetSensitive searchButton False
               widgetSetSensitive downloadButton False
               widgetSetSensitive stopButton True
               download
               labelSetMarkup actionLabel "<b>Finished downloading</b>"
               widgetSetSensitive searchButton False
               widgetSetSensitive downloadButton False
               widgetSetSensitive stopButton False
          writeIORef childThreadRef (Just (False,childThread))
     --
     onClicked stopButton $
       do Just (isSearching,childThread) <- readIORef childThreadRef
          killThread childThread
          yield
          if isSearching
            then do labelSetMarkup actionLabel "<b>Search stopped!</b>"
                    widgetSetSensitive searchButton True
                    widgetSetSensitive downloadButton True
                    widgetSetSensitive stopButton False
            else do labelSetMarkup actionLabel "<b>Download stopped!</b>"
                    widgetSetSensitive searchButton False
                    widgetSetSensitive downloadButton True
                    widgetSetSensitive stopButton False
     --
     on r2 cellToggled $ \p ->
       do let [i,j] = stringToTreePath p
          Node{subForest} <- treeStoreGetTree model2 [i]
          treeStoreChange model2 [i,j] $ \x -> x{srMark=True}
          case findIndex (srMark . rootLabel) subForest of
            Just k -> do treeStoreChange model2 [i,k] $ \x -> x{srMark=False}
                         return ()
            Nothing -> return ()
     --
     on view cursorChanged $
       do ([i],_) <- treeViewGetCursor view
          Mv{mvTitle,mvOriginal,mvYear,mvFile,mvResult} <- listStoreGetValue model i
          entrySetText entry1 mvTitle
          entrySetText entry2 mvOriginal
          entrySetText entry3 mvYear
          entrySetText entry4 mvFile
          treeStoreClear model2
          treeStoreInsertForest model2 [] 0 mvResult
          treeViewExpandAll view2
     --
     on view2 cursorChanged $
       do (path,_) <- treeViewGetCursor view2
          SR{srURL} <- treeStoreGetValue model2 path
          entrySetText urlEntry srURL
     --
     on assistant assistantApply $
       do readIORef resultIORef >>= return . fromMaybe []
          return ()
     --
     -- on assistant assistantPrepare $ \w ->
     --   do print $ show (w == toWidget page)
     --
     return ()


{-
     --
     widgetShowAll u
     response <- dialogRun d
     after (widgetDestroy d) $
       if response == ResponseAccept
       then readIORef resultIORef >>= return . fromMaybe []
       else return []
-}



catalog parent model0 view0 movieForm ref =
  do movie <- movieFormGetMovie movieForm (emptyMovie 1)
     (path,_) <- treeViewGetCursor view0
     unless (null path) $
       listStoreSetValue model0 (head path) movie
     allMovies <- listStoreToList model0
     --
     assistant <- assistantNew
     -- assistant pages
     (_,model) <- directoryPage allMovies assistant
     (_,getSites) <- sitesPage assistant
     downloadPage getSites model assistant
     -- forM_ [page1,page2,page3] $ \(page,pType,pTitle,pComplete) ->
     --   do assistantAppendPage assistant page
     --      assistantSetPageType assistant page pType
     --      assistantSetPageTitle assistant page pTitle
     --      assistantSetPageComplete assistant page pComplete
     -- run assistant
     widgetShowAll assistant


{-
     label <- mkWA (labelNewWithMnemonic "S_ites") (myBoxPackStart u PackNatural 0) [miscXalign := 0]
     box <- mkWA (hBoxNew False 2) (myBoxPackStart u PackNatural 0) [containerBorderWidth := 0]
     swin <- mkWA (myScrolledWindowNew PolicyNever PolicyAutomatic) (myBoxPackStart box PackGrow 0) [containerBorderWidth := 2]
     model <- listStoreNew [(True, "Adoro Cinema", adoroCinema), (True, "e-pipoca", ePipoca), (True, "Interfilmes", interFilmes)]
     view <- mkWA (treeViewNewWithModel model) (containerAdd swin) [treeViewHeadersVisible := False]
     labelSetMnemonicWidget label view
     r <- mkCol view model "Selected" cellRendererToggleNew [cellToggleActivatable := True] $ \(x,_,_) -> [ cellToggleActive := x ]
     mkCol view model "Site" cellRendererTextNew [] $ \(_,x,_) -> [ cellText := x ]
     box1 <- mkWA (vBoxNew False 2) (myBoxPackStart box PackNatural 0) [containerBorderWidth := 0]
     upButton <- mkWA buttonNew (myBoxPackStart box1 PackNatural 0) [buttonImage :=> imageNewFromStock stockGoUp IconSizeButton]
     downButton <- mkWA buttonNew (myBoxPackStart box1 PackNatural 0) [buttonImage :=> imageNewFromStock stockGoDown IconSizeButton]
     --
     box <- mkWA (hBoxNew False 2) (myBoxPackStart u PackNatural 0) [containerBorderWidth := 5]
     frame <- mkWA frameNew (myBoxPackStart box PackNatural 0)
              [containerBorderWidth := 5, frameLabelXAlign := 0, frameLabel := "Search A Title"]
     boxA <- mkWA (vBoxNew False 2) (containerAdd frame) [containerBorderWidth := 5]
     titleEntry <- mkWA entryNew (myBoxPackStartWitMnemonic boxA PackGrow 0 "_Title") [entryText := info movie ! Title]
     originalTitleEntry <- mkWA entryNew (myBoxPackStartWitMnemonic boxA PackGrow 0 "_Original Title") [entryText := info movie ! OriginalTitle]
     yearEntry <- mkWA entryNew (myBoxPackStartWitMnemonic boxA PackGrow 0 "_Year") [entryText := info movie ! Year]
     go1Button <- mkWA (buttonNewWithMnemonic "_Search Title") (myBoxPackStart boxA PackNatural 0) []
     --
     frame <- mkWA frameNew (myBoxPackStart box PackNatural 0)
              [containerBorderWidth := 5, frameLabelXAlign := 0, frameLabel := "Search Titles In Directory"]
     boxB <- mkWA (vBoxNew False 2) (containerAdd frame) [containerBorderWidth := 5]
     chooser <- mkWA (fileChooserButtonNew "Open Directory" FileChooserActionSelectFolder) (myBoxPackStartWitMnemonic boxB PackGrow 0 "_Directory") [fileChooserShowHidden := True]
     box2 <- mkWA (hBoxNew False 2) (myBoxPackStartWitMnemonic boxB PackGrow 0 "Extensions") [containerBorderWidth := 0]
     check1 <- mkWA (checkButtonNewWithMnemonic "._mkv") (myBoxPackStart box2 PackNatural 0) [toggleButtonActive := True]
     check2 <- mkWA (checkButtonNewWithMnemonic "._avi") (myBoxPackStart box2 PackNatural 0) [toggleButtonActive := True]
     newCheck <- mkWA (checkButtonNewWithMnemonic "Only _New Titles") (myBoxPackStart boxB PackGrow 0) []
     go2Button <- mkWA (buttonNewWithMnemonic "Search Di_rectory") (myBoxPackStart boxB PackNatural 0) []
     --
     on r cellToggled $ \p ->
       do let [i] = stringToTreePath p
          (a,b,c) <- listStoreGetValue model i
          listStoreSetValue model i (not a,b,c)
     on view cursorChanged $
       do ([i],_) <- treeViewGetCursor view
          n <- listStoreGetSize model
          widgetSetSensitive upButton (i > 0)
          widgetSetSensitive downButton (i < n - 1)
     onClicked upButton $
       do ([i],_) <- treeViewGetCursor view
          listStoreSwap model view i (i - 1)
     onClicked downButton $
       do ([i],_) <- treeViewGetCursor view
          listStoreSwap model view i (i + 1)
     onClicked go1Button $
       do title <- entryGetText titleEntry
          originalTitle <- entryGetText originalTitleEntry
          year <- entryGetText yearEntry
          sites <- listStoreToList model >>= return . map trd3 . filter fst3
          movies' <- searchMovieTitles parent sites [(trim title, trim originalTitle, trim year,"")]
          print (length movies')
          movieFormSetMovie movieForm (head movies')
     --
     onClicked go2Button $
       do maybeFilePath <- fileChooserGetFilename chooser
          when (isJust maybeFilePath) $
            do exts <- forM [check1,check2] $ \b ->
                         do active <- toggleButtonGetActive b
                            if  active
                              then buttonGetLabel b >>= return . Just . filter (/='_')
                              else return Nothing
               fs <- searchDirs (catMaybes exts) [fromJust maybeFilePath]
               xs <- forM fs parseFilePath
               let (xs1,xs2) = partitionEithers xs
               unless (null xs1) $
                 errorMsg parent $ "Failed to parse:\n\n" ++ show xs1
               sites <- listStoreToList model >>= return . map trd3 . filter fst3
               movies <- searchMovieTitles parent sites xs2
               forM_ movies $ \m ->
                 case findIndex (\m1 -> info m ! File == info m1 ! File) allMovies of
                   Just i -> do m1 <- listStoreGetValue model0 i
                                listStoreSetValue model0 i (mergeMovie1 m m1)
                   Nothing -> do listStoreAppend model0 m
                                 return ()
     --
     widgetShowAll u
     dialogRun d
     widgetDestroy d


searchMovieTitles parent sites movies =
  do d <- dialogNew
     set d [windowTitle := "Movie Catalog", windowTransientFor := parent, containerBorderWidth := 2]
     windowSetDefaultSize d 600 400
     -- dialog action area
     a <- dialogGetActionArea d
     cancelButton <- dialogAddButton d stockCancel ResponseReject
     stopButton <- mkW (buttonNewFromStock stockStop) (myBoxPackStart a PackNatural 0)
     scanButton <- mkW (buttonNewWithMnemonic "S_can Directory") (myBoxPackStart a PackNatural 0)
     searchButton <- mkW (buttonNewWithMnemonic "Search Movies in _WEB") (myBoxPackStart a PackNatural 0)
     downloadButton <- mkW (buttonNewWithMnemonic "_Download Movie Data") (myBoxPackStart a PackNatural 0)
     okButton <- dialogAddButton d stockOk ResponseAccept
     forM_ (flip widgetSetSensitive False) [stopButton, scanButton, searchButton, downloadButton, okButton]
     -- dialog upper area
     u <- dialogGetUpper d
     boxSetSpacing u 2
     hbox <- mkWA (hBoxNew False 2) (myBoxPackStart u PackGrow 0) [containerBorderWidth := 2]
     vbox1 <- mkWA (vBoxNew False 2) (myBoxPackStart hbox PackGrow 0) [containerBorderWidth := 2]
     vbox2 <- mkWA (vBoxNew False 2) (myBoxPackStart hbox PackGrow 0) [containerBorderWidth := 2]
     -- available sites for searching
     label <- mkWA (labelNewWithMnemonic "<b>Available S_ites</b>") (myBoxPackStart u PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     box <- mkWA (hBoxNew False 2) (myBoxPackStart vbox1 PackNatural 0) [containerBorderWidth := 0]
     swin <- mkWA (myScrolledWindowNew PolicyNever PolicyAutomatic) (myBoxPackStart box PackGrow 0) [containerBorderWidth := 2]
     sitesModel <- listStoreNew [(True, "Adoro Cinema", adoroCinema), (True, "e-pipoca", ePipoca), (True, "Interfilmes", interFilmes)]
     sitesView <- mkWA (treeViewNewWithModel sitesModel) (containerAdd swin) [treeViewHeadersVisible := False]
     labelSetMnemonicWidget label sitesView
     sitesR <- mkCol sitesView sitesModel "Mark" cellRendererToggleNew [cellToggleActivatable := True] $ \(x,_,_) -> [ cellToggleActive := x ]
     mkCol sitesView sitesModel "Site" cellRendererTextNew [] $ \(_,x,_) -> [ cellText := x ]
     box1 <- mkWA (vBoxNew False 2) (myBoxPackStart box PackNatural 0) [containerBorderWidth := 0]
     upButton <- mkWA buttonNew (myBoxPackStart box1 PackNatural 0) [buttonImage :=> imageNewFromStock stockGoUp IconSizeButton]
     downButton <- mkWA buttonNew (myBoxPackStart box1 PackNatural 0) [buttonImage :=> imageNewFromStock stockGoDown IconSizeButton]
     -- directory
     mkWA (labelNewWithMarkup "<b>Directory</b>") (myBoxPackStart vbox1 PackNatural 0) [miscXalign := 0]
     box <- mkWA (hBoxNew False 2) (myBoxPackStar vbox1 PackGrow 0) [containerBorderWidth := 0]
     label <- mkWA (labelNewWithMnemonic "_Path") (myBoxPackStart box PackNatural 0) [miscXalign := 0]
     chooser <- mkWA (fileChooserButtonNew "Open Directory" FileChooserActionSelectFolder) (myBoxPackStart box PackGrow 0) [fileChooserShowHidden := True]
     labelMnemonicWidget label chooser
     box <- mkWA (hBoxNew False 2) (myBoxPackStart vbox1 PackGrow 0) [containerBorderWidth := 0]
     mkvButton <- mkWA (checkButtonNewWithMnemonic "._mkv") (myBoxPackStart box PackNatural 0) [toggleButtonActive := True]
     aviButton <- mkWA (checkButtonNewWithMnemonic "._avi") (myBoxPackStart box PackNatural 0) [toggleButtonActive := True]
     -- movie list
     label <- mkWA (labelNewWithMnemonic "<b>Movie _List</b>") (myBoxPackStart vbox1 PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     model <- listStoreNew (map (\(i,x) -> (i,x,False,[])) (zip [1::Int ..] movies))
     swin <- mkWA (myScrolledWindowNew PolicyNever PolicyAutomatic) (myBoxPackStart vbox1 PackGrow 0) [containerBorderWidth := 2]
     view <- mkWA (treeViewNewWithModel model) (containerAdd swin) [treeViewHeadersVisible := True]
     labelSetMnemonicWidget label view
     mkCol view model "No." cellRendererTextNew [] $ \(x,_,k,_) -> [ cellText := show x, cellSensitive := k ]
     r1 <- mkCol view model "Ok" cellRendererToggleNew [cellToggleActivatable := True] $ \(_,_,x,_) -> [ cellToggleActive := x ]
     mkCol view model "Title" cellRendererTextNew [] $ \(_,(x,_,_,_),k,_) -> [ cellText := x, cellSensitive := k ]
     entry1 <- mkW entryNew (myBoxPackStartWitMnemonic vbox1 PackNatural 0 "Title")
     entry2 <- mkW entryNew (myBoxPackStartWitMnemonic vbox1 PackNatural 0 "Original Title")
     entry3 <- mkW entryNew (myBoxPackStartWitMnemonic vbox1 PackNatural 0 "Year")
     entry4 <- mkW entryNew (myBoxPackStartWitMnemonic vbox1 PackNatural 0 "File")
     -- progress
     actionLabel <- mkWA (labelNew Nothing) (myBoxPackStart vbox1 PackNatural 0) [miscXalign := 0]
     progress <- mkWA progressBarNew (myBoxPackStart vbox1 PackNatural 0) []
     -- search result
     label <- mkWA (labelNewWithMnemonic "<b>Search _Results</b>") (myBoxPackStart vbox2 PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     model2 <- treeStoreNew []
     swin <- mkWA (myScrolledWindowNew PolicyNever PolicyAutomatic) (myBoxPackStart vbox2 PackGrow 0) [containerBorderWidth := 2]
     view2 <- mkWA (treeViewNewWithModel model2) (containerAdd swin) [treeViewHeadersVisible := True]
     labelSetMnemonicWidget label view2
     treeViewExpandAll view2
     --style <- widgetGetStyle view2
     let color1 = "yellow" --color1 <- styleGetBackground style StateNormal
     let color2 = "white" --color2 <- styleGetLight style StateNormal
     let bg url marked = cellBackground := if not (null url) && marked then color1 else color2
     let vis x = cellVisible := not (null x)
     mkCol view2 model2 "Title" cellRendererTextNew [] $ \((x,_,_,_,k),m,_) -> [ cellTextMarkup := Just x, bg k m ]
     r2 <- mkCol view2 model2 "Mark" cellRendererToggleNew [cellToggleActivatable := True, cellToggleRadio := True] $ \((_,_,_,_,k),m,_) -> [ cellToggleActive := m, vis k, cellXAlign := 0.0, bg k m ]
     mkCol view2 model2 "Original Title" cellRendererTextNew [] $ \((_,x,_,_,k),m,_) -> [ cellText := x, bg k m ]
     mkCol view2 model2 "Country" cellRendererTextNew [] $ \((_,_,x,_,k),m,_) -> [ cellText := x, bg k m ]
     mkCol view2 model2 "Year" cellRendererTextNew [] $ \((_,_,_,x,k),m,_) -> [ cellText := x, bg k m ]
     urlEntry <- mkW entryNew (myBoxPackStart vbox2 PackNatural 0)
     --
     on rSites cellToggled $ \p ->
       do let [i] = stringToTreePath p
          (a,b,c) <- listStoreGetValue sitesModel i
          listStoreSetValue sitesmodel i (not a,b,c)
     on sitesView cursorChanged $
       do ([i],_) <- treeViewGetCursor sitesView
          n <- listStoreGetSize model
          widgetSetSensitive upButton (i > 0)
          widgetSetSensitive downButton (i < n - 1)
     onClicked upButton $
       do ([i],_) <- treeViewGetCursor view
          listStoreSwap model view i (i - 1)
     onClicked downButton $
       do ([i],_) <- treeViewGetCursor view
          listStoreSwap model view i (i + 1)
     --
     let updateDetails i =
           do (_,_,_,xs) <- listStoreGetValue model i
              treeStoreClear model2
              treeStoreInsertForest model2 [] 0 xs
              treeViewExpandAll view2

     let search =
           do forM_ (zip [0::Int ..] movies) $ \(i,movie@(title,originalTitle,year,filePath)) ->
                do progressBarSetText progress1 $ printf "%i/%i %s (%s)" (i+1) (length movies) title year
                   forM_ (zip [0::Int ..] sites) $ \(j,site@SiteConfig{siteName}) ->
                     do progressBarSetText progress2 $ printf "%i/%i %s" (j+1) (length sites) siteName
                        res <- searchSite movie site
                        let res' = case (sortLike title res) of
                                     m:ms -> (m,True,site) : map (,False,site) ms
                                     [] -> []
                        let node = Node (("<b>"++siteName++"</b>","","","",""),False,site) (map (flip Node []) res')
                        (n,x,_,ys) <- listStoreGetValue model i
                        listStoreSetValue model i (n,x,True,ys ++ [node])
                        (path,_) <- treeViewGetCursor view
                        case path of
                          [p] | i == p -> updateDetails i
                          _ -> return ()
                        putStr "."
                        progressBarSetFraction progress2 ((fromIntegral j + 1) / fromIntegral (length sites))
                   putStr ";"
                   progressBarSetFraction progress1 ((fromIntegral i + 1) / fromIntegral (length movies))

     --
     resultIORef <- newIORef Nothing
     let download =
           do movies <- listStoreToList model
              result <- forM movies $ \(i,(title,original,year,file),mark,forest) ->
                do let msg = printf "%i/%i %s (%s)" i (length movies) title year
                   progressBarSetText progress1 msg
                   putStrLn msg
                   ms <- forM forest $ \Node{subForest} ->
                     do let xs = filter (\Node{rootLabel=(_,mark,_)} -> mark) subForest
                        forM (zip [1::Int ..] xs) $ \(j,Node{rootLabel=((_,_,_,_,url),_,site)}) ->
                          do let msg = printf "%i/%i %s" j (length xs) (siteName site)
                             progressBarSetText progress2 msg
                             putStrLn msg
                             m <- downloadMovieInfo site url
                             progressBarSetFraction progress2 (fromIntegral j / fromIntegral (length xs))
                             return m
                   let m = foldl mergeMovie [(File,file)] (catMaybes (concat ms))
                   progressBarSetFraction progress1 (fromIntegral i / fromIntegral (length movies))
                   return m
              writeIORef resultIORef $ Just $ flip map (zip [1..] result) $ \(id,xs) ->
                (emptyMovie id){info=emptyInfo//xs}
     --
     childThreadRef <- newIORef Nothing
     --
     onClicked searchButton $
       do childThread <- forkIO $
            do labelSetMarkup actionLabel "<b>Searching ...</b>"
               widgetSetSensitive searchButton False
               widgetSetSensitive downloadButton False
               widgetSetSensitive stopButton True
               widgetSetSensitive cancelButton False
               widgetSetSensitive okButton False
               search
               labelSetMarkup actionLabel "<b>Finished searching</b>"
               widgetSetSensitive searchButton False
               widgetSetSensitive downloadButton True
               widgetSetSensitive stopButton False
               widgetSetSensitive cancelButton True
               widgetSetSensitive okButton False
          writeIORef childThreadRef (Just (True,childThread))
     --
     onClicked downloadButton $
       do childThread <- forkIO $
            do labelSetMarkup actionLabel "<b>Downloading ...</b>"
               widgetSetSensitive searchButton False
               widgetSetSensitive downloadButton False
               widgetSetSensitive stopButton True
               widgetSetSensitive cancelButton False
               widgetSetSensitive okButton False
               download
               labelSetMarkup actionLabel "<b>Finished downloading</b>"
               widgetSetSensitive searchButton False
               widgetSetSensitive downloadButton False
               widgetSetSensitive stopButton False
               widgetSetSensitive cancelButton True
               widgetSetSensitive okButton True
          writeIORef childThreadRef (Just (False,childThread))
     --
     onClicked stopButton $
       do Just (isSearching,childThread) <- readIORef childThreadRef
          killThread childThread
          yield
          if isSearching
            then do labelSetMarkup actionLabel "<b>Search stopped!</b>"
                    widgetSetSensitive searchButton True
                    widgetSetSensitive downloadButton True
                    widgetSetSensitive stopButton False
                    widgetSetSensitive cancelButton True
                    widgetSetSensitive okButton False
            else do labelSetMarkup actionLabel "<b>Download stopped!</b>"
                    widgetSetSensitive searchButton False
                    widgetSetSensitive downloadButton True
                    widgetSetSensitive stopButton False
                    widgetSetSensitive cancelButton True
                    widgetSetSensitive okButton True
     --
     on r1 cellToggled $ \p ->
       do let [i] = stringToTreePath p
          (n,x,mark,ys) <- listStoreGetValue model i
          listStoreSetValue model i (n,x,not mark,ys)
     --
     on r2 cellToggled $ \p ->
       do let [i,j] = stringToTreePath p
          Node{subForest} <- treeStoreGetTree model2 [i]
          treeStoreChange model2 [i,j] (\(x,_,s) -> (x,True,s))
          case findIndex (snd3 . rootLabel) subForest of
            Just k -> do treeStoreChange model2 [i,k] (\(x,_,s) -> (x,False,s))
                         return ()
            Nothing -> return ()
     --
     on view cursorChanged $
       do ([i],_) <- treeViewGetCursor view
          (_,(title,originalTitle,year,file),_,xs) <- listStoreGetValue model i
          entrySetText entry1 title
          entrySetText entry2 originalTitle
          entrySetText entry3 year
          entrySetText entry4 file
          treeStoreClear model2
          treeStoreInsertForest model2 [] 0 xs
          treeViewExpandAll view2
     --
     on view2 cursorChanged $
       do (path,_) <- treeViewGetCursor view2
          ((_,_,_,_,url),_,_) <- treeStoreGetValue model2 path
          entrySetText urlEntry url
     --
     widgetShowAll u
     response <- dialogRun d
     after (widgetDestroy d) $
       if response == ResponseAccept
       then readIORef resultIORef >>= return . fromMaybe []
       else return []






mergeMovie1 orig@M{info=xs1} new@M{info=xs2} =
  orig{info = emptyInfo // mergeMovie (assocs xs1) (assocs xs2)}

-}


listStoreSwap model view i j =
  do x <- listStoreGetValue model i
     y <- listStoreGetValue model j
     listStoreSetValue model i y
     listStoreSetValue model j x
     treeViewSetCursor view [j] Nothing



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
  do let (fileName,fileExt) = splitExtension (takeFileName filePath)
     case guess (fromFilePath fileName) of
       Nothing -> return $ Left filePath
       Just (title,year) -> return $ Right (title,"",year,filePath)


newInCol movies field value =
  not (isJust (find (\movie -> info movie ! field == value) movies))


searchSite Mv{mvTitle=title} site@SiteConfig{webSearch,parserForSearch} =
  do src <- webSearch (trim title)
     case src of
       Nothing -> return []
       Just doc ->
         do writeFile ("search.tags.html") doc
            let tags = parseTags doc
            writeFile "search.tags.txt" (unlines (map show tags))
            case parse parserForSearch "tagsoup" tags of
              Left err -> return []
              Right xs ->
                return $ flip map xs $ \(srTitle,srOriginal,srYear,srCountry,srURL) ->
                  SR{srTitle,srOriginal,srYear,srCountry,srURL,srMark=False,srSite=site}


sortLike x =
  map snd . sortBy (\(a,_) (b,_) -> compare a b) . map (\m@SR{srTitle} -> (levenshteinDistance defaultEditCosts x srTitle, m))




downloadMovieInfo SiteConfig{parsersForInfo,urlForMovie} uriMovie =
  do -- putStr $ printf "downloading %s\n" uriMovie
     movie <- fmap concat $ forM parsersForInfo $ getMovieSection (urlForMovie uriMovie)
     movie' <- downCover "images" movie
     putStr "Empty: "
     forM_ (filter (null . snd) movie') $ putStr . show . fst
     putStrLn ""
     return $ Just movie'


