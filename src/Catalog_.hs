{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Catalog where

import Graphics.UI.Gtk hiding (Release, after)
-- import Graphics.UI.Gtk.WebKit.WebView (webViewNew, webViewLoadUri)
import Text.Printf (printf)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Char (toLower)
import Data.Maybe (isJust, isNothing, fromJust, catMaybes, fromMaybe, mapMaybe)
import Data.Either (partitionEithers)
import Data.List (find, findIndex, sortBy, stripPrefix, partition, nub, (\\), sort)
import Data.List.Utils (split, join)
import qualified Data.Map as Map
import Data.Array.IArray (Array, listArray, assocs, (!), (//))
import System.FilePath (takeFileName, takeExtension, splitExtension, combine)
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.Process (runProcess)
import Text.HTML.TagSoup (parseTags)
import Text.Parsec (parse)
import Control.Exception (IOException,try)
import Data.Functor ((<$>))
import Control.Monad (forM, forM_, foldM_, when, unless)
import Data.Tree (Tree(Node,rootLabel,subForest), Forest, flatten)
import Control.Concurrent (yield, forkIO, killThread)
import Text.EditDistance (defaultEditCosts, levenshteinDistance, deletionCosts, insertionCosts, substitutionCosts, transpositionCosts, Costs(VariableCost))
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
import Util (fst3, snd3, trd3, snd4, after, trim, fromFilePath, anyM, date, findIndexM, partitionM, ifM, tracefun)
import Latex (latexMovie, latexA, latexZ)
import Search (guess, mergeMovie, getMovieSection, downCover)
import Component (mkW, mkWA, mkCol, mkSortCol, sortFunc, myBoxPackStart, myBoxPackEnd, myBoxPackStartWitMnemonic, myTableAttach, packInTableD, myScrolledWindowNew, errorMsg, radioButtonNewWithMnemonicGroup, setBaseFromBg, listStoreSwapRows, myPanedPack1, myPanedPack2)
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

defaultMv =
 Mv{ mvNumber   = 0
   , mvTitle    = ""
   , mvOriginal = ""
   , mvYear     = ""
   , mvCountry  = ""
   , mvFile     = ""
   , mvMark     = False
   , mvTitleCol = Nothing
   , mvFileCol  = Nothing
   , mvResult   = []
   , mvFound    = False
   , mvData     = []
   }

mvRefTitle Mv{mvTitleCol,mvFileCol}
  | isJust mvTitleCol = mvTitleCol
  | otherwise         = mvFileCol

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
     let close updtMovies =
           forM_ updtMovies $ \Mv{mvFileCol,mvData} ->
             case mvFileCol of
               Just k ->
                 do movie@M{info} <- listStoreGetValue model0 k
                    listStoreSetValue model0 k movie{info = info // mvData}
               Nothing ->
                 do col@Col{greatestMovieId=id} <- readIORef colRef
                    listStoreAppend model0 (emptyMovie (id+1)){info = emptyInfo // mvData}
                    writeIORef colRef col{greatestMovieId=id+1}
     --
     catalogAssistant parent prefsRef allMovies close

catalogAssistant parent prefsRef allMovies close =
  do model1 <- listStoreNew []
     sitesModel <- listStoreNew []
     assistant <- assistantNew
     set assistant [windowTitle := "Movie Catalog", windowTransientFor := parent, containerBorderWidth := 2]
     scanButton <- buttonNewFromStock stockApply
     set scanButton [buttonLabel := "Sca_n", widgetVisible := True]
     searchButton <- buttonNewFromStock stockApply
     set searchButton [buttonLabel := "_Search in Sites", widgetVisible := True]
     stopButton <- buttonNewFromStock stockStop
     set stopButton [widgetSensitive := False, widgetVisible := True]
     intPage <- fmap toWidget $ labelNew $ Just "This is an assistant to catalog movies from the filesystem.\n\
                                                \Movie information is collected from some specialized sites\n\
                                                \on the internet.\n\
                                                \By clicking the forward button, you can continue\n\
                                                \to the next section!"
     dirPage <- dirPanel allMovies assistant model1 scanButton stopButton
     bindPage <- bindPanel allMovies assistant model1
     (sitesPage, sitesModel, fieldsArrangement) <- sitesPanel prefsRef assistant
     pageIndexes <-
       forM [ (intPage,    "Introduction",                       AssistantPageIntro,  True)
            , (dirPage,    "Scan Directory",                     AssistantPageContent,False)
            , (bindPage,   "Check for Movies in Catalog",        AssistantPageContent,True)
            , (sitesPage,  "Selection of Search Sites",          AssistantPageContent,True)
            , (searchPage, "Search Movies in Specialized Sites", AssistantPageContent,True)
            ] $ \(widget,title,type_,complete) ->
         do index <- assistantAppendPage assistant widget
            assistantSetPageTitle assistant widget title
            assistantSetPageType assistant widget type_
            assistantSetPageComplete assistant widget complete
            return index

     on assistant assistantCancel $
       widgetDestroy assistant

     on assistant assistantClose $
       do close []
          widgetDestroy assistant

     on assistant assistantPrepare $ \widget ->
       if | widget == intPage    -> do myAssistantRemoveActionWidget assistant stopButton
                                       myAssistantRemoveActionWidget assistant scanButton
                                       myAssistantRemoveActionWidget assistant searchButton
          | widget == dirPage    -> do myAssistantRemoveActionWidget assistant searchButton
                                       myAssistantAddActionWidget assistant stopButton
                                       myAssistantAddActionWidget assistant scanButton
          | widget == bindPage   -> do myAssistantRemoveActionWidget assistant stopButton
                                       myAssistantRemoveActionWidget assistant scanButton
                                       myAssistantRemoveActionWidget assistant searchButton
          | widget == sitesPage  -> do myAssistantRemoveActionWidget assistant stopButton
                                       myAssistantRemoveActionWidget assistant scanButton
                                       myAssistantRemoveActionWidget assistant searchButton
          | widget == searchPage -> do myAssistantRemoveActionWidget assistant scanButton
                                       myAssistantAddActionWidget assistant stopButton
                                       myAssistantAddActionWidget assistant searchButton
          | otherwise            -> return ()
     
     widgetShowAll assistant

myAssistantRemoveActionWidget assistant widget =
  do mParent <- widgetGetParent widget
     when (isJust mParent) (assistantRemoveActionWidget assistant widget)

myAssistantAddActionWidget assistant widget =
  do mParent <- widgetGetParent widget
     when (isNothing mParent) (assistantAddActionWidget assistant widget)


movieExtensions = [("._mkv",True), ("._avi",False)]

dirPanel allMovies assistant model1 scanButton stopButton =
  do paned1 <- vPanedNew
     paned2 <- vPanedNew
     let page = toWidget paned1
     panedPack2 paned1 paned2 True False
     box1 <- mkWA (vBoxNew False 2) (myPanedPack1 paned1 True False) [containerBorderWidth := 2]
     mkWA (labelNewWithMnemonic "<b>File selection</b>") (myBoxPackStart box1 PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     box2 <- mkWA (hBoxNew False 2) (myBoxPackStart box1 PackNatural 0) [containerBorderWidth := 2]
     label <- mkWA (labelNewWithMnemonic "_Path") (myBoxPackStart box2 PackNatural 0) [miscXalign := 0]
     chooser <- mkWA (fileChooserButtonNew "Open Directory" FileChooserActionSelectFolder) (myBoxPackStart box2 PackNatural 0) [fileChooserShowHidden := True]
     fileChooserSetFilename chooser "/alt/seedbox"  -- REMOVE THIS LINE LATER ON
     labelSetMnemonicWidget label chooser
     mkW (vSpaceNew 16) (myBoxPackStart box2 PackNatural 0)
     mkWA (labelNew (Just "Extensions")) (myBoxPackStart box2 PackNatural 0) [miscXalign := 0]
     extButtons <-
       forM movieExtensions $ \(ext,active) ->
         mkWA (checkButtonNewWithMnemonic ext) (myBoxPackStart box2 PackNatural 0) [toggleButtonActive := active]
     --
     mkW (hSpaceNew 10) (myBoxPackStart box1 PackNatural 0)
     label <- mkWA (labelNewWithMnemonic "<b>Pro_gress</b>") (myBoxPackStart box1 PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     scrwin <- mkWA (myScrolledWindowNew PolicyAutomatic PolicyAutomatic) (myBoxPackStart box1 PackGrow 0) [containerBorderWidth := 2]
     progressView <- mkWA textViewNew (containerAdd scrwin) [widgetHeightRequest := 64, textViewEditable := False, textViewWrapMode := WrapWordChar]
     labelSetMnemonicWidget label progressView
     setBaseFromBg progressView
     buf <- textViewGetBuffer progressView
     insert <- textBufferGetInsert buf
     --
     box3 <- mkWA (vBoxNew False 2) (myPanedPack1 paned2 True False) [containerBorderWidth := 2]
     mkW (hSpaceNew 10) (myBoxPackStart box3 PackNatural 0)
     label <- mkWA (labelNewWithMnemonic "<b>Movie File _List</b>") (myBoxPackStart box3 PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     swin <- mkWA (myScrolledWindowNew PolicyAutomatic PolicyAutomatic) (myBoxPackStart box3 PackGrow 0) [widgetWidthRequest := 400, widgetHeightRequest := 128]
     smodel1 <- treeModelSortNewWithModel model1
     view1 <- mkWA (treeViewNewWithModel smodel1) (containerAdd swin) [treeViewHeadersVisible := True]
     labelSetMnemonicWidget label view1
     mkSortCol view1 smodel1 model1 (Just (1,mvNumber))                "#"              cellRendererTextNew [] $ \x -> [cellText := show (mvNumber x)]
     mkSortCol view1 smodel1 model1 (Just (2,mvTitle))                 "Title"          cellRendererTextNew [] $ \x -> [cellText := mvTitle x]
     mkSortCol view1 smodel1 model1 (Just (3,mvYear))                  "Year"           cellRendererTextNew [] $ \x -> [cellText := mvYear x]
     mkSortCol view1 smodel1 model1 (Just (4,mvOriginal))              "Original Title" cellRendererTextNew [] $ \x -> [cellText := mvOriginal x]
     mkSortCol view1 smodel1 model1 (Just (5,(takeFileName . mvFile))) "File Name"      cellRendererTextNew [] $ \x -> [cellText := takeFileName (mvFile x)]
     --
     label <- mkWA (labelNewWithMnemonic "File Pa_th") (myBoxPackStart box3 PackNatural 0) [miscXalign := 0]
     filePathEntry <- mkWA entryNew (myBoxPackStart box3 PackNatural 0) [entryEditable := False]
     labelSetMnemonicWidget label filePathEntry
     --
     box4 <- mkW (vBoxNew False 2) (myPanedPack2 paned2 True False)
     mkW (hSpaceNew 10) (myBoxPackStart box4 PackNatural 0)
     label <- mkWA (labelNewWithMnemonic "<b>Faile_d to Parse File List</b>") (myBoxPackStart box4 PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     swin <- mkWA (myScrolledWindowNew PolicyAutomatic PolicyAutomatic) (myBoxPackStart box4 PackGrow 0) [widgetHeightRequest := 48]
     model2 <- listStoreNew []
     view2 <- mkWA (treeViewNewWithModel model2) (containerAdd swin) [treeViewHeadersVisible := False]
     labelSetMnemonicWidget label view2
     mkCol view2 model2 "File Path" cellRendererTextNew [] $ \x -> [ cellText := x ]
     --
     let log msg =
           do textBufferInsertAtCursor buf msg
              textViewScrollMarkOnscreen progressView insert

     let stopped completed =
           do treeViewSetCursor view1 [0] Nothing
              n <- listStoreGetSize model1
              assistantSetPageComplete assistant page (n > 0)
              widgetSetSensitive chooser True
              widgetSetSensitive scanButton True
              widgetSetSensitive stopButton False
              log (if completed then "\nCompleted." else "\nAborted.")

     on scanButton buttonActivated $
       do log "\nSTART SCANNING"
          maybeFilePath <- fileChooserGetFilename chooser
          case maybeFilePath of
            Nothing ->
              log "\nNo file path was given"
            Just filePath ->
              do widgetSetSensitive chooser False
                 widgetSetSensitive scanButton False
                 widgetSetSensitive stopButton True
                 assistantSetPageComplete assistant page False
                 exts <- forM extButtons $ \b ->
                           ifM (toggleButtonGetActive b)
                               (Just . filter (/='_') <$> buttonGetLabel b)
                               (return Nothing)
                 listStoreClear model1
                 listStoreClear model2
                 treeViewSetCursor view1 [0] Nothing
                 treeViewSetCursor view2 [0] Nothing
                 childThread <- forkIO $
                   do fs <- searchDirs (\x -> postGUISync (log x)) (catMaybes exts) [filePath]
                      postGUIAsync $
                        do let (xs1,xs2) = partitionEithers (map parseFilePath fs)
                           forM_ (zip [1..] xs2) $ \(i,(title,original,year,file)) ->
                             let mv = Mv{ mvNumber   = i
                                        , mvTitle    = title
                                        , mvOriginal = original
                                        , mvYear     = year
                                        , mvCountry  = ""
                                        , mvFile     = file
                                        , mvMark     = True
                                        , mvTitleCol = Nothing
                                        , mvFileCol  = Nothing
                                        , mvResult   = []
                                        , mvFound    = False
                                        , mvData     = []
                                        }
                                 mv' = mv{ mvTitleCol = findTitleInCollection allMovies mv
                                         , mvFileCol  = findFileInCollection allMovies mv
                                         }
                             in listStoreAppend model1 mv'
                           forM_ xs1 $ listStoreAppend model2
                           stopped True
                 on stopButton buttonActivated $
                   do log ("\n======" ++ filePath ++ "=======")
                      killThread childThread
                      yield
                      stopped False
                 on assistant assistantCancel $
                   do killThread childThread
                      yield
                      stopped False
                 return ()

     on view1 cursorChanged $
       do (path,_) <- treeViewGetCursor view1
          cPath <- treeModelSortConvertPathToChildPath smodel1 path
          case cPath of
            [i] -> do Mv{mvFile} <- listStoreGetValue model1 i
                      entrySetText filePathEntry mvFile
            _   -> entrySetText filePathEntry ""
     
     return page


bindPanel allMovies assistant model1 =
  do paned <- hPanedNew
     --
     box <- mkW (vBoxNew False 2) (myPanedPack1 paned True False)
     label <- mkWA (labelNewWithMnemonic "<b>_Movies Found in Disk</b>") (myBoxPackStart box PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     swin <- mkWA (myScrolledWindowNew PolicyAutomatic PolicyAutomatic) (myBoxPackStart box PackGrow 0) [widgetWidthRequest := 128, widgetHeightRequest := 128]
     smodel1 <- treeModelSortNewWithModel model1
     treeSortableSetDefaultSortFunc smodel1 $ Just $ sortFunc model1 mvNumber
     view1 <- mkWA (treeViewNewWithModel smodel1) (containerAdd swin) [treeViewHeadersVisible := True]
     labelSetMnemonicWidget label view1
     let nothing = Nothing :: Maybe (SortColumnId, t -> ())
     _      <- mkSortCol view1 smodel1 model1 nothing               "#"              cellRendererTextNew []                         $ \x -> [cellText := show (mvNumber x)]
     _      <- mkSortCol view1 smodel1 model1 (Just (5,mvTitleCol)) "Same Title"     cellRendererTextNew []                         $ \x -> [cellText := maybe "" show (mvTitleCol x)]
     _      <- mkSortCol view1 smodel1 model1 (Just (6,mvFileCol))  "Same File"      cellRendererTextNew []                         $ \x -> [cellText := maybe "" show (mvFileCol x)]
     rTitle <- mkSortCol view1 smodel1 model1 (Just (3,mvTitle))    "Title"          cellRendererTextNew [cellTextEditable := True] $ \x -> [cellText := mvTitle x]
     rYear  <- mkSortCol view1 smodel1 model1 (Just (4,mvYear))     "Year"           cellRendererTextNew [cellTextEditable := True] $ \x -> [cellText := mvYear x]
     rOrig  <- mkSortCol view1 smodel1 model1 (Just (8,mvOriginal)) "Original Title" cellRendererTextNew [cellTextEditable := True] $ \x -> [cellText := mvOriginal x]
     -- _      <- mkSortCol view1 smodel1 model1 (Just (9,mvFile))     "File Name"      cellRendererTextNew []                         $ \x -> [cellText := mvFile x]
     --
     box <- mkW (vBoxNew False 2) (myPanedPack2 paned True False)
     label <- mkWA (labelNewWithMnemonic "<b>Ca_talog</b>") (myBoxPackStart box PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     swin <- mkWA (myScrolledWindowNew PolicyAutomatic PolicyAutomatic) (myBoxPackStart box PackGrow 0)  [widgetWidthRequest := 128, widgetHeightRequest := 128]
     model2 <-listStoreNew (zip3 (repeat False) (repeat E) allMovies)
     smodel2 <- treeModelSortNewWithModel model2
     treeSortableSetDefaultSortFunc smodel2 $ Just $ sortFunc model2 (ident.trd3)
     view2 <- mkWA (treeViewNewWithModel smodel2) (containerAdd swin) [treeViewHeadersVisible := True]
     labelSetMnemonicWidget label view2
     _    <- mkSortCol view2 smodel2 model2 nothing                               "Id"             cellRendererTextNew   []                              $ \(_,_,x) -> [cellText := show (ident x)]
     rSel <- mkSortCol view2 smodel2 model2 nothing                               "Match"          cellRendererToggleNew [cellToggleActivatable := True, cellToggleRadio := True] $ \(m,_,_) -> [cellToggleActive := m]
     _    <- mkSortCol view2 smodel2 model2 (Just (2,snd3))                       "Distance"       cellRendererTextNew   []                              $ \(_,d,_) -> [cellText := show d]
     _    <- mkSortCol view2 smodel2 model2 (Just (3,(!Title).info.trd3))         "Title"          cellRendererTextNew   []                              $ \(_,_,x) -> [cellText := info x ! Title]
     _    <- mkSortCol view2 smodel2 model2 (Just (4,(!Year).info.trd3))          "Year"           cellRendererTextNew   []                              $ \(_,_,x) -> [cellText := info x ! Year]
     _    <- mkSortCol view2 smodel2 model2 (Just (5,(!OriginalTitle).info.trd3)) "Original Title" cellRendererTextNew   []                              $ \(_,_,x) -> [cellText := info x ! OriginalTitle]
     --
     let cellRendererEdited updatefn path x =
           do [index1] <- treeModelSortConvertPathToChildPath smodel1 path
              mv <- listStoreGetValue model1 index1
              listStoreSetValue model1 index1 (updatefn mv x)
              size2 <- listStoreGetSize model2
              forM_ [0 .. size2-1] $ \index2 ->
                do (sel,_,x) <- listStoreGetValue model2 index2
                   listStoreSetValue model2 index2 (sel,distance mv x,x)
              treeViewSetCursor view2 [0] Nothing

     on rTitle edited $ cellRendererEdited (\mv x -> mv{mvTitle = x})
     on rOrig  edited $ cellRendererEdited (\mv x -> mv{mvOriginal = x})
     on rYear  edited $ cellRendererEdited (\mv x -> mv{mvYear = x})

     on view1 cursorChanged $
       do (path,_) <- treeViewGetCursor view1
          cPath <- treeModelSortConvertPathToChildPath smodel1 path
          case cPath of
            [i] -> do mv <- listStoreGetValue model1 i
                      let t = mvRefTitle mv
                      size <- listStoreGetSize model2
                      forM_ [0 .. size-1] $ \index ->
                        do (m, _, x) <- listStoreGetValue model2 index
                           listStoreSetValue model2 index (maybe False (== ident x) t, distance mv x, x)
                      treeViewSetCursor view2 [0] Nothing
            _   -> return ()

     on rSel cellToggled $ \strpath2 ->
       do (path1,_) <- treeViewGetCursor view1
          [index1] <- treeModelSortConvertPathToChildPath smodel1 path1
          mv <- listStoreGetValue model1 index1
          let path2 = stringToTreePath strpath2
          [index2] <- treeModelSortConvertPathToChildPath smodel2 path2
          (_,d,x) <- listStoreGetValue model2 index2
          listStoreSetValue model1 index1 mv{mvTitleCol = Just (ident x)}
          size2 <- listStoreGetSize model2
          let recurse i | i == size2 = return ()
                        | otherwise  = do (m,d,x) <- listStoreGetValue model2 i
                                          if m
                                            then listStoreSetValue model2 i (False,d,x)
                                            else recurse (i+1)
          recurse 0
          listStoreSetValue model2 index2 (True,d,x)
     --
     return (toWidget paned)



sitesPanel prefsRef assistant =
  do mainBox <- hBoxNew False 2
     --
     box <- mkW (vBoxNew False 2) (myBoxPackStart mainBox PackNatural 0)
     label <- mkWA (labelNewWithMnemonic "<b>_Sites</b>") (myBoxPackStart box PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     swin <- mkW (myScrolledWindowNew PolicyNever PolicyNever) (myBoxPackStart box PackNatural 0)
     model3 <- listStoreNew [ (True, "IMDB",         imdb)
                            , (True, "Adoro Cinema", adoroCinema)
                            , (True, "e-pipoca",     ePipoca)
                            , (True, "Interfilmes",  interFilmes)
                            , (True, "DVD World",    dvdWorld)
                            , (True, "All Center",   allCenter)
                            , (True, "Video Norte",  videoNorte)
                            ]
     view3 <- mkWA (treeViewNewWithModel model3) (containerAdd swin) [treeViewHeadersVisible := False]
     labelSetMnemonicWidget label view3
     rMark3 <- mkCol view3 model3 "Selected" cellRendererToggleNew [cellToggleActivatable := True] $ \(x,_,_) -> [ cellToggleActive := x ]
     mkCol view3 model3 "Site" cellRendererTextNew [] $ \(_,x,_) -> [ cellText := x ]
     box2 <- mkW (hBoxNew False 2) (myBoxPackStart box PackNatural 0)
     upButton <- mkW (buttonNewFromStock stockGoUp) (myBoxPackStart box2 PackNatural 0)
     downButton <- mkW (buttonNewFromStock stockGoDown) (myBoxPackStart box2 PackNatural 0)
     --
     sites <- map trd3 <$> listStoreToList model3
     prefs@Prefs{infoSearch=choices} <- readIORef prefsRef
     --
     box <- mkW (vBoxNew False 2) (myBoxPackStart mainBox PackNatural 0)
     label <- mkWA (labelNewWithMnemonic "<b>_Field Setup</b>") (myBoxPackStart box PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     swin <- mkWA (myScrolledWindowNew PolicyNever PolicyAutomatic) (myBoxPackStart box PackGrow 0) [widgetHeightRequest := 400]
     table <- mkWA (tableNew 0 2 False) (scrolledWindowAddWithViewport swin) [containerBorderWidth := 2]
     widgetShowAll swin
     let numSites = length sites
     ws <- forM [minBound::Info ..] $ \i ->
       do let r = fromEnum i
          label <- mkWA (labelNewWithMnemonic ('_' : show i)) (myTableAttach table 0 1 r (r+1) [Fill] [Fill] 0 0) [miscXalign := 0]
          widgetShow label
          combo <- mkW comboBoxNewText (myTableAttach table 1 2 r (r+1) [Fill] [Fill] 0 0)
          widgetShow combo
          labelSetMnemonicWidget label combo
          -- box <- mkWA (hBoxNew False 2) (myTableAttach table 2 3 r (r+1) [Fill] [Fill] 0 0) [containerBorderWidth := 2]
          -- widgetShow box
          leftButton <- mkWA buttonNew (myTableAttach table 2 3 r (r+1) [Fill] [Fill] 0 0)
                             [buttonImage :=> imageNewFromStock stockGoBack IconSizeButton, widgetSensitive := False]
          rightButton <- mkWA buttonNew (myTableAttach table (3+numSites) (4+numSites) r (r+1) [Fill] [Fill] 0 0)
                              [buttonImage :=> imageNewFromStock stockGoForward IconSizeButton, widgetSensitive := numSites > 1]
          let bsSiteNames = case lookup i choices of
                              Just (IPPreferred bsSiteNames) -> bsSiteNames
                              _ -> map siteName sites
          bs <- forM (zip [0..] bsSiteNames) $ \(i,name) ->
            (i,r,) <$> mkWA (toggleButtonNewWithMnemonic name) (myTableAttach table (3+i) (4+i) r (r+1) [Fill] [Fill] 2 2) []
          cursorRef <- newIORef ([],bs)
          forM_ bs $ \b ->
            on b toggled $
              do (left,right@p:_) <- readIORef cursorRef
                 let (left',right') = break ((==b).trd3) (reverse left ++ right)
                 when (p /= b) $
                   do widgetModifyBg (trd3 b) StateNormal (0xFFFF, 0, 0)
                      widgetRestoreBg (trd3 p) StateNormal
                      writeIORef cursorRef (reverse left',right')
                      widgetSetSensitive leftButton (length left' > 0)
                      widgetSetSensitive rightButton (length right' > 1)
          let tableSwapChilds table (i1,j1,w1) (i2,j2,w2) =
                do containerRemove table w1
                   containerRemove table w2
                   tableAttach table w1 i2 (i2+1) j2 (j2+1) [Fill][Fill] 0 0
                   tableAttach table w2 i1 (i1+1) j1 (j1+1) [Fill][Fill] 0 0
          onClicked leftButton $
            do (p:ps,q:qs) <- readIORef cursorRef
               writeIORef cursorRef (ps,q:p:qs)
               tableSwapChilds table p q
               widgetSetSensitive leftButton (not (null ps))
               widgetSetSensitive rightButton True
          onClicked rightButton $
            do (ps,q1:q2:qs) <- readIORef cursorRef
               writeIORef cursorRef (q2:ps,q1:qs)
               tableSwapChilds table q1 q2
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
     return (toWidget mainBox,result)



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
