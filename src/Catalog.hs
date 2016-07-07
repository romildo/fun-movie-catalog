{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternGuards #-}

module Catalog where

import Graphics.UI.Gtk hiding (Release, after)
import Graphics.UI.Gtk.WebKit.WebView (webViewNew, webViewEditable, webViewLoadHtmlString)
import Text.Printf (printf)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Char (toLower)
import Data.Maybe (isJust, isNothing, catMaybes, fromMaybe, mapMaybe)
import Data.Either (partitionEithers)
import Data.List (find, findIndex, sortBy, partition, nub, nubBy, sort, intercalate, zip4)
import Data.List.Split (wordsBy)
import Data.List.Utils (split, join)
import Data.Array.IArray (assocs, (!), (//))
import System.FilePath (takeFileName, takeExtension, splitExtension, combine)
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents, createDirectoryIfMissing)
import System.Process (runProcess)
import Text.HTML.TagSoup (parseTags)
import Text.Parsec (parse)
import Control.Exception (IOException,try)
import Data.Functor ((<$>))
import Control.Monad (forM, forM_, foldM, when, unless)
import Data.Tree (Tree(Node,rootLabel,subForest), Forest, flatten)
import Control.Concurrent (yield, forkIO, killThread)
import Text.EditDistance (defaultEditCosts, levenshteinDistance, deletionCosts, insertionCosts, substitutionCosts, Costs(VariableCost))
import qualified Data.Text as T
-- import Debug.Trace (trace)

import Movie (Info(Title,OriginalTitle,Year,Cover,File,Location,InclusionDate,Genres,Rating,Cast,Synopsis,Awards,Curiosities,Notes,Country,Release,Certification,ParentalGuide), Movie(..), Col(..), emptyInfo, emptyMovie)
import MovieForm (movieFormGetMovie)
import CastGUI (parseCast, castToString)
import SiteConfig (SearchResult1(..), SiteConfig(..), SiteName, webSearch)
import IMDB (imdb)
import AdoroCinema (adoroCinema)
import EPipoca (ePipoca)
import Interfilmes (interFilmes)
import DVDWorld (dvdWorld)
import AllCenter (allCenter)
import VideoNorte (videoNorte)
import CapasBrasil (capasbrasil)
import Manicomio (manicomio)
import Util (fst3, snd3, trd3, fst4, snd4, trd4, fth4, trim, fromFilePath, date, partitionM, ifM)
import Search (guess, getMovieSection, downCover, downloadCover)
import Component (mkW, mkWA, mkViewCol, mkCellRenderer, mkCol, mkSortCol, sortFunc, myBoxPackStart, myBoxPackEnd, myBoxPackStartWithMnemonic, myBoxPackEndWithMnemonic, myTableAttach, myScrolledWindowNew, errorMsg, setBaseFromBg, restoreBase, listStoreSwapRows, myPanedPack1, myPanedPack2, myNotebookAppendPage, myPixbufNewFromFileAtSize)
import Prefs (Prefs(Prefs,infoSearch,browser), InfoPref(IPFirst,IPLast,IPCombine,IPSelected))


-- data Mv =
--   Mv
--   { mvNumber   :: Int
--   , mvMark     :: Bool
--   , mvSelected :: Bool
--   , mvTitle    :: String
--   , mvOriginal :: String
--   , mvYear     :: String
--   , mvCountry  :: String
--   , mvGenres   :: String
--   , mvMedia    :: String
--   , mvFile     :: String
--   , mvTitleCol :: Maybe Int
--   , mvFileCol  :: Maybe Int
--   , mvURL      :: String
--   , mvSite     :: SiteConfig
--   , mvFound    :: Bool
--   , mvData     :: [(Info,String)]
--   }

data Mv =
  Mv
  { mvTitle          :: String
  , mvOriginal       :: String
  , mvYear           :: String
  , mvCountry        :: String
  , mvFile           :: String
  , mvTitleCol       :: (Maybe Int)
  , mvFileCol        :: !(Maybe Int)
  , mvMark           :: Bool
  , mvNumber         :: Int
  , mvResult         :: Forest SearchResult
  , mvFound          :: Bool
  , mvInfo           :: [(Info,[(Bool,(String,String))])]
  , mvShowWidgets    :: Maybe (IO ())  -- TODO: try to remove the Maybe
  , mvGetFromWidgets :: IO [(Info,String)]
  , mvData           :: [(Info,String)]
  }

defaultMv =
 Mv{ mvNumber         = 0
   , mvMark           = False
   , mvTitle          = ""
   , mvOriginal       = ""
   , mvYear           = ""
   , mvCountry        = ""
   , mvFile           = ""
   , mvTitleCol       = Nothing
   , mvFileCol        = Nothing
   , mvResult         = []
   , mvFound          = False
   , mvInfo           = []
   , mvShowWidgets    = Nothing
   , mvGetFromWidgets = return []
   , mvData        = []
   }

data SearchResult =
  SR
  { srTitle    :: String
  , srOriginal :: String
  , srYear     :: String
  , srCountry  :: String
  , srGenres   :: String
  , srMedia    :: String
  , srDistance :: Distance
  , srMark     :: Bool
  , srURL      :: String
  , srSite     :: SiteConfig
  , srImage    :: Maybe Pixbuf
  , srFound    :: Bool
  }

defaultSR =
  SR
  { srTitle    = ""
  , srOriginal = ""
  , srYear     = ""
  , srCountry  = ""
  , srGenres   = ""
  , srMedia    = ""
  , srDistance = E
  , srMark     = False
  , srURL      = ""
  , srSite     = undefined
  , srImage    = Nothing
  , srFound    = False
  }


mvToSR Mv{mvTitle,mvYear,mvOriginal} =
  defaultSR
  { srTitle    = mvTitle
  , srYear     = mvYear
  , srOriginal = mvOriginal
  , srMark     = True
  }


bold x = "<b>" ++ x ++ "</b>"
italic x = "<i>" ++ x ++ "</i>"

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
           forM_ updtMovies $ \Mv{mvTitleCol,mvGetFromWidgets} ->
             do mvData <- mvGetFromWidgets
                case mvTitleCol of
                  Just index ->
                    do  movie@M{info} <- listStoreGetValue model0 index
                        listStoreSetValue model0 index movie{info = emptyInfo // mvData}
                  _ ->
                    do col@Col{greatestMovieId=id} <- readIORef colRef
                       listStoreAppend model0 (emptyMovie (id+1)){info = emptyInfo // mvData}
                       writeIORef colRef col{greatestMovieId=id+1}
     --
     catalogAssistant parent prefsRef allMovies close

catalogAssistant parent prefsRef allMovies close =
  do model1 <- listStoreNew []
     model4 <- treeStoreNew []
     assistant <- assistantNew
     set assistant [windowTitle := "Movie Catalog", windowTransientFor := parent, containerBorderWidth := 2]
     scanButton <- buttonNewFromStock stockApply
     set scanButton [buttonLabel := "Sca_n", widgetVisible := True]
     searchButton <- buttonNewFromStock stockApply
     set searchButton [buttonLabel := "_Search in Sites", widgetVisible := True]
     downloadButton <- buttonNewFromStock stockApply
     set downloadButton [buttonLabel := "_Download", widgetVisible := True]
     stopButton <- buttonNewFromStock stockStop
     set stopButton [widgetSensitive := False, widgetVisible := True]
     clearButton <- buttonNewFromStock stockClear
     set clearButton [widgetSensitive := True, widgetVisible := True]
     -- intPage <- fmap toWidget $ labelNew $ Just "This is an assistant to catalog movies from the filesystem.\n\
     --                                            \Movie information is collected from some specialized sites\n\
     --                                            \on the internet.\n\
     --                                            \By clicking the forward button, you can continue\n\
     --                                            \to the next section!"
     (dirPage, getLocation) <- dirPanel allMovies assistant model1 model4 scanButton stopButton
     (sitesPage, getSites, _) <- sitesPanel prefsRef assistant
     (searchPage, getSearchResults) <- searchPanel prefsRef assistant allMovies model1 model4 searchButton downloadButton stopButton clearButton getSites getLocation
     applyPage <- fmap toWidget $ labelNew $ Just "Do you want to apply the changes to the catalog?"
     pageIndexes <-
       forM [ -- (intPage,    "Introduction",                       AssistantPageIntro,   True)
            -- , 
              (dirPage,    "Scan Directory",                     AssistantPageContent, False)
            , (sitesPage,  "Selection of Search Sites",          AssistantPageContent, True)
            , (searchPage, "Search Movies in Specialized Sites", AssistantPageContent, False)
            , (applyPage,  "Apply Changes",                      AssistantPageConfirm, True)
            ] $ \(widget,title,type_,complete) ->
         do index <- assistantAppendPage assistant widget
            assistantSetPageTitle assistant widget title
            assistantSetPageType assistant widget type_
            assistantSetPageComplete assistant widget complete
            return index

     on assistant assistantCancel $
       widgetDestroy assistant

     on assistant assistantClose $
       do movies <- listStoreToList model1
          close (filter mvMark movies)
          widgetDestroy assistant

     on assistant assistantPrepare $ \widget ->
       if -- | widget == intPage    -> do myAssistantRemoveActionWidget assistant stopButton
          --                              myAssistantRemoveActionWidget assistant scanButton
          --                              myAssistantRemoveActionWidget assistant searchButton
          --                              myAssistantRemoveActionWidget assistant downloadButton
          --                              myAssistantRemoveActionWidget assistant clearButton

          | widget == dirPage    -> do myAssistantRemoveActionWidget assistant searchButton
                                       myAssistantRemoveActionWidget assistant downloadButton
                                       myAssistantRemoveActionWidget assistant clearButton
                                       myAssistantAddActionWidget assistant stopButton
                                       myAssistantAddActionWidget assistant scanButton

          | widget == sitesPage  -> do myAssistantRemoveActionWidget assistant stopButton
                                       myAssistantRemoveActionWidget assistant clearButton
                                       myAssistantRemoveActionWidget assistant scanButton
                                       myAssistantRemoveActionWidget assistant searchButton
                                       myAssistantRemoveActionWidget assistant downloadButton

          | widget == searchPage -> do myAssistantRemoveActionWidget assistant scanButton
                                       myAssistantAddActionWidget assistant stopButton
                                       myAssistantAddActionWidget assistant downloadButton
                                       myAssistantAddActionWidget assistant searchButton
                                       myAssistantAddActionWidget assistant clearButton

          | widget == applyPage  -> do myAssistantRemoveActionWidget assistant stopButton
                                       myAssistantRemoveActionWidget assistant scanButton
                                       myAssistantRemoveActionWidget assistant searchButton
                                       myAssistantRemoveActionWidget assistant downloadButton
                                       myAssistantRemoveActionWidget assistant clearButton

          | otherwise            -> return ()
     
     widgetShowAll assistant

myAssistantRemoveActionWidget assistant widget =
  do mParent <- widgetGetParent widget
     when (isJust mParent) (assistantRemoveActionWidget assistant widget)

myAssistantAddActionWidget assistant widget =
  do mParent <- widgetGetParent widget
     when (isNothing mParent) (assistantAddActionWidget assistant widget)


movieExtensions = [("._mkv",True), ("._avi",False)]

dirPanel allMovies assistant model1 model4 scanButton stopButton =
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
     locationEntry <- mkWA entryNew (myBoxPackEndWithMnemonic box2 PackNatural 0 "_Location") []
     --
     mkW (hSpaceNew 10) (myBoxPackStart box1 PackNatural 0)
     log <- mkLog (myBoxPackStart box1 PackGrow 0)
     --
     box3 <- mkWA (vBoxNew False 2) (myPanedPack1 paned2 True False) [containerBorderWidth := 2]
     mkW (hSpaceNew 10) (myBoxPackStart box3 PackNatural 0)
     label <- mkWA (labelNewWithMnemonic "<b>Movie File _List</b>") (myBoxPackStart box3 PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     swin <- mkWA (myScrolledWindowNew PolicyAutomatic PolicyAutomatic) (myBoxPackStart box3 PackGrow 0) [widgetWidthRequest := 400, widgetHeightRequest := 256]
     smodel1 <- treeModelSortNewWithModel model1
     view1 <- mkWA (treeViewNewWithModel smodel1) (containerAdd swin) [treeViewHeadersVisible := True]
     labelSetMnemonicWidget label view1
     mkSortCol view1 smodel1 model1 (Just (1,mvNumber))                "#"              cellRendererTextNew [] $ \x -> [cellText := show (mvNumber x)]
     mkSortCol view1 smodel1 model1 (Just (2,mvTitle))                 "Title"          cellRendererTextNew [] $ \x -> [cellText := mvTitle x]
     mkSortCol view1 smodel1 model1 (Just (3,mvYear))                  "Year"           cellRendererTextNew [] $ \x -> [cellText := mvYear x]
     mkSortCol view1 smodel1 model1 (Just (4,mvOriginal))              "Original Title" cellRendererTextNew [] $ \x -> [cellText := mvOriginal x]
     mkSortCol view1 smodel1 model1 (Just (5,(takeFileName . mvFile))) "File Name"      cellRendererTextNew [] $ \x -> [cellText := takeFileName (mvFile x)]
     --
     -- label <- mkWA (labelNewWithMnemonic "File Pa_th") (myBoxPackStart box3 PackNatural 0) [miscXalign := 0]
     -- filePathEntry <- mkWA entryNew (myBoxPackStart box3 PackNatural 0) [entryEditable := False]
     -- labelSetMnemonicWidget label filePathEntry

     
     box3a <- mkWA (hBoxNew False 2) (myBoxPackStart box3 PackNatural 0) [containerBorderWidth := 2]
     filePathEntry <- mkWA entryNew (myBoxPackStartWithMnemonic box3a PackGrow 0 "File Pa_th") [entryEditable := False]
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
     let stopped completed =
           do treeViewSetCursor view1 [0] Nothing
              maybeIterFirst <- treeModelGetIterFirst model1
              assistantSetPageComplete assistant page (isJust maybeIterFirst)
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
                 treeStoreClear model4
                 treeViewSetCursor view1 [0] Nothing
                 treeViewSetCursor view2 [0] Nothing
                 -- TODO: treeViewSetCursor view4 [0] Nothing
                 childThread <- forkIO $
                   do fs <- searchDirs (postGUISync . log) (catMaybes exts) [filePath]
                      postGUIAsync $
                        do let (xs1,xs2) = partitionEithers (map parseFilePath fs)
                           forM_ (zip [1..] xs2) $ \(i,(title,original,year,file)) ->
                             let mv = defaultMv { mvNumber   = i
                                                , mvTitle    = title
                                                , mvOriginal = original
                                                , mvYear     = year
                                                , mvFile     = file
                                                , mvMark     = True
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

     let getLocation =
           trim <$> entryGetText locationEntry

     return (page,getLocation)


sitesPanel prefsRef assistant =
  do mainBox <- hBoxNew False 2
     --
     box <- mkWA (vBoxNew False 2) (myBoxPackStart mainBox PackNatural 0) [containerBorderWidth := 2]
     label <- mkWA (labelNewWithMnemonic "<b>_Sites</b>") (myBoxPackStart box PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     swin <- mkW (myScrolledWindowNew PolicyNever PolicyNever) (myBoxPackStart box PackNatural 0)
     model3 <- listStoreNew [ (True, "e-Pipoca",     ePipoca)
                            , (True, "IMDB",         imdb)
                            , (True, "Adoro Cinema", adoroCinema)
                            , (True, "Interfilmes",  interFilmes)
                            , (True, "DVD World",    dvdWorld)
                            , (True, "All Center",   allCenter)
                            , (True, "Video Norte",  videoNorte)
                            , (True, "Capas Brasil", capasbrasil)
                            , (True, "Manicomio",    manicomio)
                            ]
     view3 <- mkWA (treeViewNewWithModel model3) (containerAdd swin) [treeViewHeadersVisible := False]
     labelSetMnemonicWidget label view3
     rMark3 <- mkCol view3 model3 "Selected" cellRendererToggleNew [cellToggleActivatable := True] $ \(x,_,_) -> [ cellToggleActive := x ]
     mkCol view3 model3 "Site" cellRendererTextNew [] $ \(_,x,_) -> [ cellText := x ]
     box2 <- mkW (hBoxNew False 2) (myBoxPackStart box PackNatural 0)
     upButton <- mkW (buttonNewFromStock stockGoUp) (myBoxPackStart box2 PackNatural 0)
     downButton <- mkW (buttonNewFromStock stockGoDown) (myBoxPackStart box2 PackNatural 0)
     box2 <- mkW (hBoxNew False 2) (myBoxPackStart box PackNatural 0)
     unsAllButton <- mkWA (buttonNewWithMnemonic "_Unselect All") (myBoxPackEnd box2 PackNatural 0) []
     allButton <- mkWA (buttonNewWithMnemonic "Select _All") (myBoxPackEnd box2 PackNatural 0) []
     loginButton <- mkWA (buttonNewWithMnemonic "_Login") (myBoxPackStart box PackNatural 0) []
     --
     sites <- map trd3 <$> listStoreToList model3
     prefs@Prefs{infoSearch=choices} <- readIORef prefsRef
     --
     box <- mkW (vBoxNew False 2) (myBoxPackStart mainBox PackGrow 0)
     label <- mkWA (labelNewWithMnemonic "<b>_Field Setup</b>") (myBoxPackStart box PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     swin <- mkWA (myScrolledWindowNew PolicyAutomatic PolicyAutomatic) (myBoxPackStart box PackGrow 0) [widgetWidthRequest := 600, widgetHeightRequest := 400]
     table <- mkWA (tableNew 0 0 False) (scrolledWindowAddWithViewport swin) [tableRowSpacing := 2, containerBorderWidth := 2]
     let numSites = length sites
     ws <- forM [minBound::Info ..] $ \i ->
       do let r = fromEnum i
          label <- mkWA (labelNew (Just (show i))) (myTableAttach table 0 1 r (r+1) [Fill] [Fill] 0 0) [miscXalign := 0]
          combo <- mkW comboBoxNewText (myTableAttach table 1 2 r (r+1) [Fill] [Fill] 0 0)
          labelSetMnemonicWidget label combo
          forM_ ["First", "Last", "Combine", "Selected"] $ (comboBoxAppendText combo . T.pack)
          comboBoxSetActive combo $ case lookup i choices of
                                      Just IPFirst        -> 0
                                      Just IPLast         -> 1
                                      Just IPCombine      -> 2
                                      Just (IPSelected _) -> 3
                                      Nothing             -> 0
          box1 <- mkWA (hBoxNew False 2) (myTableAttach table 2 3 r (r+1) [Fill] [Fill] 0 0) [containerBorderWidth := 2]
          leftButton <- mkWA buttonNew (myBoxPackStart box1 PackNatural 0)
                             [buttonImage :=> imageNewFromStock stockGoBack IconSizeButton, widgetSensitive := False]
          rightButton <- mkWA buttonNew (myBoxPackEnd box1 PackNatural 0)
                              [buttonImage :=> imageNewFromStock stockGoForward IconSizeButton, widgetSensitive := numSites > 1]
          let bsSiteNames = case lookup i choices of
                              Just (IPSelected bsSiteNames) -> bsSiteNames
                              _ -> map siteName sites
          bs <- forM bsSiteNames $ \name ->
            do frame <- mkWA frameNew (myBoxPackStart box1 PackNatural 0) [widgetName := Just "selectableframe", frameShadowType := ShadowNone]
               button <- mkWA (toggleButtonNewWithMnemonic name) (containerAdd frame) []
               return (name,frame,button)

          case lookup i choices of
            Just (IPSelected _) -> return ()
            _ -> widgetSetNoShowAll box1 True

          cursorRef <- newIORef ([],bs)

          let refreshBaseColor w status
                | status    = do -- style <- widgetGetStyle w
                                 -- color <- styleGetBase style StateSelected
                                 -- color <- styleGetBackground style StateSelected
                                 let color = Color 65000 32000 32000
                                 -- forM_ sensitiveStates $ \s -> widgetModifyBase w s color
                                 forM_ sensitiveStates $ \s -> widgetModifyBg w s color
                | otherwise = do -- forM_ sensitiveStates $ widgetRestoreBase w
                                 forM_ sensitiveStates $ widgetRestoreBg w
                where
                  sensitiveStates = [ StateNormal, StateActive, StateSelected, StatePrelight ]

          let widgetSelected w =
                do style <- widgetGetStyle w
                   forM_ [StateNormal ..] $ \state ->
                     do light <- styleGetLight style state
                        middle <- styleGetMiddle style state
                        dark <- styleGetDark style state
                        widgetModifyBase w state light

          let widgetUnSelected w =
                forM_ [StateNormal ..] $ \state ->
                  widgetRestoreBase w state

          forM_ bs $ \nb@(n,f,b) -> 
            on b toggled $
              do (left,right@((_,fr,p):_)) <- readIORef cursorRef
                 let (left',right') = break ((==b).trd3) (reverse left ++ right)
                 -- forM_ [StateNormal ..] $ \s -> widgetRestoreBg p s
                 -- forM_ [StateNormal ..] $ \s -> widgetModifyBg b s (Color 0xFFFF 0 0)
                 -- REMEMBER: gtk_widget_get_style() and gtk_style_get_font()
                 refreshBaseColor fr False -- ; refreshBaseColor p False
                 refreshBaseColor f True -- ; refreshBaseColor b True
                 frameSetShadowType fr ShadowNone ; frameSetShadowType f ShadowOut
                 writeIORef cursorRef (reverse left',right')
                 widgetSetSensitive leftButton (length left' > 0)
                 widgetSetSensitive rightButton (length right' > 1)

          onClicked leftButton $
            do (p:ps,q:qs) <- readIORef cursorRef
               writeIORef cursorRef (ps,q:p:qs)
               boxReorderChild box1 (snd3 q) (length ps + 2)
               widgetSetSensitive leftButton (not (null ps))
               widgetSetSensitive rightButton True

          onClicked rightButton $
            do (ps,q1:q2:qs) <- readIORef cursorRef
               writeIORef cursorRef (q2:ps,q1:qs)
               boxReorderChild box1 (snd3 q1) (length ps + 3)
               widgetSetSensitive leftButton True
               widgetSetSensitive rightButton (not (null qs))

          on combo changed $
            do widgetSetNoShowAll box1 False
               j <- comboBoxGetActive combo
               if j == 3
                 then widgetShowAll box1
                 else widgetHideAll box1

          return (i,combo,cursorRef)
     --
     let selectAll status =
           do n <- listStoreGetSize model3
              forM_ [0..n-1] $ \i ->
                do (a,b,c) <- listStoreGetValue model3 i
                   unless (a == status) $
                     listStoreSetValue model3 i (status,b,c)
     --
     on allButton buttonActivated $ selectAll True
     --
     on unsAllButton buttonActivated $ selectAll False
     --
     on rMark3 cellToggled $ \path ->
       do let [i] = stringToTreePath path
          (a,b,c) <- listStoreGetValue model3 i
          listStoreSetValue model3 i (not a,b,c)
          -- TODO: setWidgetSensitive searchButton ? depends on having at least one selected site
     --
     on view3 cursorChanged $
       do (path,_) <- treeViewGetCursor view3
          case path of
            [i] -> do n <- listStoreGetSize model3
                      widgetSetSensitive upButton (i > 0)
                      widgetSetSensitive downButton (i < n - 1)
                      (_,_,SiteConfig{authenticate}) <- listStoreGetValue model3 i
                      widgetSetSensitive loginButton (isJust authenticate)
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
     on loginButton buttonActivated $
       do (path,_) <- treeViewGetCursor view3
          case path of
            [i] -> do (a,b,s@SiteConfig{siteName,authenticate=Just auth}) <- listStoreGetValue model3 i
                      let (u,p) = case siteName of
                                    "CapasBrasil" -> ("eagle007", "1995.An@")
                                    _ -> ("romildo", "ana1995")
                      res <- auth u p
                      case res of
                        Left err -> return ()
                        Right cookies -> listStoreSetValue model3 i (a,b,s{cookies})
            _   -> return ()
     --
     --
     let getSites =
           map trd3 . filter fst3 <$> listStoreToList model3
     --
     let result =
           forM ws $ \(i,combo,cursorRef) ->
             do k <- comboBoxGetActive combo
                case k of
                  0 -> return (i,IPFirst)
                  1 -> return (i,IPLast)
                  2 -> return (i,IPCombine)
                  3 -> do (left,right) <- readIORef cursorRef
                          return (i,IPSelected (map fst3 (reverse left ++ right)))
     --
     return (toWidget mainBox, getSites, result)



data Distance = D Int Char Char Int -- distance, from, to, distance, text length
              | E

instance Show Distance where
  show (D x a b _) = printf "%3d %c%c" x a b
  show E           = "     E"

instance Eq Distance where
  D x _ _ _ == D y _ _ _ = x == y
  E         == E         = True
  _         == _         = False

instance Ord Distance where
  compare (D x _ _ _) (D y _ _ _) = compare x y
  compare E           E           = EQ
  compare E           _           = GT
  compare _           _           = LT

goodDistance (D x _ _ n) = x <= div n 5
goodDistance E           = False

distanceMvM Mv{mvTitle,mvOriginal,mvYear} M{info} =
  distance (mvTitle,mvOriginal,mvYear) (info!Title,info!OriginalTitle,info!Year)

distance (t1,o1,y1) (t2,o2,y2) =
  case sort [ distTT, distOO, distTO, distOT ] of
    D x a b n : _ -> case distYY of
                       D y _ _ n' -> D (x + y) a b (n+n')
                       _          -> D x a b n
    _             -> E
  where
    dist k1 k2 x1 x2
      | null x1 || null x2 = E
      | otherwise = D (editDistance x1 x2) k1 k2 (max (length x1) (length x2))

    distTT = dist 'T' 'T' t1 t2
    distTO = dist 'T' 'O' t1 o2
    distOT = dist 'O' 'T' o1 t2
    distOO = dist 'O' 'O' o1 o2
    distYY = dist 'Y' 'Y' y1 y2

editDistance text1 text2 =
  levenshteinDistance defaultEditCosts (map toLower text1) (map toLower text2)

-- editDistance text1 text2 =
--   levenshteinDistance editCosts text1 text2

editCosts =
  defaultEditCosts
    { substitutionCosts =
        VariableCost $ \(c1,c2) ->
          if toLower c1 == toLower c2 ||
             elem c1 separators && elem c2 separators ||
             any (\ds -> elem c1 ds && elem c2 ds) diacritics
          then 0
          else 1
    , deletionCosts =
        VariableCost $ \c ->
          if elem c separators
          then 0
          else 1
    , insertionCosts =
        VariableCost $ \c ->
          if elem c separators
          then 0
          else 1
    }
  where
    separators = " -_,;:()[]{}"
    diacritics = [ "aáâãäAÁÂÃÄ"
                 , "eéêẽëEÉÊẼË"
                 , "iíîĩïIÍÎĨÏ"
                 , "oóôõöOÓÔÕÖ"
                 , "uúûũüUÚÛŨÜ"
                 , "cçCÇ"
                 ]

-- editDistance :: String -> String -> Int
-- editDistance s1 s2 = snd (editDistance' Map.empty s1 s2)

-- editDistance' m s1 s2 | Just cost <- Map.lookup (s1,s2) m = (m,cost)
-- editDistance' m [] s2 = let n = length s2 in (Map.insert ([],s2) n m, n)
-- editDistance' m s1 [] = let n = length s1 in (Map.insert (s1,[]) n m, n)
-- editDistance' m s1@(x:r1) s2@(y:r2) = let (m1,c1) = editDistance' m r1 s2
--                                           (m2,c2) = editDistance' m1 s1 r2
--                                           (m3,c3) = editDistance' m2 r1 r2
--                                           c = min (c1+1) (min (c2+1) (c3+cost))
--                                       in (Map.insert (s1,s2) c m, c)
--   where
--     cost | x == y    = 0
--          | otherwise = 1


searchPanel prefsRef assistant allMovies model1 model4 searchButton downloadButton stopButton clearButton getSites getLocation =
  do paned <- hPanedNew
     let page = paned
     ntbk <- mkWA notebookNew (myPanedPack2 paned True False) [containerBorderWidth := 2]
     --
     -- MOVIE LIST
     --
     vbox1 <- mkWA (vBoxNew False 2) (myPanedPack1 paned True False) [containerBorderWidth := 2]
     label <- mkWA (labelNewWithMnemonic "<b>List of _Movies</b>") (myBoxPackStart vbox1 PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     swin <- mkWA (myScrolledWindowNew PolicyAutomatic PolicyAutomatic) (myBoxPackStart vbox1 PackGrow 0) [widgetWidthRequest := 256, widgetHeightRequest := 128]
     smodel1 <- treeModelSortNewWithModel model1
     treeSortableSetDefaultSortFunc smodel1 $ Just $ sortFunc model1 mvNumber
     view1 <- mkWA (treeViewNewWithModel smodel1) (containerAdd swin) [treeViewHeadersVisible := True]
     labelSetMnemonicWidget label view1
     _      <- mkSortCol view1 smodel1 model1 (Just (2,mvNumber))   "#"              cellRendererTextNew   []                              $ \x -> [cellText := show (mvNumber x)]
     rSel1  <- mkSortCol view1 smodel1 model1 (Just (3,mvMark))     "S"              cellRendererToggleNew [cellToggleActivatable := True] $ \x -> [cellToggleActive := mvMark x]
     rTitle <- mkSortCol view1 smodel1 model1 (Just (4,mvTitle))    "Title"          cellRendererTextNew   [cellTextEditable := True]      $ \x -> [cellText := mvTitle x]
     rYear  <- mkSortCol view1 smodel1 model1 (Just (5,mvYear))     "Year"           cellRendererTextNew   [cellTextEditable := True]      $ \x -> [cellText := mvYear x]
     _      <- mkSortCol view1 smodel1 model1 (Just (7,mvTitleCol)) "#T"             cellRendererTextNew   []                              $ \x -> [cellText := maybe "" (show . succ) (mvTitleCol x)]
     _      <- mkSortCol view1 smodel1 model1 (Just (8,mvFileCol))  "#F"             cellRendererTextNew   []                              $ \x -> [cellText := maybe "" (show . succ) (mvFileCol x)]
     rOrig  <- mkSortCol view1 smodel1 model1 (Just (6,mvOriginal)) "Original Title" cellRendererTextNew   [cellTextEditable := True]      $ \x -> [cellText := mvOriginal x]
     box <- mkWA (hBoxNew False 2) (myBoxPackStart vbox1 PackNatural 0) [containerBorderWidth := 2]
     filePathEntry <- mkWA entryNew (myBoxPackStartWithMnemonic box PackGrow 0 "File Pa_th") [entryEditable := False]
     box <- mkW (hBoxNew False 2) (myBoxPackStart vbox1 PackNatural 0)
     sitesButton <- mkWA (buttonNewWithMnemonic "_Sites") (myBoxPackStart box PackNatural 0) []
     unsAllButton <- mkWA (buttonNewWithMnemonic "_Unselect All") (myBoxPackEnd box PackNatural 0) []
     newFilesButton <- mkWA (buttonNewWithMnemonic "Select _New Files") (myBoxPackEnd box PackNatural 0) []
     newTitlesButton <- mkWA (buttonNewWithMnemonic "Select _New Titles") (myBoxPackEnd box PackNatural 0) []
     allButton <- mkWA (buttonNewWithMnemonic "Select _All") (myBoxPackEnd box PackNatural 0) []
     --
     -- CATALOG
     --
     swin <- mkWA (myScrolledWindowNew PolicyAutomatic PolicyAutomatic) (myNotebookAppendPage ntbk "Ca_talog")  [widgetWidthRequest := 128, widgetHeightRequest := 128]
     model2 <-listStoreNew (zip4 [0::Int ..] (repeat False) (repeat E) allMovies)
     smodel2 <- treeModelSortNewWithModel model2
     treeSortableSetDefaultSortFunc smodel2 $ Just $ sortFunc model2 trd4
     view2 <- mkWA (treeViewNewWithModel smodel2) (containerAdd swin) [treeViewHeadersVisible := True]
     _     <- mkSortCol view2 smodel2 model2 (Just (2,fst4))                       "Id"             cellRendererTextNew   []                              $ \(i,_,_,_) -> [cellText := show (i + 1)]
     rSel2 <- mkSortCol view2 smodel2 model2 (Just (3,snd4))                       "Match"          cellRendererToggleNew [cellToggleActivatable := True, cellToggleRadio := True] $ \(_,m,_,_) -> [cellToggleActive := m]
     _     <- mkSortCol view2 smodel2 model2 (Just (4,trd4))                       "Distance"       cellRendererTextNew   []                              $ \(_,_,d,_) -> [cellText := show d]
     _     <- mkSortCol view2 smodel2 model2 (Just (5,(!Title).info.fth4))         "Title"          cellRendererTextNew   []                              $ \(_,_,_,x) -> [cellText := info x ! Title]
     _     <- mkSortCol view2 smodel2 model2 (Just (6,(!Year).info.fth4))          "Year"           cellRendererTextNew   []                              $ \(_,_,_,x) -> [cellText := info x ! Year]
     _     <- mkSortCol view2 smodel2 model2 (Just (7,(!OriginalTitle).info.fth4)) "Original Title" cellRendererTextNew   []                              $ \(_,_,_,x) -> [cellText := info x ! OriginalTitle]
     _     <- mkSortCol view2 smodel2 model2 (Just (8,(!Location).info.fth4))      "Location"       cellRendererTextNew   []                              $ \(_,_,_,x) -> [cellText := info x ! Location]
     --
     -- RESULTS
     --
     vbox2 <- mkWA (vBoxNew False 2) (myNotebookAppendPage ntbk "Search _Results") [containerBorderWidth := 2]
     swin <- mkW (myScrolledWindowNew PolicyAutomatic PolicyAutomatic) (myBoxPackStart vbox2 PackGrow 0)
     view4 <- mkWA (treeViewNewWithModel model4) (containerAdd swin) [treeViewHeadersVisible := False]
     -- it seems that there is a bug in gtk2hs that prevents using a sorted tree model in this case
     --style <- widgetGetStyle view2
     let color1 = "yellow" --color1 <- styleGetBackground style StateNormal
     let color2 = "white" --color2 <- styleGetLight style StateNormal
     let bg x = cellBackground := if not (null (srURL x)) && srMark x then color1 else color2

     let siteAttribs x@SR{srTitle,srURL}
           | null srURL = [ cellVisible := True
                          , cellText := srTitle
                          , cellTextWeightSet := True
                          , cellTextWeight := 900
                          , bg x
                          ]
           | otherwise  = [ cellVisible := False
                          ]

     -- let selAttribs x = [ cellToggleActive := srMark x
     --                    , cellVisible := not (null (srURL x))
     --                    , cellXAlign := 0.0
     --                    , bg x
     --                    , cellToggleRadio := True
     --                    ]

     let selAttribs x@SR{srMark,srURL}
           | null srURL = [ cellVisible := False
                          ]
           | otherwise  = [ cellVisible := True
                          , cellToggleActive := srMark
                          , cellXAlign := 0.0
                          , cellToggleRadio := True
                          , bg x
                          ]

     -- let titleAttribs x = [ cellText := srTitle x
     --                      , bg x
     --                      , cellTextWeightSet := True
     --                      , cellTextWeight := if null (srURL x) then 700 else 400
     --                      ]

     let nl "" = ""
         nl x  = "\n" ++ x

     let sp "" = ""
         sp x  = " " ++ x

     let paren "" = ""
         paren x = " (" ++ x ++ ")"

     let titleAttribs x@SR{srTitle,srImage,srMark,srYear,srOriginal,srCountry,srMedia,srDistance,srURL}
           | null srURL = [ cellText := srTitle
                          , cellTextWeightSet := True, cellTextWeight := 700
                          , cellTextScaleSet := True, cellTextScale := 1.2
                          , bg x
                          ]
           | otherwise  = [ cellTextMarkup := Just $ T.pack $ printf "<b>%s</b><i>%s</i>%s%s%s%s%s"
                                                            (escapeMarkup srTitle)
                                                            (escapeMarkup (nl srOriginal))
                                                            (escapeMarkup (if null srYear && null srCountry then "" else "\n"))
                                                            (escapeMarkup srYear)
                                                            (escapeMarkup (sp srCountry))
                                                            (escapeMarkup (nl srMedia))
                                                            (escapeMarkup (nl (trim (show srDistance))))
                          , cellTextWeightSet := False
                          , cellTextScaleSet := False
                          , bg x
                          ]

     let imgAttribs x@SR{srImage,srURL}
           | null srURL = [ cellVisible := False
                          ]
           | otherwise  = [ cellVisible := True
                          , maybe (cellPixbufStockId := stockMissingImage) (cellPixbuf :=) srImage
                          , bg x
                          ]

     col <- mkViewCol view4 "Title"
     rMark4 <- mkCellRenderer view4 model4 col cellRendererToggleNew [cellToggleActivatable := True] selAttribs
     _      <- mkCellRenderer view4 model4 col cellRendererPixbufNew []                              imgAttribs
     _      <- mkCellRenderer view4 model4 col cellRendererTextNew   []                              titleAttribs

     box <- mkW (hBoxNew False 6) (myBoxPackStart vbox2 PackNatural 0)
     urlEntry <- mkW entryNew (myBoxPackStartWithMnemonic box PackGrow 0 "URL")
     urlButton <- mkWA (buttonNewWithMnemonic "_Browse") (myBoxPackStart box PackNatural 0) [widgetSensitive := False]
     setBaseFromBg urlEntry
     --
     -- COLLECTED DATA PANEL
     --
     (setupCollectedData,clearCollectedData) <- showMovie (myNotebookAppendPage ntbk "_Collected data")
     --
     -- PROGRESS
     --
     mkW (hSpaceNew 2) (myBoxPackStart vbox1 PackNatural 0)
     log <- mkLog (myBoxPackStart vbox1 PackNatural 0)
     --
     return (toWidget paned)
     --
     -- SIGNAL HANDLERS
     --
     let treeStoreForM model f =
           let loop Nothing = return []
               loop (Just iter) =
                 do path <- treeModelGetPath model iter
                    x <- f model path
                    next <- treeModelIterNext model iter
                    xs <- loop next
                    return (x:xs)
           in loop =<< treeModelGetIterFirst model

     let treeStoreGetForest model =
           treeStoreForM model treeStoreGetTree

     let treeStoreGetTopLevel model =
           treeStoreForM model treeStoreGetValue

     let buttonsSetSensitive =
           do movies <- listStoreToList model1
              widgetSetSensitive stopButton False
              widgetSetSensitive clearButton True
              widgetSetSensitive searchButton (any mvMark movies)
              widgetSetSensitive downloadButton (any (\Mv{mvMark,mvResult} -> mvMark && any srMark (concatMap flatten mvResult)) movies)
              assistantSetPageComplete assistant page (any mvMark movies)
     --
     let updateCatalogView mv =
           do let ref = mvRefTitle mv
              size <- listStoreGetSize model2
              forM_ [0 .. size-1] $ \index ->
                do (k, m, _, x) <- listStoreGetValue model2 index
                   listStoreSetValue model2 index (k, maybe False (== k) ref, distanceMvM mv x, x)
              treeViewSetCursor view2 [0] Nothing
     --
     let updateSearchResults xs =
           do treeStoreClear model4
              treeStoreInsertForest model4 [] 0 xs
              treeViewExpandAll view4
     --
     let cellRendererEdited updatefn path x =
           do [i] <- treeModelSortConvertPathToChildPath smodel1 path
              mv <- listStoreGetValue model1 i
              let mv' = updatefn x mv
              listStoreSetValue model1 i mv'
              updateCatalogView mv'

     on rTitle edited $ cellRendererEdited (\x mv -> mv{mvTitle = x})
     on rOrig  edited $ cellRendererEdited (\x mv -> mv{mvOriginal = x})
     on rYear  edited $ cellRendererEdited (\x mv -> mv{mvYear = x})
     --
     let select test status =
           do n <- listStoreGetSize model1
              forM_ [0..n-1] $ \i ->
                do x@Mv{mvMark=m} <- listStoreGetValue model1 i
                   unless (not (test x) || m == status) $
                     listStoreSetValue model1 i x{mvMark=status}
              buttonsSetSensitive
     --
     on allButton buttonActivated $ select (const True) True
     --
     on unsAllButton buttonActivated $ select (const True) False
     --
     on newFilesButton buttonActivated $ select (isNothing . mvFileCol) True
     --
     on newTitlesButton buttonActivated $ select (isNothing . mvTitleCol) True
     --
     on view1 cursorChanged $
       do (path,_) <- treeViewGetCursor view1
          cPath <- treeModelSortConvertPathToChildPath smodel1 path
          case cPath of
            [i] -> do mv@Mv{mvFile,mvResult,mvShowWidgets} <- listStoreGetValue model1 i
                      entrySetText filePathEntry mvFile
                      updateCatalogView mv
                      updateSearchResults mvResult
                      fromMaybe clearCollectedData mvShowWidgets
            _   -> entrySetText filePathEntry ""

     on rSel1 cellToggled $ \strpath ->
       do let path = stringToTreePath strpath
          [i] <- treeModelSortConvertPathToChildPath smodel1 path
          m@Mv{mvMark} <- listStoreGetValue model1 i
          listStoreSetValue model1 i m{mvMark = not mvMark}
          buttonsSetSensitive
     --
     on rSel2 cellToggled $ \strpath2 ->
       do (path1,_) <- treeViewGetCursor view1
          [index1] <- treeModelSortConvertPathToChildPath smodel1 path1
          mv <- listStoreGetValue model1 index1
          let path2 = stringToTreePath strpath2
          [index2] <- treeModelSortConvertPathToChildPath smodel2 path2

          size2 <- listStoreGetSize model2
          let recurse i
                | i == size2 = return Nothing
                | otherwise  = do (k,m,d,x) <- listStoreGetValue model2 i
                                  if m then return (Just i) else recurse (i+1)
          maybeIndex2Old <- recurse 0

          (k,_,d,x) <- listStoreGetValue model2 index2
          listStoreSetValue model2 index2 (k,True,d,x)

          case maybeIndex2Old of
            Just index2Old -> do (k,_,d,x) <- listStoreGetValue model2 index2Old
                                 listStoreSetValue model2 index2Old (k,False,d,x)
            Nothing -> return ()

          (k,s,_,_) <- listStoreGetValue model2 index2
          listStoreSetValue model1 index1 mv{mvTitleCol = if s then Just k else Nothing}
     --
          
     let stopped normal =
           do log $ if normal then "\nFinished." else "\nAborted."
              widgetSetSensitive stopButton False
              buttonsSetSensitive

     on searchButton buttonActivated $
       do log "\nSearching ..."
          widgetSetSensitive stopButton True
          widgetSetSensitive clearButton False
          widgetSetSensitive searchButton False
          widgetSetSensitive downloadButton False
          assistantSetPageComplete assistant page False
          --
          sites <- getSites
          movies <- listStoreToList model1
          let selMovies = filter (mvMark . snd) (zip [0::Int ..] movies)
          let size = length selMovies
          childThread <- forkIO $
            do forM_ (zip [1::Int ..] selMovies) $ \(counter,(index,movie)) ->
                 do postGUISync $ log (printf "\n[%i:%i] %s (%s)\n " counter size (mvTitle movie) (mvYear movie))
                    forM_ (zip [1::Int ..] sites) $ \(j,site@SiteConfig{siteName}) ->
                      do postGUISync $ log (printf " :%s" siteName)
                         res <- searchSite (postGUISync . log) (mvTitle movie, mvOriginal movie, mvYear movie) site
                         let res' = case (sortLike (mvTitle movie, mvOriginal movie, mvYear movie) res) of
                                      m:ms -> m{srMark=True} : ms
                                      [] -> []
                         let x = defaultSR{srTitle = siteName, srSite = site}
                         let node = Node x (map (flip Node []) res')
                         m@Mv{mvResult=r} <- postGUISync $ listStoreGetValue model1 index
                         let r' = r ++ [node]
                         let m' = m{mvFound = True, mvResult = r'}
                         postGUISync $ listStoreSetValue model1 index m'
                         (path,_) <- postGUISync $ treeViewGetCursor view1
                         [cursor] <- postGUISync $ treeModelSortConvertPathToChildPath smodel1 path
                         when (cursor == index) (postGUISync $ updateSearchResults r')
               stopped True
          on stopButton buttonActivated $
            do killThread childThread
               yield
               stopped False
          on assistant assistantCancel $
            do killThread childThread
               yield
               stopped False
          return ()
     --
     on downloadButton buttonActivated $
       do log "\nDownloading ..."
          widgetSetSensitive stopButton True
          widgetSetSensitive clearButton False
          widgetSetSensitive searchButton False
          widgetSetSensitive downloadButton False
          assistantSetPageComplete assistant page False
          --
          location <- getLocation
          Prefs{infoSearch} <- readIORef prefsRef
          movies <- listStoreToList model1
          let selMovies = filter (mvMark . snd) (zip [0::Int ..] movies)
          let size = length selMovies
          childThread <- forkIO $
            do forM_ (zip [1::Int ..] selMovies) $ \(counter,(index,movie)) ->
                 do postGUISync $ log (printf "\n[%i:%i] %s (%s)\n " counter size (mvTitle movie) (mvYear movie))
                    let movieInCollection =
                          case mvTitleCol movie of
                            Just index ->
                              filter (not . null . snd) (assocs (info (allMovies !! index)))
                            _ ->
                              []
                    ms0 <-
                      forM (mvResult movie) $ \Node{subForest} ->
                        case find (\Node{rootLabel=SR{srMark}} -> srMark) subForest of
                          Nothing -> return []
                          Just Node{rootLabel=SR{srTitle,srYear,srURL,srSite}} ->
                            do postGUISync $ log (printf " :%s" (siteName srSite))
                               ys <- downloadMovieInfo srSite srURL
                               return $ map (\(i,x) -> (i,(siteName srSite,x))) ys
                    now <- fmap (\(year,month,day) -> printf "%i/%i/%i" year month day :: String) date
                    let ms = mergeAL (concat ms0) ++ [(File,[("",mvFile movie)]),(Location,[("",location)]),(InclusionDate,[("",now)])]
                    let ms' = organizeFields infoSearch False movieInCollection ms
                    (showWidgets,getFromWidgets) <- postGUISync $ setupCollectedData ms'
                    postGUISync $ listStoreSetValue model1 index movie{mvInfo=ms',mvShowWidgets=Just showWidgets,mvGetFromWidgets=getFromWidgets}
                    --
                    (path,_) <- postGUISync $ treeViewGetCursor view1
                    [cursor] <- postGUISync $ treeModelSortConvertPathToChildPath smodel1 path
                    when (cursor == index) $ postGUISync $ showWidgets
               stopped True
          on stopButton buttonActivated $
            do killThread childThread
               yield
               stopped False
          on assistant assistantCancel $
            do killThread childThread
               yield
               stopped False
          return ()
     on searchButton buttonActivated $
       do log "\nSearching ..."
          
     --
     on clearButton buttonActivated $
       do log "\nREMOVING SEARCH RESULTS AND DOWNLOADED DATA"
          size <- listStoreGetSize model1
          forM_ [0 .. size - 1] $ \index ->
            do m <- listStoreGetValue model1 index
               listStoreSetValue model1 index m{mvResult=[], mvShowWidgets=Nothing, mvGetFromWidgets=return [], mvFound=False}
          updateSearchResults []
          clearCollectedData
     --
     on rMark4 cellToggled $ \strpath ->
       do let path4 = stringToTreePath strpath
          case path4 of
            [i,j] ->
              do Node{subForest} <- treeStoreGetTree model4 [i]
                 treeStoreChange model4 [i,j] $ \x -> x{srMark=True}
                 case findIndex (srMark . rootLabel) subForest of
                   Just k -> do treeStoreChange model4 [i,k] $ \x -> x{srMark=False}
                                return ()
                   Nothing -> return ()
                 forest <- treeStoreGetForest model4
                 (path,_) <- treeViewGetCursor view1
                 cPath <- treeModelSortConvertPathToChildPath smodel1 path
                 case cPath of
                   [k] -> do m <- listStoreGetValue model1 k
                             listStoreSetValue model1 k m{mvResult=forest}
                   _ -> return ()
                 buttonsSetSensitive
            _ ->
              return ()
     --
     on view4 cursorChanged $
       do (path,_) <- treeViewGetCursor view4
          SR{srURL} <- treeStoreGetValue model4 path
          entrySetText urlEntry srURL
          widgetSetSensitive urlButton (not (null srURL))
     --
     on urlButton buttonActivated $
       do url <- entryGetText urlEntry
          Prefs{browser} <- readIORef prefsRef
          if null browser
            then errorMsg (toWindow assistant) "Web browser not defined in preferences"
            else runProcess browser [url] Nothing Nothing Nothing Nothing Nothing >> return ()
     --
     let getSearchResults = return () :: IO ()-- TODO
     return (toWidget paned, getSearchResults)




needArea = [ Cast, Synopsis, Awards, Curiosities, Notes ]
needWeb = [ ParentalGuide ]

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
                              | elem i needWeb ->
                                do scr <- mkWA (myScrolledWindowNew PolicyAutomatic PolicyAutomatic) pack [containerBorderWidth := 2]
                                   w <- mkWA webViewNew (containerAdd scr) [widgetHeightRequest := 128, webViewEditable := False]
                                   webViewLoadHtmlString w x ""
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
              mkW (hSpaceNew 3) (myTableAttach table 0 3 (r'+0) (r'+1) [Fill] [Fill] 0 0)
              mkW hSeparatorNew (myTableAttach table 0 3 (r'+1) (r'+2) [Fill] [Fill] 0 0)
              mkW (hSpaceNew 3) (myTableAttach table 0 3 (r'+2) (r'+3) [Fill] [Fill] 0 0)
              widgetShowAll table
              return (r'+3, (i,ws):mvWidgets)

     let showMv table =
           do clearMv
              containerAdd viewport table
              lower <- adjustmentGetLower vadj
              adjustmentSetValue vadj lower

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
  where f Cast          = sepCast
        f Country       = sep ','  (==)
        f Rating        = sep '\n' (==)
        f Genres        = sep ','  (==)
        f Release       = sep ','  (==)
        f Certification = sep ','  (==)
        f _             = concat

sep :: Char -> (String -> String -> Bool) -> [String] -> String
sep s eq = intercalate [s] . nubBy eq . (map trim) . concat . (map (wordsBy (==s)))

sepCast = castToString . nubBy (\(k1,_) (k2,_) -> k1 == k2) . concat . map parseCast




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


findTitleInCollection allMovies mv =
  findIndex (goodDistance . distanceMvM mv) allMovies

findFileInCollection allMovies Mv{mvFile} =
  findIndex ((== mvFile) . (!File) . info) allMovies


searchSite log (title,original,year) site@SiteConfig{parserForSearch} =
  do res <- go title
     case res of
       [] | not (null original) -> go original
       _                        -> return res
  where
    go title =
      do src <- webSearch site (trim title)
         case src of
           Left err ->
             do log ("\nERROR: " ++ show err)
                return []
           Right doc ->
             do writeFile ("search.tags.html") doc
                let tags = parseTags doc
                writeFile "search.tags.hs" (unlines (map show tags))
                case parse parserForSearch "tagsoup" tags of
                  Left err -> return []
                  Right xs ->
                    forM xs $ \SR1{ sr1Title=srTitle
                                  , sr1Original=srOriginal
                                  , sr1Country=srCountry
                                  , sr1Year=srYear
                                  , sr1Genres=srGenres
                                  , sr1Media=srMedia
                                  , sr1URL=srURL
                                  , sr1Image=img
                                  } ->
                      do srImage <- case img of
                                      Nothing -> return Nothing
                                      Just uri -> downloadImgSearch "cache" uri
                         return SR{ srTitle
                                  , srOriginal
                                  , srYear
                                  , srCountry
                                  , srGenres
                                  , srMedia
                                  , srURL
                                  , srMark=False
                                  , srSite=site
                                  , srFound=True
                                  , srDistance=distance (title,original,year) (srTitle,srOriginal,srYear)
                                  , srImage
                                  }

thumbSize = 96

downloadImgSearch imgDir uri =
  do res <- try (createDirectoryIfMissing True imgDir)
     case res of
       Left ex -> putStrLn (show (ex::IOException)) >> return Nothing
       Right _ -> do fname <- downloadCover imgDir uri uri
                     myPixbufNewFromFileAtSize fname thumbSize thumbSize


sortLike (title,original,year) =
  map snd .
  sortBy (\(a,_) (b,_) -> compare a b) .
  map (\m@SR{srTitle,srOriginal,srYear} -> (distance (title,original,year) (srTitle,srOriginal,srYear), m))



downloadMovieInfo :: SiteConfig -> String -> IO [(Info, String)]
downloadMovieInfo SiteConfig{parsersForInfo,urlForMovie} uriMovie =
  do movie <- concat <$> forM parsersForInfo (getMovieSection (urlForMovie uriMovie))
     movie' <- downCover "images" movie
     return movie'





hSpaceNew height =
  do label <- labelNew (Nothing :: Maybe String)
     set label [widgetHeightRequest := height]
     return label

vSpaceNew width =
  do label <- labelNew (Nothing :: Maybe String)
     set label [widgetWidthRequest := width]
     return label





mergeAL :: Eq a => [(a, b)] -> [(a, [b])]
mergeAL [] = []
mergeAL ((k,v):xs) = let (xs1,xs2) = partition ((==k) . fst) xs
                     in (k, v : map snd xs1) : mergeAL xs2

tieField :: [(Info, InfoPref)] -> (Info, [(SiteName, String)]) -> (Info, String)
tieField infoPrefs (i,xs) =
  (i,y)
  where
    y = case lookup i infoPrefs of
          Just IPFirst -> snd (head xs)
          Just IPLast -> snd (last xs)
          Just IPCombine -> combineValues (map snd xs)
          Just (IPSelected ks) -> case mapMaybe (\k -> lookup k xs) ks of
                                     x:_ -> x
                                     _   -> snd (head xs) -- ""
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
                 Just (IPSelected ks) -> map (\x@(s,_) -> (elem s ks,x)) xs
                 Nothing -> let x:rest = xs in (True,x) : map (False,) rest






mkLog pack =
  do box <- mkWA (vBoxNew False 2) pack [containerBorderWidth := 0]
     label <- mkWA (labelNewWithMnemonic "<b>_Progress</b>") (myBoxPackStart box PackNatural 0) [miscXalign := 0, labelUseMarkup := True]
     scrwin <- mkWA (myScrolledWindowNew PolicyAutomatic PolicyAutomatic) (myBoxPackStart box PackGrow 0) [containerBorderWidth := 2]
     view <- mkWA textViewNew (containerAdd scrwin) [widgetWidthRequest := 128, widgetHeightRequest := 96, textViewEditable := False, textViewWrapMode := WrapWordChar]
     labelSetMnemonicWidget label view
     setBaseFromBg view
     buf <- textViewGetBuffer view
     insert <- textBufferGetInsert buf
     iter <- textBufferGetEndIter buf
     --
     let log msg =
           do -- textBufferInsertAtCursor buf msg
              textBufferInsert buf iter msg
              textViewScrollMarkOnscreen view insert
     --
     return log



listStoreChangeAll model f =
  do size <- listStoreGetSize model
     forM_ [0 .. size - 1] $ \index ->
       do x <- listStoreGetValue model index
          listStoreSetValue model index (f x)



forestToAssociationList forest =
  map f forest
  where
    f (Node{rootLabel=SR{srTitle},subForest}) =
      (srTitle, map rootLabel subForest)
