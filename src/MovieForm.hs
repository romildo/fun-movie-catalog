{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}

module MovieForm where

import Control.Monad (forM, forM_)
import Graphics.UI.Gtk hiding (Release, after)
import Graphics.UI.Gtk.WebKit.WebView (webViewNew, webViewEditable)
import Data.Array.IArray (array, (!), (//))
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import Data.Maybe (fromJust)
import Data.List ((\\))
import Data.List.Utils (split, join)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

import Util (trim, findPos, whenM, after)
import Movie
import Component
import MovieFormType (MovieForm(MovieForm,mfIdent,mfInfo,mfComment,mfDisc,mfMovie))
import CastGUI (castViewNew)
import Rating (ratingsViewNew)
import Comments (drawComments)
import InternetSearch (internetSearch)
import MovieHtml (movieToHtml)


movieFormNew window ref prefsRef pack =
  do mainNtbk <- mkWA notebookNew pack [containerBorderWidth := 2]
     
     boxMain <- mkW (vBoxNew False 2) (myNotebookAppendPage mainNtbk "Mo_vie Details")

     -- title, year on top box
     -- other info on medium box
     boxTop <- mkW (hBoxNew False 2) (myBoxPackStart boxMain PackGrow 0)

     -- for the title, original title, ...
     boxTopM <- mkWA (vBoxNew False 2) (myBoxPackStart boxTop PackGrow 0) [containerBorderWidth := 2]
     tableT <- mkWA (tableNew 2 4 False) (myBoxPackStart boxTopM PackGrow 0) [containerBorderWidth := 2]
     boxTId <- mkWA (hBoxNew False 2) (myTableAttach tableT 0 1 0 1 [Fill] [Fill] 0 0) [containerBorderWidth := 2]
     --frameId <- mkW frameNew (myBoxPackEnd boxTId PackGrow 2)
     ident <- myEntryNewWithText [entryWidthChars := 3, entryEditable := False, widgetSensitive := False] "0" (myBoxPackEnd boxTId PackGrow 2)

     -- for internet search button, cover and location
     boxInternetCover <- mkWA (vBoxNew False 2) (myBoxPackStart boxTop PackNatural 2) [containerBorderWidth := 2]
     buttonInternet <- mkWA (buttonNewWithMnemonic "_Internet search") (myBoxPackStart boxInternetCover PackNatural 0) []
     buttonCover <- mkWA buttonNew (myBoxPackStart boxInternetCover PackNatural 0) [containerBorderWidth := 0]

     -- for other images
     -- imagesExpander <- mkW (expanderNewWithMnemonic "_Screenshots") (myBoxPackStart boxTop PackNatural 0)
     -- imgBox <- mkW (vBoxNew False 2) (myBoxPackStart boxTop PackNatural 2)
     -- mkW (labelNewWithMnemonic "_Screenshots") (myBoxPackStart imgBox PackNatural 2)
     -- screenShots <- forM [2,3,4,5] $ \x ->
     --                  do b <- mkWA buttonNew (myBoxPackStart imgBox PackNatural 0) [containerBorderWidth := 2]
     --                     mkW (imageNewFromStock stockNo IconSizeButton) (containerAdd b)
     --                     on b buttonActivated (imageEdit ref window)
     --                     return b

     -- img2 <- myImageNew (myBoxPackStart imgBox PackNatural 0)
     -- img3 <- myImageNew (myBoxPackStart imgBox PackNatural 0)
     -- img4 <- myImageNew (myBoxPackStart imgBox PackNatural 0)
     -- img5 <- myImageNew (myBoxPackStart imgBox PackNatural 0)

     scrwin <- mkWA (scrolledWindowNew Nothing Nothing)
                    (myNotebookAppendPage mainNtbk "_Summary")
                    [ scrolledWindowHscrollbarPolicy := PolicyAutomatic
                    , scrolledWindowVscrollbarPolicy := PolicyAutomatic
                    , scrolledWindowShadowType       := ShadowIn
                    , containerBorderWidth           := 2
                    ]
     movieWebView <- mkWA webViewNew (containerAdd scrwin) [webViewEditable := False]

     parentalguideScrWin <- mkWA (scrolledWindowNew Nothing Nothing)
                    (myNotebookAppendPage mainNtbk "_Parental Guide")
                    [ scrolledWindowHscrollbarPolicy := PolicyAutomatic
                    , scrolledWindowVscrollbarPolicy := PolicyAutomatic
                    , scrolledWindowShadowType       := ShadowIn
                    , containerBorderWidth           := 2
                    ]
     -- parentalGuideTextView <- mkWA textViewNew (containerAdd scrwin) [textViewEditable := False]
     -- b <- textViewGetBuffer parentalGuideTextView
     -- tagTable <- textBufferGetTagTable b
     -- tag <- textTagNew (Just "test")
     -- set tag [ textTagForegroundSet := True, textTagForeground := "blue" ]
     -- textTagTableAdd tagTable tag
     
     comments <- drawComments [] (myNotebookAppendPage mainNtbk "_Comments")

     -- for the detail info and additional text info
     p <- mkWA hPanedNew (myBoxPackStart boxMain PackGrow 0) [containerBorderWidth := 2]
     -- for other less important info
     boxDetails <- mkWA (vBoxNew False 2) (\w -> panedPack1 p w True False) [containerBorderWidth := 2]
     detailsNtbk <- mkWA notebookNew (myBoxPackStart boxDetails PackGrow 0) [containerBorderWidth := 2]
     tableB <- mkWA (tableNew 2 4 False) (myNotebookAppendPage detailsNtbk "_Technical Details") [containerBorderWidth := 2]
     tableC <- mkWA (tableNew 2 2 False) (myNotebookAppendPage detailsNtbk "_More Details") [containerBorderWidth := 2]
     -- sinopsys, notes, comments, cast, ratings, ...
     boxBR <- mkWA (vBoxNew False 2) (\w -> panedPack2 p w True False) [containerBorderWidth := 2]
     synNtbk <- mkWA notebookNew (myBoxPackStart boxBR PackGrow 2) [containerBorderWidth := 2]
     -- discs
     discs <- drawDiscs (myNotebookAppendPage mainNtbk "_Discs")
     -- ripper and uploader
     ripperBox <- mkWA (hBoxNew False 2) (myBoxPackStart boxMain PackNatural 2) [containerBorderWidth := 2]

     let componentDescriptions =
           [
             (Cover           , myImageNew                            (containerAdd buttonCover))
           , (Title           , myEntryNew [ entryWidthChars := 40 ]  (\w -> do mkWA (labelNewWithMnemonic "_Title")
                                                                                     (myBoxPackStart boxTId PackNatural 0)
                                                                                     [labelMnemonicWidget := w]
                                                                                f <- fontDescriptionNew
                                                                                fontDescriptionSetWeight f WeightBold
                                                                                widgetModifyFont w (Just f)
                                                                                tableAttachDefaults tableT w 1 4 0 1))
           , (OriginalTitle   , myEntryNew []                         (packInTableD tableT 2 0 4 "_Original Title"))
           , (AlternateTitle  , myEntryNew []                         (packInTableD tableT 3 0 4 "_Alternate Title"))
           , (Genres          , myEntryWithButtonNew (editGenres ref) (packInTableD tableT 4 0 4 "_Genres"))
           , (Director        , myEntryNew []                         (packInTableD tableT 5 0 4 "_Director"))
           , (Stars           , myEntryNew []                         (packInTableD tableT 6 0 4 "_Stars"))
           , (Year            , myEntryNew [ entryWidthChars := 4  ]  (packInTableD tableT 7 0 2 "_Production Year"))
           , (Release         , myEntryNew [ entryWidthChars := 10 ]  (packInTableD tableT 8 0 2 "_Release Date"))
           , (Country         , myEntryNew []                         (packInTableD tableT 7 2 2 "_Country"))
           , (Length          , myEntryWithSuffixNew "min"            (packInTableD tableT 8 2 2 "_Length"))
           -- , (File            , myFileChooserButtonNew "Select A Media File" FileChooserActionOpen (packInTableD tableT 9 0 4 "F_ile"))
           , (File            , myEntryWithButtonNew (chooseFile window) (packInTableD tableT 9 0 4 "F_ile"))

           , (Location        , myEntryNew []                         (myBoxPackStartWithMnemonic boxInternetCover PackNatural 2 "_Location"))

           , (Type            , myEntryNew []                         (packInTableD tableB 0 0 4 "_Type"))
           -- , (Rating          , myEntryNew []                         (packInTableD tableB 1 0 4 "IMDB _Rating"))
           , (Score           , myEntryNew []                         (packInTableD tableB 2 0 4 "_Score"))
           , (Subtitles       , myEntryWithButtonNew (editLangs ref)  (packInTableD tableB 3 0 4 "_Subtitles"))
           , (Audios          , myEntryWithButtonNew (editLangs ref)  (packInTableD tableB 4 0 4 "_Audios"))
           , (Screen          , myComboBoxEntryNew defaultScreens ""  (packInTableD tableB 5 0 4 "_Screen"))
           , (Format          , myComboBoxEntryNew defaultFormats ""  (packInTableD tableB 6 0 4 "For_mat"))
           , (Theme           , myEntryNew []                         (packInTableD tableB 7 0 4 "_Theme"))
           , (Certification   , myComboBoxEntryNew defaultCertifications "" (packInTableD tableB 8 0 4 "_Certification"))
           , (ParentalGuide   , myWebViewNew []                       (containerAdd parentalguideScrWin))
           , (Contains        , myEntryNew []                         (packInTableD tableB 9 0 4 "_Contains"))
           , (Keywords        , myEntryNew []                         (packInTableD tableB 10 0 4 "_Keywords"))
           , (Site            , myEntryNew []                         (packInTableD tableB 11 0 4 "_Site"))
           , (IMDB            , myEntryNew []                         (packInTableD tableB 12 0 4 "_Link IMDB"))
           , (InclusionDate   , myEntryNew [ entryEditable := False, entryWidthChars := 10 ] (packInTableD tableB 13 0 4 "_Inclusion Date"))

           , (Studio          , myEntryNew []                         (packInTableD tableC 1 0 2 "_Studio"))
           , (Distributor     , myEntryNew []                         (packInTableD tableC 2 0 2 "_Distributor"))
           , (Screenwriter    , myEntryNew []                         (packInTableD tableC 3 0 2 "_Screenwriter"))
           , (Producer        , myEntryNew []                         (packInTableD tableC 4 0 2 "_Producer"))
           , (Music           , myEntryNew []                         (packInTableD tableC 5 0 2 "_Music"))
           , (Photography     , myEntryNew []                         (packInTableD tableC 6 0 2 "_Photography"))
           , (ProductionDesign, myEntryNew []                         (packInTableD tableC 7 0 2 "_Production Design"))
           , (ArtDirection    , myEntryNew []                         (packInTableD tableC 8 0 2 "_Art Direction"))
           , (SetDecorators   , myEntryNew []                         (packInTableD tableC 9 0 2 "_Set Decorators"))
           , (Figurino        , myEntryNew []                         (packInTableD tableC 10 0 2 "Fi_gurino"))
           , (MakeUp          , myEntryNew []                         (packInTableD tableC 11 0 2 "_Make-up"))
           , (Editor          , myEntryNew []                         (packInTableD tableC 12 0 2 "_Editor"))
           , (CastingDirectors, myEntryNew []                         (packInTableD tableC 13 0 2 "_Casting Directors"))
           , (SpecialEffects  , myEntryNew []                         (packInTableD tableC 14 0 2 "_Special Effects"))
           , (ProductionManagers, myEntryNew []                       (packInTableD tableC 15 0 2 "_Production Managers"))

           , (Cast            , castViewNew ""                        (myBoxPackStart boxBR PackGrow 2))
           , (Rating          , ratingsViewNew ""                     (myBoxPackStart boxBR PackGrow 2))

           , (Synopsis        , myTextViewNew (-1) (-1) ""            (myNotebookAppendPage synNtbk "_Synopsys"))
           , (Awards          , myTextViewNew (-1) (-1) ""            (myNotebookAppendPage mainNtbk "_Awards"))
           , (Curiosities     , myTextViewNew (-1) (-1) ""            (myNotebookAppendPage mainNtbk "_Curiosities"))
           , (Notes           , myTextViewNew (-1) (-1) ""            (myNotebookAppendPage mainNtbk "_Notes"))

           , (Ripper          , myEntryNew []                         (myBoxPackStartWithMnemonic ripperBox PackGrow 2 "_Ripper"))
           , (Uploader        , myEntryNew []                         (myBoxPackStartWithMnemonic ripperBox PackGrow 2 "_Uploader"))
           ]

     components <- forM componentDescriptions $ \(i,c) -> c >>= return . (i,)

     let movieWidgets = MovieForm{mfIdent=ident,mfInfo=components,mfComment=comments,mfDisc=discs,mfMovie=movieWebView}

     on buttonInternet buttonActivated (internetSearch window ref prefsRef movieWidgets)

     on buttonCover buttonActivated (imageEdit ref window (fromJust (lookup Cover components)))

     return movieWidgets




-- update widgets with movie data
movieFormSetMovie (MovieForm id components (setComments,_,_) (setDiscs,_) movieWebView) movie =
  do setText id (show (ident movie))
     mapM_ (\(i,c) -> setText c (info movie ! i)) components
     setComments (comments movie)
     setDiscs (discs movie)
     setMovieWebView movieWebView movie

-- retrieve data from widgets
movieFormGetMovie (MovieForm id components (_,getComments,_) (_,getDiscs) _) movie =
  do ident' <- getText id
     info' <- forM components $ \(i,c) -> getText c >>= return . (i,)
     comments' <- getComments
     discs' <- getDiscs
     return $ movie
               { ident = read ident'
               , info = info movie // info'
               , comments = comments'
               , discs = discs'
               }


drawDiscs pack =
  do mainBox <- mkWA (vBoxNew False 0) pack [containerBorderWidth := 2]
     ntbk0 <- mkWA notebookNew (myBoxPackStart mainBox PackGrow 0) [containerBorderWidth := 2, notebookTabPos := PosLeft]
     buttonBox <- mkWA (hBoxNew False 2) (myBoxPackStart mainBox PackNatural 0) [containerBorderWidth := 2]
     buttonAdd <- mkW (buttonNewFromStock stockAdd) (myBoxPackStart buttonBox PackNatural 2)
     buttonRemove <- mkW (buttonNewFromStock stockRemove) (myBoxPackStart buttonBox PackNatural 2)
     wssRef <- newIORef []
     let addDisc disc =
           do n <- notebookGetNPages ntbk0
              box <- mkWA (vBoxNew False 0) (myNotebookAppendPage ntbk0 ("Disc " ++ show (n+1))) [containerBorderWidth := 2]
              t <- mkWA (tableNew 0 2 False) (myBoxPackStart box PackNatural 0) [containerBorderWidth := 2]
              ntbk <- mkWA notebookNew (myBoxPackStart box PackGrow 0) [containerBorderWidth := 2]
              let controlDescriptions =
                    [ (Source         , myEntryNewWithText [], packInTableD t 0 0 2 "_Source"            )
                    , (Conversion     , myEntryNewWithText [], packInTableD t 1 0 2 "_Conversion"        )
                    , (Bitrate        , myEntryNewWithText [], packInTableD t 2 0 2 "_Bitrate"           )
                    , (Reduction      , myEntryNewWithText [], packInTableD t 3 0 2 "_Reduction"         )
                    , (ImageFormat    , myComboBoxEntryNew ["", "Alcohol DVD", "CloneDVD DVD", "DVD Files", "ISO DVD", "Nero DVD"], (packInTableD t 4 0 2 "_Image"))
                    , (Size           , myEntryNewWithText [entryWidthChars := 6], packInTableD t 5 0 2 "_Size" )
                    , (URL            , myEntryNewWithText [], packInTableD t 6 0 2 "_URL"               )
                    , (Episodes       , myTextViewNew 400 50 , myNotebookAppendPage ntbk "_Episodes"      )
                    , (Extras         , myTextViewNew 400 50 , myNotebookAppendPage ntbk "_Extras"        )
                    , (Links          , myTextViewNew 400 50 , myNotebookAppendPage ntbk "_Links"         )
                    ]
              ws <- forM controlDescriptions $ \(i,mk,pack) ->
                      mk (disc!i) pack >>= return . (i,)
              widgetShowAll box
              --widgetSetSensitive buttonRemove True
              return ws
     on ntbk0 switchPage $ \i ->
       do n <- notebookGetNPages ntbk0
          widgetSetSensitive buttonRemove (i == n - 1)
     on buttonAdd buttonActivated $
       do n <- notebookGetNPages ntbk0
          ws <- addDisc emptyDiscInfo
          notebookSetCurrentPage ntbk0 n
          modifyIORef wssRef $ (++[ws])
     on buttonRemove buttonActivated $
       do i <- notebookGetCurrentPage ntbk0
          notebookRemovePage ntbk0 i
          modifyIORef wssRef $ \wss ->
            let (wss1,_:wss2) = splitAt i wss
            in wss1 ++ wss2
          n <- notebookGetNPages ntbk0
          widgetSetSensitive buttonRemove (n > 0)
     let set discs =
           do containerForeach ntbk0 $ containerRemove ntbk0
              wss <- forM discs addDisc
              widgetShowAll ntbk0
              writeIORef wssRef wss
     let get =
           do wss <- readIORef wssRef
              forM wss $ \ws ->
                do xs <- forM ws $ \(i,w) -> getText w >>= return . (i,) . trim
                   return $ array discInfoBounds xs
     return (set,get)





editGenres_old ref text =
  do let list = (map trim (split "," text))
     d <- dialogNew
     set d [windowDefaultHeight := 600]
     windowSetTitle d "Genres Edition"
     dialogAddButton d stockCancel ResponseReject
     ok <- dialogAddButton d stockOk ResponseAccept
     widgetGrabDefault ok
     u <- dialogGetUpper d
     box <- mkWA (hBoxNew False 2) (boxPackStartDefaults u) [containerBorderWidth := 2]
     --
     scrwin <- mkWA (scrolledWindowNew Nothing Nothing)
                    (myBoxPackStart box PackGrow 0)
                    [ scrolledWindowHscrollbarPolicy := PolicyNever
                    , scrolledWindowVscrollbarPolicy := PolicyAutomatic
                    , scrolledWindowShadowType       := ShadowIn
                    , containerBorderWidth           := 2
                    ]
     status@Col{genres} <- readIORef ref
     model <- listStoreNew (map (\(x,y) -> (x,y,elem x list)) genres)
     view <- mkWA (treeViewNewWithModel model)
                  (containerAdd scrwin)
                  [ treeViewHeadersVisible := True
                  ]
     --widgetSetSizeRequest view (-1) 480

     let background marked =
             cellBackground := if marked then "yellow" else "white"

     r1 <- mkCol view model "Language"
                 cellRendererTextNew [] $
                 \(x,_,m) -> [ cellText := x, background m ]
     r2 <- mkCol view model "Translation"
                 cellRendererTextNew [cellTextEditable := True] $
                 \(_,x,m) -> [ cellText := x, background m ]
     r3 <- mkCol view model "Selected"
                 cellRendererToggleNew [cellToggleActivatable := True] $
                 \(_,_,m) -> [ cellToggleActive := m, background m ]
     --
     boxL <- mkWA (vBoxNew False 2) (myBoxPackEnd box PackNatural 0) [containerBorderWidth := 2]
     mkWA (labelNewWithMnemonic "_Genre") (myBoxPackStart boxL PackNatural 0) [miscXalign := 0]
     e1 <- mkW entryNew (myBoxPackStart boxL PackNatural 0)
     mkWA (labelNewWithMnemonic "_Translation") (myBoxPackStart boxL PackNatural 0) [miscXalign := 0]
     e2 <- mkW entryNew (myBoxPackStart boxL PackNatural 0)
     --bPol <- mkW (checkButtonNewWithMnemonic "_Scroll") (myBoxPackEnd boxL PackNatural 0)
     bRem <- mkWA (buttonNewFromStock stockRemove) (myBoxPackEnd boxL PackNatural 0) [widgetSensitive := False]
     bAdd <- mkW (buttonNewFromStock stockAdd) (myBoxPackEnd boxL PackNatural 0)
     --
     on r2 edited $ \[i] x -> do (a,b,c) <- listStoreGetValue model i
                                 listStoreSetValue model i (a,x,c)

     on r3 cellToggled $ \p -> do let [i] = stringToTreePath p
                                  (a,b,c) <- listStoreGetValue model i
                                  listStoreSetValue model i (a,b,not c)

     on bRem buttonActivated $ whenM (askMsg (toWindow d) "Really want to remove item?")
                                 (removeFromListView view model [bRem])

     -- onToggled bPol $ do isActive <- toggleButtonGetActive bPol
     --                     let policy = if isActive then PolicyAutomatic else PolicyNever
     --                     set scrwin [ scrolledWindowVscrollbarPolicy := policy ]
     --                     set r3 [ cellBackground := if isActive then "yellow" else "white", cellBackgroundSet := True ]
     --
     on view cursorChanged $
       do widgetSetSensitive bRem True
          ([pos],_) <- treeViewGetCursor view
          (x,y,z) <- listStoreGetValue model pos
          entrySetText e1 x
          entrySetText e2 y
     --
     on bAdd buttonActivated $
       do t1 <- fmap trim (entryGetText e1)
          t2 <- fmap trim (entryGetText e2)
          if null t1
             then errorMsg (toWindow d) "The genre cannot be empty."
             else do list <- listStoreToList model
                     case findPos (\(x,_,_) -> compare t1 x) list of
                       Left  _ -> errorMsg (toWindow d) "Genre is already in the list."
                       Right i -> do listStoreInsert model i (t1,t2,False)
                                     treeViewSetCursor view [i] Nothing
     --
     widgetShowAll box
     resp <- dialogRun d
     widgetDestroy d
     case resp of
       ResponseAccept -> do list <- listStoreToList model
                            writeIORef ref status{genres=map (\(x,y,_) -> (x,y)) list}
                            let result = map (\(x,_,_) -> x) (filter (\(_,_,set) -> set) list)
                            return (Just (join "," result))
       _ -> return Nothing



editGenres ref text =
  do d <- dialogNew
     set d [windowDefaultHeight := 600]
     windowSetTitle d "Genres Edition"
     dialogAddButton d stockCancel ResponseReject
     ok <- dialogAddButton d stockOk ResponseAccept
     widgetGrabDefault ok
     u <- dialogGetUpper d
     box <- mkWA (hBoxNew False 2) (boxPackStartDefaults u) [containerBorderWidth := 2]
     --
     vbox <- mkW (vBoxNew False 2) (myBoxPackStart box PackGrow 0)
     mkWA (labelNewWithMnemonic "Available _Genres") (myBoxPackStart vbox PackNatural 0) [miscXalign := 0]
     scrwin <- mkWA (scrolledWindowNew Nothing Nothing) (myBoxPackStart vbox PackGrow 0)
                    [ scrolledWindowHscrollbarPolicy := PolicyNever
                    , scrolledWindowVscrollbarPolicy := PolicyAutomatic
                    , scrolledWindowShadowType       := ShadowOut
                    , containerBorderWidth           := 2
                    ]
     status@Col{genres} <- readIORef ref
     let selectedGenres = map trim (split "," text)
     rawmodel1 <- listStoreNew (map fst genres \\ selectedGenres)
     model1 <- treeModelSortNewWithModel rawmodel1
     treeSortableSetDefaultSortFunc model1 $ Just $ \iter1 iter2 ->
       do x1 <- treeModelGetRow rawmodel1 iter1
          x2 <- treeModelGetRow rawmodel1 iter2
          return (compare x1 x2)
     view1 <- mkWA (treeViewNewWithModel model1) (containerAdd scrwin) [treeViewHeadersVisible := False]
     col <- treeViewColumnNew
     treeViewColumnSetTitle col "Genre"
     rend <- cellRendererTextNew
     cellLayoutPackStart col rend True
     cellLayoutSetAttributeFunc col rend model1 $ \iter ->
       do cIter <- treeModelSortConvertIterToChildIter model1 iter
          x <- treeModelGetRow rawmodel1 cIter
          set rend [cellText := x]
     treeViewAppendColumn view1 col
     -- mkCol view1 model1 "Genre" cellRendererTextNew [] $ \x -> [cellText := x]
     --
     vbox <- mkW (vBoxNew False 2) (myBoxPackStart box PackNatural 0)
     selButton <- mkWA buttonNew (myBoxPackStart vbox PackNatural 0) [widgetSensitive := False, buttonImage :=> imageNewFromStock stockGoForward IconSizeButton]
     unselButton <- mkWA buttonNew (myBoxPackStart vbox PackNatural 0) [widgetSensitive := False, buttonImage :=> imageNewFromStock stockGoBack IconSizeButton]
     --
     vbox <- mkW (vBoxNew False 2) (myBoxPackStart box PackGrow 0)
     mkWA (labelNewWithMnemonic "_Selected Genres") (myBoxPackStart vbox PackNatural 0) [miscXalign := 0]
     scrwin <- mkWA (scrolledWindowNew Nothing Nothing) (myBoxPackStart vbox PackGrow 0)
                    [ scrolledWindowHscrollbarPolicy := PolicyNever
                    , scrolledWindowVscrollbarPolicy := PolicyAutomatic
                    , scrolledWindowShadowType       := ShadowOut
                    , containerBorderWidth           := 2
                    ]
     model2 <- listStoreNew selectedGenres
     view2 <- mkWA (treeViewNewWithModel model2) (containerAdd scrwin) [treeViewHeadersVisible := False]
     r2 <- mkCol view2 model2 "Genre" cellRendererTextNew [cellTextEditable := True] $ \x -> [cellText := x]
     newButton <- mkW (buttonNewFromStock stockNew) (myBoxPackStart vbox PackNatural 2)
     --
     vbox <- mkW (vBoxNew False 2) (myBoxPackStart box PackGrow 0)
     upButton <- mkWA buttonNew (myBoxPackStart vbox PackNatural 0) [buttonImage :=> imageNewFromStock stockGoUp IconSizeButton]
     downButton <- mkWA buttonNew (myBoxPackStart vbox PackNatural 0) [buttonImage :=> imageNewFromStock stockGoDown IconSizeButton]
     --
     on r2 edited $ \[i] x -> listStoreSetValue model2 i x
     --
     on view1 cursorChanged $
       widgetSetSensitive selButton True
     --
     on view2 cursorChanged $
       do (path,_) <- treeViewGetCursor view2
          case path of
            [i] -> do n <- listStoreGetSize model2
                      widgetSetSensitive upButton (i > 0)
                      widgetSetSensitive downButton (i < n - 1)
            _   -> return ()
          widgetSetSensitive unselButton True
     --
     on selButton buttonActivated $
       do (path,_) <- treeViewGetCursor view1
          rawpath <- treeModelSortConvertPathToChildPath model1 path
          case rawpath of
            [i] -> do let [k] = path
                      x <- listStoreGetValue rawmodel1 i
                      (path,_) <- treeViewGetCursor view2
                      case path of
                        [j] -> do listStoreInsert model2 (j+1) x
                                  treeViewSetCursor view2 [j+1] Nothing
                        _ -> do listStoreAppend model2 x
                                size <- listStoreGetSize model2
                                treeViewSetCursor view2 [size-1] Nothing
                      listStoreRemove rawmodel1 i
                      size <- listStoreGetSize rawmodel1
                      if size > 0
                      then treeViewSetCursor view1 [if k == size then k - 1 else k] Nothing
                      else widgetSetSensitive selButton False
            _   -> return ()
     --
     on unselButton buttonActivated $
       do (path,_) <- treeViewGetCursor view2
          case path of
            [i] -> do x <- listStoreGetValue model2 i
                      listStoreAppend rawmodel1 x
                      listStoreRemove model2 i
                      size <- listStoreGetSize model2
                      if size > 0
                      then treeViewSetCursor view2 [if i == size then i - 1 else i] Nothing
                      else widgetSetSensitive unselButton False
            _   -> return ()
     --
     on upButton buttonActivated $
       do (path,_) <- treeViewGetCursor view2
          case path of
            [i] -> listStoreSwapRows model2 view2 i (i - 1)
            _   -> return ()
     --
     on downButton buttonActivated $
       do (path,_) <- treeViewGetCursor view2
          case path of
            [i] -> listStoreSwapRows model2 view2 i (i + 1)
            _   -> return ()
     --
     on newButton buttonActivated $
       do widgetGrabFocus view2
          Just c <- treeViewGetColumn view2 0
          (treePath,_) <- treeViewGetCursor view2
          case treePath of
            [i] -> do listStoreInsert model2 (i+1) ""
                      treeViewSetCursor view2 [i+1] (Just (c, True))
            _   -> do j <- listStoreAppend model2 ""
                      treeViewSetCursor view2 [j] (Just (c, True))

     widgetShowAll box
     resp <- dialogRun d
     widgetDestroy d
     case resp of
       ResponseAccept -> do genres1 <- listStoreToList rawmodel1
                            genres2 <- listStoreToList model2
                            writeIORef ref status{genres=map (,"") (genres1 ++ genres2)}
                            return (Just (join "," genres2))
       _ -> return Nothing


editLangs ref text =
    return Nothing


removeFromListView view model widgets =
  do ([pos],_) <- treeViewGetCursor view
     listStoreRemove model pos
     size <- listStoreGetSize model
     if size > 0
        then let pos' = if pos > 0 then pos - 1 else pos
             in treeViewSetCursor view [pos'] Nothing
        else forM_ widgets $ \w -> widgetSetSensitive w False


imageEdit ref window w =
  do chooser <-
       fileChooserDialogNew
         (Just "Image Path")
         (Just window)
         FileChooserActionOpen
         [("stockCancel", ResponseCancel), ("stockOpen", ResponseAccept)]
     path <- getText w
     fileChooserSetFilename chooser path
     resp <- dialogRun chooser
     widgetDestroy chooser `after`
       case resp of
         ResponseAccept ->
           do maybeFilePath <- fileChooserGetFilename chooser
              case maybeFilePath of
                Nothing -> return ()
                Just filePath -> setText w filePath
         _ -> return ()


chooseFile parent text =
  do d <- fileChooserDialogNew (Just "Movie File Name") (Just parent) FileChooserActionSave
            [("stockCancel", ResponseCancel), ("stockOpen", ResponseAccept)]
     fileChooserSetCurrentName d text
     widgetShow d
     response <- dialogRun d
     widgetDestroy d `after`
       case response of
         ResponseAccept -> fileChooserGetFilename d
         _ -> return Nothing



setMovieWebView wview movie =
  setText wview (renderHtml (movieToHtml movie))
