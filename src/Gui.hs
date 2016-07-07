{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad (when, unless, forM, forM_)
import Graphics.UI.Gtk hiding (Release, after)
import Data.Array.IArray ((!), (//))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Char (isDigit)
import Data.List (partition, sort, union, unionBy, stripPrefix, sortBy)
import System.Environment (getArgs)
import System.FilePath (replaceFileName)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Process (runProcess)
import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))
import Text.Printf (printf)
import qualified Data.Text as T

import Util (trim, errHead)
import Parser (readCollection)
import Writer (writeCollection)
import Movie
import Component
import Comments (drawComments)
import Latex (latex)
import Filter (filterMovies)
import MovieForm (movieFormNew, movieFormGetMovie, movieFormSetMovie)
import Catalog (catalog)
import Prefs (Prefs(Prefs,player),defaultPrefs, readPreferences, savePreferences, prefsSetup)
import IMDB (imdb)
import AdoroCinema (adoroCinema)
import EPipoca (ePipoca)
import Interfilmes (interFilmes)
import DVDWorld (dvdWorld)
import AllCenter (allCenter)
import VideoNorte (videoNorte)
import CapasBrasil (capasbrasil)
import Manicomio (manicomio)
import MovieHtml (collectionToHtml)


menuBarDescr window model fModel visCol view movieForm prevPathRef colRef filterCriteriaRef prefsRef =
  [
    (MIMnemonic "_File",
     [ (MIStock stockNew,      Just $ newCollection window model view movieForm colRef)
     , (MIMnemonic "_Catalog", Just $ catalog window model view movieForm colRef prefsRef)
     , (MIStock stockOpen,     Just $ openCollection window model view movieForm colRef)
     , (MIStock stockSave,     Just $ saveCollection window model fModel view movieForm colRef)
     , (MIStock stockSaveAs,   Just $ saveAsCollection window model fModel view movieForm colRef)
     , (MIStock stockQuit,     Just $ quit window prefsRef)
     ])
  ,
    (MIMnemonic "_Movies",
     [ (MIMnemonic "_New Movie",                       Just $ newMovie window model fModel view movieForm colRef)
     , (MIMnemonic "_Delete Movie",                    Just $ delMovie window model fModel view movieForm prevPathRef)
     , (MIMnemonic "_Search Directory For New Movies", Just $ searchDirForNewMovies window model view colRef)
     , (MIMnemonic "_Filter",                          Just $ filterMovies window model fModel visCol view filterCriteriaRef)
     , (MIMnemonic "_Adapt Directory",                 Just $ dirAdapt window model fModel view prevPathRef)
     , (MIMnemonic "_Play",                            Just $ playMovie window movieForm prefsRef)
     ])
  , (MIMnemonic "_Tools",
     [ (MIMnemonic "_Html",        Just $ saveHtml window model view movieForm colRef)
     , (MIMnemonic "_LaTeX",       Just $ titleToLatex window model view movieForm colRef)
     , (MIMnemonic "_Preferences", Just $ prefsSetup window prefsRef)
     ])
  , (MIMnemonic "_Help",
     [ (MIStock stockAbout, Just $ showAboutDialog window)
     ])
  ]




newCollection parent model view movieForm colRef =
  do let title = emptyMovie 1
     writeIORef colRef emptyCol
       { genres = defaultGenres
       , languages = defaultLanguages
       , audiocompressions = defaultAudioCompressions
       , titles = [ title ]
       , greatestMovieId = 1
       }
     movieFormSetMovie movieForm title
     listStoreClear model
     listStoreAppend model title
     treeViewSetCursor view [0] Nothing


openCollection window model view movieForm colRef =
  do dia <- fileChooserDialogNew (Just "Open Collection")
                                 (Just window)
                                 FileChooserActionOpen
                                 [("stockOpen", ResponseOk),
                                  ("stockCancel", ResponseCancel)]
     widgetShow dia
     response <- dialogRun dia
     case response of
       ResponseOk ->
         do file <- fileChooserGetFilename dia
            case file of
              Just fpath -> openCollectionFile window fpath model view movieForm colRef
              _          -> return ()
       _ -> return ()
     widgetDestroy dia

openCollectionFile parent fpath model view movieForm colRef =
  do res <- readCollection fpath
     case res of
       Left msg ->
         errorMsg parent msg
       Right Col{genres,languages,audiocompressions,titles} ->
         do titles' <- forM titles $ \t@M{info} ->
                         do cover <- myPixbufNewFromFileAtSize (info ! Cover) 64 64
                            return t{cover}
            let sTitles = sortBy (\x y -> compare (info x ! Title) (info y ! Title)) titles'
            listStoreClear model
            mapM_ (listStoreAppend model) sTitles
            case sTitles of
              title:_ -> do movieFormSetMovie movieForm title
                            treeViewSetCursor view [0] Nothing
              _ -> return ()
            imgDirPath <- do let imgPath = replaceFileName fpath "images"
                             createDirectoryIfMissing True imgPath
                             return imgPath
            writeIORef colRef Col
              { genres = sort $ unionBy fstEq genres defaultGenres
              , languages = sort $ unionBy fstEq languages defaultLanguages
              , audiocompressions = sort $ union audiocompressions defaultAudioCompressions
              , titles = sTitles
              , greatestMovieId = foldl (\g M{ident} -> max g ident) 0 sTitles
              , imageDir = Just imgDirPath
              , filePath = Just fpath
              }

fstEq (x,_) (y,_) = x == y

splitFieldList xs =
    goSplitFieldList xs [] []
    where
      goSplitFieldList [] a b = (a,b)
      goSplitFieldList (x@(key,val):xs) a b =
          case indexedKey key of
            Just (k,n) -> goSplitFieldList xs a (addIndexedKeyToList k n val b)
            Nothing -> goSplitFieldList xs (x:a) b

indexedKey key =
    let (k1,k2) = partition isDigit (reverse key)
    in if null k1 || null k2 then Nothing
       else Just (reverse k2,(read (reverse k1))::Int)

addIndexedKeyToList k n v xs =
    go xs []
    where
      go [] [] = [(i,[])|i<-[1..n-1]] ++ [(n,[(k,v)])]
      go [] ys@((n',_):_) = reverse ys ++ [(i,[])|i<-[n'+1..n-1]] ++ [(n,[(k,v)])]
      go xs@(x@(n',vs):xs') ys
          | n < n'    = reverse ys ++ (n,[(k,v)]):([(i,[])|i<-[n+1..n'-1]] ++ xs)
          | n == n'   = reverse ys ++ (n,(k,v):vs):xs'
          | otherwise = go xs' (x:ys)




saveAsCollection window model fModel view movieForm colRef =
  do dia <- fileChooserDialogNew (Just "Save Collection")
                                 (Just window)
                                 FileChooserActionSave
                                 [ ("stockCancel", ResponseCancel)
                                 , ("stockSave", ResponseAccept)
                                 ]
     fileChooserSetDoOverwriteConfirmation dia True
     widgetShow dia
     response <- dialogRun dia
     case response of
       ResponseAccept ->
         do file <- fileChooserGetFilename dia
            case file of
              Just fpath -> saveCollectionFile fpath model fModel view movieForm colRef
              _          -> return ()
       _ -> return ()
     widgetDestroy dia

saveCollectionFile fpath model fModel view movieForm colRef =
  do updateColModel model fModel view movieForm
     titles <- listStoreToList model
     col <- readIORef colRef
     writeCollection fpath col{titles}

saveCollection window model fModel view movieForm colRef =
  do Col{filePath} <- readIORef colRef
     case filePath of
       Just fpath -> saveCollectionFile fpath model fModel view movieForm colRef
       Nothing -> saveAsCollection window model fModel view movieForm colRef


newMovie window model fModel view movieForm colRef =
  do updateColModel model fModel view movieForm
     status@Col{greatestMovieId} <- readIORef colRef
     writeIORef colRef status{greatestMovieId = greatestMovieId+1}
     listStoreAppend model (emptyMovie (greatestMovieId+1))
     lastRow
  where
    lastRow = do iter <- treeModelGetIterFirst model
                 goToLastRow iter iter
    goToLastRow iter1 (Just iter2) = treeModelIterNext model iter2 >>= goToLastRow (Just iter2)
    goToLastRow (Just iter1) Nothing = do path <- treeModelGetPath model iter1
                                          treeViewSetCursor view path Nothing
    goToLastRow Nothing Nothing = return ()


delMovie parent model fModel view movieForm prevPathRef =
  do M{info} <- movieFormGetMovie movieForm (emptyMovie 1)
     res <- askMsg parent $ printf "Really Delete Movie\n\"%s\"?" (info!Title)
     when res $
       do (filterPath,_) <- treeViewGetCursor view
          case filterPath of
            [i] -> do [j] <- treeModelFilterConvertPathToChildPath fModel filterPath
                      listStoreRemove model j
                      writeIORef prevPathRef []
                      n <- treeModelGetSize fModel
                      treeViewSetCursor view (if i == n then [i-1] else [i]) Nothing
            _ -> errorMsg parent "Movie not found! (BUG)"


treeModelGetSize model =
  do maybeIter <- treeModelGetIterFirst model
     loop 0 maybeIter
  where
    loop n (Just iter) = do maybeIter <- treeModelIterNext model iter
                            loop (n+1) maybeIter
    loop n Nothing = return n

searchDirForNewMovies parent model view colRef =
  errorMsg parent "Not yet implemented!"



saveHtml parent model view movieForm colRef =
  do -- FIXME: save current title
     col <- readIORef colRef
     let doc = collectionToHtml col
     d <- fileChooserDialogNew
            (Just "Save Html Document")
            (Just parent)
            FileChooserActionSave
            [ ("stockCancel", ResponseCancel), ("stockSave", ResponseAccept) ]
     fileChooserSetDoOverwriteConfirmation d True
     widgetShow d
     response <- dialogRun d
     when (response == ResponseAccept) $
       do mFile <- fileChooserGetFilename d
          maybe (return ()) (\fpath -> writeFile fpath doc) mFile
     widgetDestroy d


titleToLatex parent model view movieForm colRef =
  do movie <- movieFormGetMovie movieForm (emptyMovie 1)
     let doc = latex movie
     d <- fileChooserDialogNew
            (Just "Save LaTeX Document")
            (Just parent)
            FileChooserActionSave
            [ ("stockCancel", ResponseCancel), ("stockSave", ResponseAccept) ]
     fileChooserSetDoOverwriteConfirmation d True
     widgetShow d
     response <- dialogRun d
     when (response == ResponseAccept) $
       do mFile <- fileChooserGetFilename d
          maybe (return ()) (\fpath -> writeFile fpath doc) mFile
     widgetDestroy d

playMovie parent movieForm prefsRef =
  do prefs@Prefs{player} <- readIORef prefsRef
     movie <- movieFormGetMovie movieForm (emptyMovie 1)
     let filePath = info movie ! File
     exists <- doesFileExist filePath
     if exists
     then if null player
          then errorMsg parent "Player not defined in preferences"
          else runProcess player [filePath] Nothing Nothing Nothing Nothing Nothing >> return ()
     else errorMsg parent "File does not exist"


dirAdapt parent model fModel view prevPathRef =
  do d <- dialogNew
     set d [windowTitle := "Adapt Directory", windowTransientFor := parent]
     dialogAddButton d "stockCancel" ResponseReject
     ok <- dialogAddButton d "stockOk" ResponseAccept
     widgetGrabDefault ok
     u <- dialogGetUpper d
     --
     t <- mkWA (tableNew 0 2 False) (myBoxPackStart u PackGrow 0) [containerBorderWidth := 2]
     curEntry <- mkW entryNew (packInTableD t 0 0 2 "_Current Directory")
     newEntry <- mkW entryNew (packInTableD t 1 0 2 "_New Directory")
     --
     widgetShowAll d
     resp <- dialogRun d
     when (resp == ResponseAccept) $
       do curDir <- fmap trim $ entryGetText curEntry
          newDir <- fmap trim $ entryGetText newEntry
          (fPath,_) <- treeViewGetCursor view
          treeViewSetCursor view fPath Nothing
          n <- treeModelGetSize fModel
          forM_ [0::Int .. n-1] $ \i ->
            do [j] <- treeModelFilterConvertPathToChildPath fModel [i]
               print j
               movie@M{info} <- listStoreGetValue model j
               let curFile = info ! File
               case stripPrefix curDir curFile of
                 Nothing -> return ()
                 Just x -> listStoreSetValue model j movie{info=info//[(File,newDir ++ x)]}
          writeIORef prevPathRef []
          treeViewSetCursor view fPath Nothing
     widgetDestroy d



quit parent prefsRef =
  readIORef prefsRef >>= savePreferences parent >> mainQuit



updateColModel model fModel view movieForm =
  do (filterPath,_) <- treeViewGetCursor view
     path <- treeModelFilterConvertPathToChildPath fModel filterPath
     case path of
       [i] -> do movie1 <- listStoreGetValue model i
                 movie2 <- movieFormGetMovie movieForm movie1
                 listStoreSetValue model i movie2


visCol :: ColumnId Movie Bool
visCol = makeColumnIdBool 0


main =
  do hSetBuffering stdout NoBuffering

     unsafeInitGUIForThreadedRTS

     win <- windowNew
     windowSetTitle win "Movie Collection Management"
     -- windowSetDefaultSize win 600 (-1)

     prefsRef <- newIORef defaultPrefs
     readPreferences win [imdb,adoroCinema,ePipoca,interFilmes,dvdWorld,allCenter,videoNorte,capasbrasil,manicomio] >>=
       writeIORef prefsRef

     onDestroy win (quit win prefsRef)

     mainBox <- vBoxNew False 0
     containerAdd win mainBox

     paned <- hPanedNew
     containerSetBorderWidth paned 5
     panedSetPosition paned 300
     boxPackEnd mainBox paned PackGrow 0

     sw <- scrolledWindowNew Nothing Nothing
     scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
     scrolledWindowSetShadowType sw ShadowIn
     widgetSetSizeRequest sw 250 100
     panedPack1 paned sw False False

     model <- listStoreNew []
     customStoreSetColumn model visCol (const True)
     fModel <- treeModelFilterNew model []
     treeModelFilterSetVisibleColumn fModel visCol
     view <- mkWA (treeViewNewWithModel fModel) (containerAdd sw) [treeViewHeadersVisible := False]

     mkCol view model "Pic" cellRendererPixbufNew [] $ \M{cover} ->
       [ maybe (cellPixbufStockId := stockMissingImage) (cellPixbuf :=) cover ]
     mkCol view model "Title" cellRendererTextNew [] $ \M{info}  ->
       [ cellTextMarkup := Just $ T.pack $ printf "<b>%s</b>\n%s\n%s %s"
                                         (escapeMarkup (info ! Title))
                                         (escapeMarkup (info ! OriginalTitle))
                                         (escapeMarkup (info ! Year))
                                         (escapeMarkup (info ! Country)) ]

     colRef <- newIORef emptyCol

     movieForm <- movieFormNew win colRef prefsRef $ \w -> panedPack2 paned w True False

     -- an IORef to keep the current filter criteria
     filterCriteriaRef <- newIORef []

     -- an IORef to keep the path of the previous cursor on the tree
     -- view; it is needed to save data from the movie form into the
     -- model when navigating in the view
     prevPathRef <- newIORef []

     menuBar <- createMenuBar (menuBarDescr win model fModel visCol view movieForm prevPathRef colRef filterCriteriaRef prefsRef)
     boxPackStart mainBox menuBar PackNatural 0

     on view cursorChanged $
       do prevPath <- readIORef prevPathRef
          unless (null prevPath) $
            do prevRow <- listStoreGetValue model (errHead "A" prevPath)
               prevRow' <- movieFormGetMovie movieForm prevRow
               listStoreSetValue model (errHead "B" prevPath) prevRow'
          (filterPath,_) <- treeViewGetCursor view
          path <- treeModelFilterConvertPathToChildPath fModel filterPath
          writeIORef prevPathRef path
          if null path
          then movieFormSetMovie movieForm (emptyMovie 0)
          else do row <- listStoreGetValue model (errHead "C" path)
                  movieFormSetMovie movieForm row

     -- initialize collection with file given in the command line
     args <- getArgs
     case args of
       [fpath] -> openCollectionFile win fpath model view movieForm colRef
       _       -> return ()

     widgetShowAll win
     mainGUI




showAboutDialog window =
  do logo <- pixbufNewFromFileAtSize "dvdseeders.png" 128 128
     d <- aboutDialogNew
     set d [ windowTransientFor     := window
           , aboutDialogName        := "Movie Collection Manager"
           , aboutDialogVersion     := "0.1"
           , aboutDialogCopyright   := "(C) 2009, 2012 José Romildo Malaquias"
           , aboutDialogComments    := "DVDSeeders Movie Collection Manager"
           , aboutDialogLicense     := Just "Free to all"
           , aboutDialogWrapLicense := True
           , aboutDialogAuthors     := [ "José Romildo Malaquias <romildo@iceb.ufop.br>" ]
           , aboutDialogLogo        := Just logo
           ]
     dialogRun d
     widgetDestroy d


