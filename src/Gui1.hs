{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Prelude hiding (catch)
import Control.Exception (catch)
import Control.Applicative ((<$>))
import Control.Monad (unless)
import Graphics.UI.Gtk
import System.Glib.GError (catchGError)
import Data.Array.IArray
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe
import Data.Char (toLower,isDigit)
import Data.List (partition)

import Parser (readCollection)





class Component a where
    getText :: a -> IO String
    getText _ = return ""
    setText :: a -> String -> IO ()
    setText _ _ = return ()

data AnyComponent = forall c . Component c => AnyComponent c

instance Component AnyComponent where
    getText (AnyComponent c) = getText c
    setText (AnyComponent c) = setText c

instance Component Entry where
    getText = entryGetText
    setText = entrySetText

instance Component Label where
    getText = labelGetText
    setText = labelSetText

instance Component TextView where
    getText x = do buffer <- textViewGetBuffer x
                   start <- textBufferGetStartIter buffer
                   end <- textBufferGetEndIter buffer
                   textBufferGetText buffer start end True
    setText x text = do buffer <- textViewGetBuffer x
                        textBufferSetText buffer text


data MyLabel = MyLabel String Label

myLabelNew prefix text pack =
    do label <- labelNewWithMnemonic (prefix ++ " " ++ text)
       miscSetAlignment label 0 0.5
       pack (toWidget label)
       return (AnyComponent (MyLabel prefix label))

instance Component MyLabel where
    getText (MyLabel prefix label) = do x <- labelGetText label
                                        return (drop (length prefix + 1) x)
    setText (MyLabel prefix label) text = labelSetText label (prefix ++ " " ++ text)

myEntryNew attributes pack =
    do entry <- entryNew
       set entry attributes
       pack (toWidget entry)
       return (AnyComponent entry)

myEntryWithSuffixNew suffix pack =
    do box <- hBoxNew False 2
       pack (toWidget box)
       entry <- entryNew
       boxPackStart box entry PackGrow 0
       label <- labelNew (Just suffix)
       boxPackStart box label PackNatural 0
       return (AnyComponent entry)

myTextViewNew width pack =
    do scrwin <- scrolledWindowNew Nothing Nothing
       scrolledWindowSetPolicy scrwin PolicyAutomatic PolicyAutomatic
       scrolledWindowSetShadowType scrwin ShadowIn
       view <- textViewNew
       containerSetBorderWidth view 2
       textViewSetWrapMode view WrapWord
       containerAdd scrwin view
       pack (toWidget scrwin)
       return (AnyComponent view)

data MyImage = MyImage Image (IORef FilePath)

myImageNew pack =
    do image <- imageNewFromIconName stockMissingImage IconSizeDialog
       pack (toWidget image)
       path <- newIORef ""
       return (AnyComponent (MyImage image path))

instance Component MyImage where
    getText (MyImage _ path) = readIORef path
    setText (MyImage image path) text =
        do writeIORef path text
           result <- myPixbufNewFromFileAtSize text 100 134
           case result of
             Just pixbuf -> imageSetFromPixbuf image pixbuf
             Nothing -> do imageSetFromIconName image stockMissingImage IconSizeDialog
                           miscSetAlignment image 0.5 0.5
                           widgetSetSizeRequest image 100 134


data MyComboBoxEntry = MyComboBoxEntry ComboBoxEntry (ListStore String)

myComboBoxEntryNew entries pack =
    do combo <- comboBoxEntryNewText
       mapM_ (comboBoxAppendText combo) entries
       model <- comboBoxGetModelText combo
       Just child <- binGetChild combo
       let entry = castToEntry child
       onEntryActivate entry $
           do str <- entryGetText entry
              strs <- listStoreToList model
              let (pos,found) = findPosition strs str 0
              unless found (listStoreInsert model pos str)
              comboBoxSetActive combo pos
       pack (toWidget combo)
       return (AnyComponent (MyComboBoxEntry combo model))

instance Component MyComboBoxEntry where
    getText (MyComboBoxEntry combo model) =
        do i <- comboBoxGetActive combo
           if i == -1
              then return ""
              else do strs <- listStoreToList model
                      return (strs !! i)
    setText (MyComboBoxEntry combo model) text =
        do strs <- listStoreToList model
           let (pos,found) = findPosition strs text 0
           unless found (listStoreInsert model pos text)
           comboBoxSetActive combo pos

findPosition [] _ i = (i,False)
findPosition (x:xs) y i | x == y = (i,True)
                        | x >  y = (i,False)
                        | otherwise = findPosition xs y (i + 1)





data Info = Id
          | Title
          | OriginalTitle
          | Country
          | Year
          | Genres
          | Director
          | Length
          | Screen
          | Format
          | IMDB
          | Subtitles
          | Audios
          | Certification
          | Theme
          | Contains
          | InclusionDate
          | Site
          | Studio
          | Distributor
          | Screenwriter
          | Producer
          | Music
          | Photography
          | ProductionDesign
          | ArtDirection
          | Figurino
          | Editor
          | SpecialEffects
          | Cast
          | Synopsis
          | Synopsis2
          | Notes
          | ForeignNotes
          | Awards
          | Curiosities
          | Comments
          | Cover
            deriving (Eq,Bounded,Ord,Ix,Show)

data DiskInfo = Source
              | Bitrate
                deriving (Eq,Bounded,Ord,Ix,Show)

data Title = T { cover :: Maybe Pixbuf
               , info :: Array Info String
               , disks :: [Array DiskInfo String]
               }
             --deriving (Show)


data ContainerGroup = GrA | GrB | GrC
                      deriving (Eq,Show)


infoBounds = (minBound::Info,maxBound::Info)

diskInfoBounds = (minBound::DiskInfo,maxBound::DiskInfo)


createMenuBar descr
    = do bar <- menuBarNew
         mapM_ (createMenu bar) descr
         return bar
    where
      createMenu bar (name,items)
          = do menu <- menuNew
               item <- menuItemNewWithLabelOrMnemonic name
               menuItemSetSubmenu item menu
               menuShellAppend bar item
               mapM_ (createMenuItem menu) items
      createMenuItem menu (name,action)
          = do item <- menuItemNewWithLabelOrMnemonic name
               menuShellAppend menu item
               case action of
                 Just act -> onActivateLeaf item act
                 Nothing  -> onActivateLeaf item (return ())
      menuItemNewWithLabelOrMnemonic name
          | elem '_' name = menuItemNewWithMnemonic name
          | otherwise     = menuItemNewWithLabel name

menuBarDescr window model view =
    [ ("_File", [ ("_Open", Just (openCollection window model view))
                , ("_Save", Nothing)
                , ("_Quit", Just mainQuit)
                ]
      )
    , ("_Collection", [ ("_New title", Just (newTitle window model view))
                      ]
      )
    -- , ("_Help",  [ ("_Help", Nothing)
    --              ]
    --   )
    ]


openCollection window model view =
    do dia <- fileChooserDialogNew (Just "Open Collection")
                                   (Just window)
                                   FileChooserActionOpen
                                   [(stockOpen, ResponseOk),
                                    (stockCancel, ResponseCancel)]
       widgetShow dia
       response <- dialogRun dia
       case response of
         ResponseOk ->
             do file <- fileChooserGetFilename dia
                case file of
                  Just fpath ->
                      do (_,_,_,ts,_) <- readCollection fpath
                         listStoreClear model
                         mapM_ (getValue model) ts
                         treeViewSetCursor view [0] Nothing
                         return ()
                  _ -> return ()
         _ -> return ()
       widgetDestroy dia

myPixbufNewFromFileAtSize filePath width height =
    catchGError (Just <$> pixbufNewFromFileAtSize filePath width height)
          (\_ -> return Nothing)



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


getValue model (infoFields:diskFields) =
    do let coverPath = titleInfo ! Cover
       cover <- myPixbufNewFromFileAtSize coverPath 64 64
       listStoreAppend model $ T { cover = cover, info = titleInfo, disks = diskInfo }
    where
      collectInfo bounds xs =
          goCollectInfo (range bounds) []
          where
            goCollectInfo [] ys = array bounds ys
            goCollectInfo (i:is) ys = case lookup (map toLower (show i)) xs of
                                        Just v -> goCollectInfo is ((i,v):ys)
                                        Nothing -> goCollectInfo is ((i,""):ys)

      titleInfo = collectInfo infoBounds infoFields
      diskInfo = map (collectInfo diskInfoBounds) diskFields


lookupKey key xs =
    goLookupKey xs []
    where
      goLookupKey [] missing = Nothing
      goLookupKey (x@(k,v):rest) ys | key == k  = Just (v, reverse ys ++ rest)
                                    | otherwise = goLookupKey rest (x:ys)


emptyTitle = T { cover = Nothing
               , info = listArray infoBounds (repeat "") // [ (Id,"9999") ]
               , disks = []
               }

newTitle window model view =
    do listStoreAppend model emptyTitle
       lastRow
    where
      lastRow = do iter <- treeModelGetIterFirst model
                   goToLastRow iter iter
      goToLastRow iter1 (Just iter2) = treeModelIterNext model iter2 >>= goToLastRow (Just iter2)
      goToLastRow (Just iter1) Nothing = do path <- treeModelGetPath model iter1
                                            treeViewSetCursor view path Nothing
      goToLastRow Nothing Nothing = return ()

addColumn view model title renderer f = do
  c <- treeViewColumnNew
  treeViewColumnSetTitle c title
  r <- renderer
  cellLayoutPackStart c r True
  cellLayoutSetAttributes c r model f
  treeViewAppendColumn view c

main =
    do initGUI

       window <- windowNew
       windowSetTitle window "Movie Collection"
       windowSetDefaultSize window 600 (-1)
       onDestroy window mainQuit

       mainBox <- vBoxNew False 0
       containerAdd window mainBox

       paned <- hPanedNew
       containerSetBorderWidth paned 5
       panedSetPosition paned 300
       boxPackEnd mainBox paned PackGrow 0

       scrwin <- scrolledWindowNew Nothing Nothing
       scrolledWindowSetPolicy scrwin PolicyAutomatic PolicyAutomatic
       scrolledWindowSetShadowType scrwin ShadowIn
       panedPack1 paned scrwin False False
       --boxPackEnd mainBox scrwin PackNatural 0

       model <- listStoreNew [ emptyTitle ]
       view <- treeViewNewWithModel model
       treeViewSetHeadersVisible view True
       addColumn view model "Pic"   cellRendererPixbufNew $ \row -> [ case cover row of
                                                                        Just p -> cellPixbuf := p
                                                                        Nothing -> cellPixbufStockId := stockMissingImage
                                                                    ]
       addColumn view model "Id"    cellRendererTextNew   $ \row -> [ cellText   :=  info row ! Id ]
       addColumn view model "Title" cellRendererTextNew   $ \row -> [ cellText   :=  info row ! Title ]
       addColumn view model "Original title" cellRendererTextNew   $ \row -> [ cellText   :=  info row ! OriginalTitle ]
       addColumn view model "Year"  cellRendererTextNew   $ \row -> [ cellText   :=  info row ! Year ]
       --scrolledWindowAddWithViewport scrwin view
       containerAdd scrwin view
       --panedAdd1 paned view
       --boxPackEnd mainBox view PackNatural 0

       components <- drawDetails (\w -> panedPack2 paned w True False)

       currentPos <- newIORef (0::Int)

       on view cursorChanged $
         do oldPos <- readIORef currentPos
            oldRow <- listStoreGetValue model oldPos
            newRow <- getDetails components oldRow
            listStoreSetValue model oldPos newRow
            ([pos],_) <- treeViewGetCursor view
            writeIORef currentPos pos
            row <- listStoreGetValue model pos
            showDetails components row
            return ()

       menuBar <- createMenuBar (menuBarDescr window model view)
       boxPackStart mainBox menuBar PackNatural 0

       widgetShowAll window
       mainGUI

drawDetails :: (Widget -> IO ()) -> IO (Array Info AnyComponent,Notebook)
drawDetails pack =
    do box <- vBoxNew False 2
       pack (toWidget box)
       -- title, year and cast on top box
       -- other info on medium box
       -- disks info on bottom box
       boxT <- hBoxNew False 2
       boxPackStart box boxT PackGrow 0
       boxB <- hBoxNew False 2
       boxPackStart box boxB PackGrow 0
       -- boxD <- hBoxNew False 2
       -- boxPackStart box boxD PackGrow 0
       disksNtbk <- notebookNew
       boxPackStart box disksNtbk PackGrow 2
       -- for the title, original title, ...
       tableT <- tableNew 2 2 False
       containerSetBorderWidth tableT 2
       boxPackStart boxT tableT PackGrow 0
       -- for the cast
       boxCast <- vBoxNew False 2
       widgetSetSizeRequest boxCast 300 100
       containerSetBorderWidth boxCast 2
       boxPackStart boxT boxCast PackGrow 0
       labelCast <- labelNew (Just "Cast")
       miscSetAlignment labelCast 0 0.5
       boxPackStart boxCast labelCast PackNatural 0
       boxPackStart boxT boxCast PackGrow 2
       -- for the cover
       frame <- frameNew
       containerSetBorderWidth frame 2
       boxPackStart boxT frame PackNatural 0
       --
       boxBL <- vBoxNew False 2
       boxPackStart boxB boxBL PackGrow 0
       boxBR <- vBoxNew False 2
       boxPackStart boxB boxBR PackGrow 0
       -- for the title and id only
       tBox <- hBoxNew False 2
       boxPackStart boxBL tBox PackGrow 2
       -- for the rest of the main info
       boxA <- hBoxNew False 2
       boxPackStart boxBL boxA PackGrow 0
       -- for other less important info
       detailsNtbk <- notebookNew
       boxPackStart boxBL detailsNtbk PackGrow 2
       tableB <- tableNew 2 2 False
       containerSetBorderWidth tableB 2
       notebookAppendPage detailsNtbk tableB "Technique Details"
       tableC <- tableNew 2 2 False
       containerSetBorderWidth tableC 2
       notebookAppendPage detailsNtbk tableC "More Details"
       -- sinopsys, notes
       synNtbk <- notebookNew
       containerSetBorderWidth synNtbk 2
       boxPackStart boxBR synNtbk PackGrow 2
       notesNtbk <- notebookNew
       containerSetBorderWidth notesNtbk 2
       boxPackStart boxBR notesNtbk PackGrow 2

       let componentDescriptions = (array infoBounds
             [
               (Cover           , myImageNew                            (containerAdd frame))
             , (Id              , myLabelNew "Title" "0"                (\w -> tableAttachDefaults tableT w 0 1 0 1))
             , (Title           , myEntryNew [ entryWidthChars := 40 ]  (\w -> do f <- fontDescriptionNew
                                                                                  fontDescriptionSetWeight f WeightBold
                                                                                  widgetModifyFont w (Just f)
                                                                                  tableAttachDefaults tableT w 1 2 0 1))
             , (OriginalTitle   , myEntryNew []                         (packInTable tableT 1 "Original title"))
             , (Country         , myEntryNew []                         (packInTable tableT 2 "Country"))
             , (Year            , myEntryNew [ entryWidthChars := 10 ]  (packInTable tableT 3 "Production year"))
             , (Genres          , myEntryNew []                         (packInTable tableB 0 "Genres"))
             , (Director        , myEntryNew []                         (packInTable tableB 1 "Director"))
             , (Length          , myEntryWithSuffixNew "min"            (packInTable tableB 2 "Length"))
             , (Screen          , myComboBoxEntryNew ["Fullscreen", "Widescreen"] (packInTable tableB 3 "Screen"))
             , (Format          , myComboBoxEntryNew ["NTSC", "PAL"]    (packInTable tableB 4 "Format"))
             , (IMDB            , myEntryNew []                         (packInTable tableB 5 "Link IMDB"))
             , (Subtitles       , myEntryNew []                         (packInTable tableB 6 "Subtitles"))
             , (Audios          , myEntryNew []                         (packInTable tableB 7 "Audios"))
             , (Certification   , myComboBoxEntryNew ["10 anos", "12 anos", "14 anos", "16 anos", "Parental" ] (packInTable tableB 8 "Certification"))
             , (Theme           , myEntryNew []                         (packInTable tableB 9 "Theme"))
             , (Contains        , myEntryNew []                         (packInTable tableB 10 "Contains"))
             , (InclusionDate   , myEntryNew [ entryEditable := False, entryWidthChars := 10 ] (packInTable tableC 0 "Inclusion date"))
             , (Site            , myEntryNew []                         (packInTable tableC 1 "Site"))
             , (Studio          , myEntryNew []                         (packInTable tableC 2 "Studio"))
             , (Distributor     , myEntryNew []                         (packInTable tableC 3 "Distributor"))
             , (Screenwriter    , myEntryNew []                         (packInTable tableC 4 "Screenwriter"))
             , (Producer        , myEntryNew []                         (packInTable tableC 5 "Producer"))
             , (Music           , myEntryNew []                         (packInTable tableC 6 "Music"))
             , (Photography     , myEntryNew []                         (packInTable tableC 7 "Photography"))
             , (ProductionDesign, myEntryNew []                         (packInTable tableC 8 "Production design"))
             , (ArtDirection    , myEntryNew []                         (packInTable tableC 9 "Art direction"))
             , (Figurino        , myEntryNew []                         (packInTable tableC 10 "Figurino"))
             , (Editor          , myEntryNew []                         (packInTable tableC 11 "Editor"))
             , (SpecialEffects  , myEntryNew []                         (packInTable tableC 12 "Special effects"))
             , (Cast            , myTextViewNew 40                      (packInBox boxCast))
             , (Synopsis        , myTextViewNew 40                      (\w -> notebookAppendPage synNtbk w "Synopsys"))
             , (Synopsis2       , myTextViewNew 40                      (\w -> notebookAppendPage synNtbk w "Synopsys (en)"))
             , (Notes           , myTextViewNew 40                      (\w -> notebookAppendPage notesNtbk w "Notes"))
             , (ForeignNotes    , myTextViewNew 40                      (\w -> notebookAppendPage notesNtbk w "Foreign notes"))
             , (Awards          , myTextViewNew 40                      (\w -> notebookAppendPage notesNtbk w "Awards"))
             , (Curiosities     , myTextViewNew 40                      (\w -> notebookAppendPage notesNtbk w "Curiosities"))
             , (Comments        , myTextViewNew 40                      (\w -> notebookAppendPage notesNtbk w "Comments"))
--             , (Source 1        , myEntryNew []                         (packInTable tableC 12 "Source disk 1"))
             ]) :: Array Info (IO AnyComponent)
       components <- goDrawDetails componentDescriptions [] (range infoBounds)
       return (components,disksNtbk)
    where
      goDrawDetails t components [] = return (array infoBounds components)
      goDrawDetails t components (i:is) =
          do x <- t ! i
             goDrawDetails t ((i,x):components) is

myLabelNewInTable label table column row =
    do label <- labelNewWithMnemonic label
       miscSetAlignment label 0 0.5
       tableAttach table label column (column + 1) row (row + 1) [Fill] [Fill] 5 2

packInContainer container row label widget =
    containerAdd container widget

packInBox box widget =
    boxPackStart box widget PackGrow 0

packInBoxWithLabel box label widget =
    do l <- labelNewWithMnemonic label
       miscSetAlignment l 0 0.5
       boxPackStart box l PackNatural 0
       boxPackStart box widget PackGrow 2

packInTable table row label widget =
    do if label /= ""
          then do l <- labelNewWithMnemonic label
                  miscSetAlignment l 0 0.5
                  tableAttach table l 0 1 row (row + 1) [Fill] [Fill] 2 0
          else return ()
       tableAttachDefaults table widget 1 2 row (row + 1)

packInTableV table col label widget =
    do if label /= ""
          then do l <- labelNewWithMnemonic label
                  miscSetAlignment l 0 0.5
                  tableAttach table l col (col+1) 0 1 [Fill] [Fill] 0 0
          else return ()
       tableAttach table widget col (col+1) 1 2 [Fill] [Fill] 0 5

myTableAttachDefaults table col1 col2 row1 row2 widget =
    tableAttachDefaults table (toWidget widget) col1 col2 row1 row2


showDetails :: (Array Info AnyComponent,Notebook) -> Title -> IO ()
showDetails (components,_) value =
    mapM_ (\i -> setText (components ! i) (info value ! i))
          (range infoBounds)

getDetails :: (Array Info AnyComponent,Notebook) -> Title -> IO Title
getDetails (components,_) value =
    goGetDetails (range infoBounds) []
    where
      goGetDetails [] xs = return (value { info = array infoBounds (reverse xs) })
      goGetDetails (i:is) xs =
          do x <- getText (components ! i)
             goGetDetails is ((i,x):xs)
