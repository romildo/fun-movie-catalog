{-# LANGUAGE ExistentialQuantification #-}

-- | A set of definitions dealing with widget components
module Component where

import Data.Maybe (fromMaybe)
import Control.Monad (when, unless, forM, forM_)
import Control.Applicative ((<$>))
import Data.List.Utils (split,join)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Glib.GError (GError)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView (WebView, webViewNew, webViewGetMainFrame, webViewLoadHtmlString)
import Graphics.UI.Gtk.WebKit.WebDataSource (webDataSourceGetData)
import Graphics.UI.Gtk.WebKit.WebFrame (webFrameGetDataSource)
import Control.Exception (try)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

import Util (trim,findPosition)


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


instance Component WebView where
    getText x = do f <- webViewGetMainFrame x
                   src <- webFrameGetDataSource f
                   fromMaybe "" <$> fmap B.unpack <$> webDataSourceGetData src
    setText x text = webViewLoadHtmlString x text ""


data MyTextView = MyTextView TextView TextBuffer

instance Component MyTextView where
    getText (MyTextView _ textBuffer) = do start <- textBufferGetStartIter textBuffer
                                           end <- textBufferGetEndIter textBuffer
                                           textBufferGetText textBuffer start end True
    setText (MyTextView _ textBuffer) text = textBufferSetText textBuffer text


data MyLabel = MyLabel Label

myLabelNew text attrs pack =
    do label <- labelNewWithMnemonic text
       set label attrs
       pack (toWidget label)
       return (AnyComponent (MyLabel label))

instance Component MyLabel where
    getText (MyLabel label) = labelGetText label
    setText (MyLabel label) text = labelSetText label text

myEntryNew attributes pack =
    do entry <- entryNew
       set entry attributes
       pack (toWidget entry)
       return (AnyComponent entry)

myEntryNewWithText attributes text pack =
    do entry <- entryNew
       set entry attributes
       entrySetText entry text
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

myEntryWithButtonNew action pack =
    do box <- hBoxNew False 2
       pack (toWidget box)
       entry <- entryNew
       boxPackStart box entry PackGrow 0
       button <- buttonNew
       image <- imageNewFromIconName stockEdit IconSizeMenu
       buttonSetImage button image
       boxPackStart box button PackNatural 0
       onClicked button $
         do text <- entryGetText entry
            result <- action (text :: String)
            case result of
              Just newText -> entrySetText entry (newText :: String)
              _ -> return ()
       return (AnyComponent entry)

textViewNewInScrolledWindow width height text pack =
  do scrwin <- scrolledWindowNew Nothing Nothing
     pack scrwin
     scrolledWindowSetPolicy scrwin PolicyAutomatic PolicyAutomatic
     scrolledWindowSetShadowType scrwin ShadowIn
     view <- textViewNew
     containerSetBorderWidth view 2
     textViewSetWrapMode view WrapWord
     when (width >= 0 || height >= 0) $
       widgetSetSizeRequest view width height
     textBuffer <- textViewGetBuffer view
     textBufferSetText textBuffer text
     containerAdd scrwin view
     return (view,textBuffer)

myTextViewNew width height text pack =
  do (view,textBuffer) <- textViewNewInScrolledWindow width height text (pack . toWidget)
     return (AnyComponent (MyTextView view textBuffer))

-- webview
myWebViewNew attributes pack =
    do webView <- webViewNew
       set webView attributes
       pack (toWidget webView)
       return (AnyComponent webView)


data MyImage = MyImage Image (IORef FilePath)

myImageNew pack =
  do image <- imageNewFromIconName stockMissingImage IconSizeDialog
     pack (toWidget image)
     path <- newIORef ""
     set image [widgetTooltipText := Just "(unknown)"]
     return (AnyComponent (MyImage image path))

instance Component MyImage where
  getText (MyImage _ path) = readIORef path
  setText (MyImage image path) text =
    do writeIORef path text
       result <- myPixbufNewFromFileAtSize text 141 188
       case result of
         Just pixbuf ->
           do imageSetFromPixbuf image pixbuf
              set image [widgetTooltipText := Just text]
         Nothing ->
           do imageSetFromIconName image stockMissingImage IconSizeDialog
              miscSetAlignment image 0.5 0.5
              widgetSetSizeRequest image 100 134
              set image [widgetTooltipText := Just "(unknown)"]

myPixbufNewFromFileAtSize "" _ _ = return Nothing
myPixbufNewFromFileAtSize filePath width height =
  do tried <- try (pixbufNewFromFileAtSize filePath width height)
     case tried of
       Left ex ->
         do putStrLn ("\nEXCEPTION: " ++ show (ex :: GError))
            return Nothing
       Right pb ->
         return (Just pb)


-- NEEDS REVIEW: ComboBoxText OR String?
data MyComboBoxEntry = MyComboBoxEntry ComboBoxEntry (ListStore ComboBoxText)

myComboBoxEntryNew entries' text' pack =
    do let text = T.pack text'
       let entries = map T.pack entries'
       combo <- comboBoxEntryNewText
       mapM_ (comboBoxAppendText combo) entries
       model <- comboBoxGetModelText combo
       Just child <- binGetChild combo
       let entry = castToEntry child
       entrySetWidthChars
         entry
         (round (0.9 * (fromIntegral (maximum (map length (text' : entries')) + 1))))
       onEntryActivate entry $
           do str <- entryGetText entry
              strs <- listStoreToList model
              let (pos,found) = findPosition strs str 0
              unless found (listStoreInsert model pos str)
              comboBoxSetActive combo pos
       pack (toWidget combo)
       let w =  MyComboBoxEntry combo model
       setText w (T.unpack text)
       return (AnyComponent w)

instance Component MyComboBoxEntry where
    getText (MyComboBoxEntry combo model) =
        do i <- comboBoxGetActive combo
           if i == -1
              then return ""
              else do strs <- listStoreToList model
                      return (T.unpack (strs !! i))
    setText (MyComboBoxEntry combo model) text' =
        do let text = T.pack text'
           strs <- listStoreToList model
           let (pos,found) = findPosition strs text 0
           unless found (listStoreInsert model pos text)
           comboBoxSetActive combo pos


data MyStringList = MyStringList TreeView (ListStore String)

myStringListNew label text editor pack =
    do box <- mkW (vBoxNew False 2) pack
       scrwin <- mkWA (scrolledWindowNew Nothing Nothing)
                      (myBoxPackEnd box PackGrow 0)
                      [ scrolledWindowHscrollbarPolicy := PolicyAutomatic
                      , scrolledWindowVscrollbarPolicy := PolicyAutomatic
                      , scrolledWindowShadowType       := ShadowIn
                      , containerBorderWidth           := 2
                      ]
       model <- listStoreNew []
       view <- mkWA (treeViewNewWithModel model)
                    (containerAdd scrwin)
                    [ treeViewHeadersVisible := False
                    ]
       r <- mkRendererText []
       addColumn view model "" r $ \row -> [ cellText := row ]
       box1 <- mkW (hBoxNew False 2) (myBoxPackStart box PackNatural 0)
       mkW (labelNewWithMnemonic label) (myBoxPackStart box1 PackNatural 0)
       button <- mkWA buttonNew
                      (myBoxPackStart box1 PackNatural 0)
                      [ buttonImage :=> imageNewFromIconName stockEdit IconSizeMenu ]

       onClicked button $
         do list <- listStoreToList model
            result <- editor list
            case result of
              Just newList -> do listStoreClear model
                                 mapM_ (listStoreAppend model) newList
              _ -> return ()

       return (AnyComponent (MyStringList view model))

instance Component MyStringList where
    getText (MyStringList view model) =
        do xs <- listStoreToList model
           return (join "," xs)

    setText (MyStringList view model) text =
        do listStoreClear model
           mapM_ (listStoreAppend model) (map trim (split "," text))



data MyFileChooserButton = MyFileChooserButton FileChooserButton (IORef String)

myFileChooserButtonNew title action pack =
  do b <- fileChooserButtonNew title action
     pack b
     ref <- newIORef ""
     return (AnyComponent (MyFileChooserButton b ref))

instance Component MyFileChooserButton where
  getText (MyFileChooserButton b ref) = do f <- fileChooserGetFilename b
                                           case f of
                                             Just x -> return x
                                             Nothing -> readIORef ref
  setText (MyFileChooserButton b ref) text = do fileChooserSetFilename b text
                                                writeIORef ref text





mkW :: (WidgetClass b, Monad m) =>
       m b             -- ^ A monad that makes a new widget
    -> (Widget -> m a) -- ^ A function to pack the widget in a container
    -> m b             -- ^ The result is a monad that returns the new widget
mkW newW pack =
  do x <- newW
     pack (toWidget x)
     return x

mkWA newW pack attributes =
  do x <- newW
     set x attributes
     pack (toWidget x)
     return x

{-
-- make a new widget with a label and pack them in an given box
mkWAL newW label box attributes =
  do lab <- newLabelWithMnemonic label
     boxPackStart box lab PackNatural 0
     labelSetUseMarkup lab True
     w <- newW
     boxPackStart box w PackGrow 0
     set w attributes
     labelSetMnemonicWidget lab w
     retun w

-- make a new widget with a label in a horizontal layout
mkWLh newW label pack =
  mkWALh new label pack []

-- make a new widget with a label in an vertical layout
mkWLv newW label pack =
  mkWALv new label pack []

-- make a new widget with a label in a horizontal layout and set its attributes
mkWALh newW label pack atts =
  do box <- hBoxNew False 2
     pack box
     mkWL newW label box atts

-- make a new widget with a label in a vertical layout and set its attributes
mkWALv newW label pack atts =
  do box <- vBoxNew False 2
     pack box
     mkWL newW label box atts
-}



mkEntry text pack =
    do entry <- entryNew
       entrySetText entry text
       pack (toWidget entry)
       return entry

mkTextView width height text pack =
    do scrwin <- scrolledWindowNew Nothing Nothing
       pack (toWidget scrwin)
       scrolledWindowSetPolicy scrwin PolicyAutomatic PolicyAutomatic
       scrolledWindowSetShadowType scrwin ShadowIn
       view <- textViewNew
       containerAdd scrwin view
       containerSetBorderWidth view 2
       textViewSetWrapMode view WrapWord
       widgetSetSizeRequest view width height
       textBuffer <- textViewGetBuffer view
       textBufferSetText textBuffer text
       return (view,textBuffer)




radioButtonNewWithMnemonicGroup (x:xs) =
  do r <- radioButtonNewWithMnemonic x
     rs <- mapM (radioButtonNewWithMnemonicFromWidget r) xs
     return (r:rs)
radioButtonNewWithMnemonicGroup [] =
  return []




myBoxPackStart box packing padding child =
    boxPackStart box child packing padding

myBoxPackEnd box packing padding child =
    boxPackEnd box child packing padding

myBoxPackStartWithMnemonic box packing padding mnemonic child =
  do label <- labelNewWithMnemonic mnemonic
     miscSetAlignment label 0 0.5
     labelSetMnemonicWidget label child
     boxPackStart box label PackNatural padding
     boxPackStart box child packing padding

myBoxPackEndWithMnemonic box packing padding mnemonic child =
  do label <- labelNewWithMnemonic mnemonic
     miscSetAlignment label 0 0.5
     labelSetMnemonicWidget label child
     boxPackEnd box child packing padding
     boxPackEnd box label PackNatural padding



-- like panedPack1 and panedPack2, but the child is the last argument instead of the second
myPanedPack1 paned resize shrink child = panedPack1 paned child resize shrink
myPanedPack2 paned resize shrink child = panedPack2 paned child resize shrink




myNotebookAppendPage notebook label w =
    do tabLabel <- labelNewWithMnemonic label
       menuLabel <- labelNewWithMnemonic label
       notebookAppendPageMenu notebook w tabLabel menuLabel
       return ()

myNotebookAppendPageMenu notebook label w =
    do notebookAppendPageMenu notebook w label
       return ()

myTableAttach table col1 col2 row1 row2 xoptions yoptions xpadding ypadding widget =
    tableAttach table widget col1 col2 row1 row2 xoptions yoptions xpadding ypadding

myTableAttachDefaults table col1 col2 row1 row2 widget =
    tableAttachDefaults table widget col1 col2 row1 row2

packInContainer container row label widget =
    containerAdd container widget

-- packInBox box widget =
--     boxPackStart box widget PackGrow 0

-- packInBoxWithLabel box label widget =
--   do lab <- labelNewWithMnemonic label
--      miscSetAlignment lab 0 0.5
--      labelSetMnemonicWidget lab widget
--      boxPackStart box lab PackNatural 0
--      boxPackStart box widget PackGrow 2


packInTableD table row column numColumns label widget =
    do unless (label == "") $
         do lab <- labelNewWithMnemonic label
            miscSetAlignment lab 0 0.5
            labelSetMnemonicWidget lab widget
            tableAttach table lab column (column + 1) row (row + 1) [Fill] [Fill] 2 0
       tableAttachDefaults table widget (column + 1) (column + numColumns) row (row + 1)

packInTable table row column numColumns attachx attachy "" widget =
    tableAttach table widget column (column + numColumns) row (row + 1) attachx attachy 0 0

packInTable table row column numColumns attachx attachy label widget =
    do lab <- labelNewWithMnemonic label
       miscSetAlignment lab 0 0.5
       labelSetMnemonicWidget lab widget
       tableAttach table lab column (column + 1) row (row + 1) [Fill] [Fill] 2 0
       tableAttach table widget (column + 1) (column + numColumns) row (row + 1) attachx attachy 0 0

packInTableV table row column numRows label widget =
    do unless (label == "") $
         do lab <- labelNewWithMnemonic label
            miscSetAlignment lab 0 0.5
            labelSetMnemonicWidget lab widget
            tableAttach table lab column (column + 1) row (row + 1) [Fill] [Fill] 2 0
       tableAttachDefaults table widget column (column + 1) (row + 1) (row + numRows)
















createMenuBar descr
    = do bar <- menuBarNew
         mapM_ (createMenu bar) descr
         return bar
    where
      createMenu bar (name,items)
          = do menu <- menuNew
               item <- myMenuItemNew name
               menuItemSetSubmenu item menu
               menuShellAppend bar item
               mapM_ (createMenuItem menu) items
      createMenuItem menu (name,action)
          = do item <- myMenuItemNew name
               menuShellAppend menu item
               case action of
                 Just act -> onActivateLeaf item act
                 Nothing  -> onActivateLeaf item (return ())
      menuItemNewWithLabelOrMnemonic name
          | elem '_' name = menuItemNewWithMnemonic name
          | otherwise     = menuItemNewWithLabel name


data MenuItemCategory = MIStock StockId
                      | MILabel String
                      | MIMnemonic String

myMenuItemNew (MIStock x)    = fmap toMenuItem (imageMenuItemNewFromStock x)
myMenuItemNew (MILabel x)    = menuItemNewWithLabel x
myMenuItemNew (MIMnemonic x) = menuItemNewWithMnemonic x





mkRendererText attribs =
    do r <- cellRendererTextNew
       set r attribs
       return r

mkRendererToggle attribs =
    do r <- cellRendererToggleNew
       set r attribs
       return r


addColumn view model title renderer f =
    do c <- treeViewColumnNew
       treeViewColumnSetTitle c title
       cellLayoutPackStart c renderer True
       cellLayoutSetAttributes c renderer model f
       treeViewAppendColumn view c


mkViewCol view title =
  do col <- treeViewColumnNew
     treeViewColumnSetTitle col title
     treeViewColumnSetResizable col True
     -- -- treeViewColumnSetSizing col TreeViewColumnAutosize
     -- treeViewColumnSetSizing col TreeViewColumnFixed
     treeViewColumnSetReorderable col True
     treeViewAppendColumn view col
     return col

mkCellRenderer view model col makeCellRenderer attribs f =
  do rend <- makeCellRenderer
     treeViewColumnPackStart col rend True
     set rend attribs
     cellLayoutSetAttributes col rend model f
     return rend

mkColN view model title renderers =
  do col <- treeViewColumnNew
     treeViewColumnSetTitle col title
     treeViewColumnSetResizable col True
     -- -- treeViewColumnSetSizing col TreeViewColumnAutosize
     -- treeViewColumnSetSizing col TreeViewColumnFixed
     treeViewColumnSetReorderable col True
     forM renderers $ \(makeCellRenderer, attribs, f) ->
       do rend <- makeCellRenderer
          treeViewColumnPackStart col rend True
          set rend attribs
          cellLayoutSetAttributes col rend model f
          treeViewAppendColumn view col
          return rend

-- mkCol view model title makeCellRenderer attribs f =
--   do col <- mkViewCol view title
--      mkCellRenderer view model col makeCellRenderer attribs f

mkCol view model title makeCellRenderer attribs f =
  head <$> mkColN view model title [(makeCellRenderer, attribs, f)]



sortFunc rawmodel selector iter1 iter2 =
  do x1 <- treeModelGetRow rawmodel iter1
     x2 <- treeModelGetRow rawmodel iter2
     return (compare (selector x1) (selector x2))

mkSortCol view model rawmodel maybeSort title mkCellRenderer attribs f =
  do col <- treeViewColumnNew
     treeViewColumnSetTitle col title
     treeViewColumnSetResizable col True
     -- treeViewColumnSetSizing col TreeViewColumnAutosize
     rend <- mkCellRenderer
     treeViewColumnPackStart col rend True
     set rend attribs
     cellLayoutSetAttributeFunc col rend model $ \iter ->
       do cIter <- treeModelSortConvertIterToChildIter model iter
          row <- treeModelGetRow rawmodel cIter
          set rend (f row)
     treeViewAppendColumn view col
     case maybeSort of
       Just (id,selector) ->
         do treeSortableSetSortFunc model id $ sortFunc rawmodel selector
            treeViewColumnSetSortColumnId col id
       Nothing ->
         return ()
     return rend




myScrolledWindowNew hscrollbarPolicy vscrollbarPolicy =
  do w <- scrolledWindowNew Nothing Nothing
     scrolledWindowSetPolicy w hscrollbarPolicy vscrollbarPolicy
     scrolledWindowSetShadowType w ShadowOut
     return w





errorMsg parentWindow message =
  do d <- messageDialogNew (Just parentWindow) [DialogModal] MessageError ButtonsClose message
     dialogRun d
     widgetDestroy d

askMsg parentWindow question =
    do d <- messageDialogNew (Just parentWindow) [DialogModal] MessageQuestion ButtonsYesNo (question :: String)
       response <- dialogRun d
       widgetDestroy d
       return (response == ResponseYes)






setBaseFromBg w =
  do style <- widgetGetStyle w
     forM_ (enumFrom StateNormal) $
       \state -> styleGetBackground style state >>= widgetModifyBase w state

restoreBase w =
  forM_ (enumFrom StateNormal) $
    \state -> widgetRestoreBase w state

setEntryBorderColor w color =
  forM_ (enumFrom StateNormal) $
       \state -> widgetModifyBg w state color



-- | The 'listStoreSwapRows' function swaps two rows in a tree view with
-- a list store model
listStoreSwapRows ::
  TreeViewClass self =>
  ListStore a -> -- ^ The list store model
  self ->        -- ^ The tree view widget
  Int ->         -- ^ The number of the first row and
  Int ->         -- ^ the number of the second row to swap
  IO ()
listStoreSwapRows model view i j =
  do x <- listStoreGetValue model i
     y <- listStoreGetValue model j
     listStoreSetValue model i y
     listStoreSetValue model j x
     treeViewSetCursor view [j] Nothing

