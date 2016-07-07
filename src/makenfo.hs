module Main (main) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as MV
import Data.IORef

data Title = Title { ident :: Int
                   , title :: String
                   , originalTitle :: String
                   , seen :: Bool
                   }

collection
    = [ Title 1 "Title 1" "Original title 1" False
      , Title 2 "Title 2" "Original title 2" True
      , Title 3 "Title 3" "Original title 3" True
      , Title 4 "Title 4" "Original title 4" False
      ]

data UIType = UIText String
            | UICheck Bool

data UIDescriptor = UID String UIType 

uiDescr = [ UID "Identifier"     (UIText "1")
          , UID "Title"          (UIText "")
          , UID "Original title" (UIText "")
          , UID "Seen"           (UICheck False)
          ]


menuBarDescr
    = [ ("_File", [ ("Open", Nothing)
                  , ("Save", Nothing)
                  , ("_Quit", Just mainQuit)
                  ]
        )
      , ("Help",  [ ("Help", Nothing)
                  ]
        )
      ]

createListView =
    do model <- MV.listStoreNew collection
       view <- MV.treeViewNewWithModel model
       col1 <- MV.treeViewColumnNew
       col2 <- MV.treeViewColumnNew
       col3 <- MV.treeViewColumnNew
       col4 <- MV.treeViewColumnNew
       MV.treeViewColumnSetTitle col1 "Id"
       MV.treeViewColumnSetTitle col2 "Title"
       MV.treeViewColumnSetTitle col3 "Original title"
       MV.treeViewColumnSetTitle col4 "Flag"
       renderer1 <- MV.cellRendererTextNew
       renderer2 <- MV.cellRendererTextNew
       renderer3 <- MV.cellRendererTextNew
       renderer4 <- MV.cellRendererToggleNew
       cellLayoutPackStart col1 renderer1 True
       cellLayoutPackStart col2 renderer2 True
       cellLayoutPackStart col3 renderer3 True
       cellLayoutPackStart col4 renderer4 True
       cellLayoutSetAttributes col1 renderer1 model $ \row -> [ MV.cellText := show (ident row) ]
       cellLayoutSetAttributes col2 renderer2 model $ \row -> [ MV.cellText := title row ]
       cellLayoutSetAttributes col3 renderer3 model $ \row -> [ MV.cellText := originalTitle row ]
       cellLayoutSetAttributes col4 renderer4 model $ \row -> [ MV.cellActive := seen row ]
       MV.treeViewAppendColumn view col1
       MV.treeViewAppendColumn view col2
       MV.treeViewAppendColumn view col3
       MV.treeViewAppendColumn view col4
       return view

createDataBox descr
    = do box <- vBoxNew False 0
         titleBox <- hBoxNew False 0
         containerAdd box titleBox
         detailsTable <- tableNew (length descr) 2 False
         createTable detailsTable 0 descr
         containerAdd box detailsTable
         discsBox <- hBoxNew False 0
         containerAdd box discsBox
         return box
    where
      createTable table i []
          = return ()
      createTable table i (UID label ui : ds)
          = do lab <- labelNew (Just label)
               tableAttachDefaults table lab 0 1 i (i + 1)
               fUI table i ui
               createTable table (i + 1) ds
      fUI table i (UIText init)
          = do entry <- entryNew
               tableAttachDefaults table entry 1 2 i (i + 1)
      fUI table i (UICheck init)
          = do button <- checkButtonNew
               tableAttachDefaults table button 1 2 i (i + 1)

defaultSpacing = 8

main =
    do initGUI
       window <- windowNew
       windowSetTitle window "Movie Collection Organizer"
       onDestroy window mainQuit
       box <- vBoxNew False 5
       containerAdd window box
       menuBar <- createMenuBar menuBarDescr
       boxPackStart box menuBar PackNatural 0
       paned <- hPanedNew
       boxPackStart box paned PackNatural 0
       scrolledWindow <- scrolledWindowNew Nothing Nothing
       scrolledWindowSetPolicy scrolledWindow PolicyAutomatic PolicyAutomatic
       panedAdd1 paned scrolledWindow
       frame <- frameNew
       frameSetShadowType frame ShadowIn
       scrolledWindowAddWithViewport scrolledWindow frame
       view <- createListView
       containerAdd frame view
       --containerAdd scrolledWindow view
       frame <- frameNew
       frameSetShadowType frame ShadowIn
       panedAdd2 paned frame
       dataBox <- createDataBox uiDescr
       containerAdd frame dataBox
       widgetShowAll window
       mainGUI

createMenuBar descr
    = do menuBar <- menuBarNew
         mapM_ (createMenu menuBar) descr
         return menuBar
    where
      createMenu menuBar (name,items)
          = do menu <- menuNew
               item <- menuItemNewWithLabelOrMnemonic name
               menuItemSetSubmenu item menu
               menuShellAppend menuBar item
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
