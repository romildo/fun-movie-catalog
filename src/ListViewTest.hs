module Main (main) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as MV

data Title = Title { ident :: Int
                   , title :: String
                   , seen  :: Bool
                   }

collection
    = [ Title 1 "Title 1" False
      , Title 2 "Title 2" True
      , Title 3 "Title 3" True
      , Title 4 "Title 4" False
      ]

createListView =
    do model <- MV.listStoreNew collection
       view <- MV.treeViewNewWithModel model
       col1 <- MV.treeViewColumnNew
       col2 <- MV.treeViewColumnNew
       col3 <- MV.treeViewColumnNew
       MV.treeViewColumnSetTitle col1 "Id"
       MV.treeViewColumnSetTitle col2 "Title"
       MV.treeViewColumnSetTitle col3 "Flag"
       renderer1 <- MV.cellRendererTextNew
       renderer2 <- MV.cellRendererTextNew
       renderer3 <- MV.cellRendererToggleNew
       cellLayoutPackStart col1 renderer1 True
       cellLayoutPackStart col2 renderer2 True
       cellLayoutPackStart col3 renderer3 True
       cellLayoutSetAttributes col1 renderer1 model $ \row -> [ MV.cellText := show (ident row) ]
       cellLayoutSetAttributes col2 renderer2 model $ \row -> [ MV.cellText := title row ]
       cellLayoutSetAttributes col3 renderer3 model $ \row -> [ MV.cellActive := seen row ]
       MV.treeViewAppendColumn view col1
       MV.treeViewAppendColumn view col2
       MV.treeViewAppendColumn view col3
       return view

defaultSpacing = 8

main =
    do initGUI
       -- main window
       window <- windowNew
       windowSetTitle window "ListView Testing"
       onDestroy window mainQuit
       -- scrolled window
       scrolledWindow <- scrolledWindowNew Nothing Nothing
       scrolledWindowSetPolicy scrolledWindow PolicyAutomatic PolicyAutomatic
       containerAdd window scrolledWindow
       -- list view
       view <- createListView
       containerAdd scrolledWindow view
       -- main loop
       widgetShowAll window
       mainGUI
