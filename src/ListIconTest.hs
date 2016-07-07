module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as MV

data Title = Title { title :: String
                   , cover :: String
                   }
             deriving (Show)

addColumn view model title renderers =
    do c <- MV.treeViewColumnNew
       MV.treeViewColumnSetTitle c title
       MV.treeViewAppendColumn view c
       addRenderers c renderers
    where
      addRenderers c ((renderer,f):rs) = do r <- renderer
                                            MV.cellLayoutPackStart c r True
                                            MV.cellLayoutSetAttributes c r model f
                                            addRenderers c rs
      addRenderers c [] = return ()

main =
    do initGUI

       window <- windowNew
       windowSetTitle window "List Icon Test"
       windowSetDefaultSize window 200 200
       onDestroy window mainQuit

       scrwin <- scrolledWindowNew Nothing Nothing
       containerAdd window scrwin

       model <- MV.listStoreNew [Title {title="Title One", cover="makenfo.jpg"}]
       view <- MV.treeViewNewWithModel model
       MV.treeViewSetHeadersVisible view True
       addColumn view model "Title" [(MV.cellRendererPixbufNew, \row -> [ MV.cellPixbuf :=> pixbufNewFromFileAtSize (cover row) 64 64 ])
                                    ,(MV.cellRendererTextNew,   \row -> [ MV.cellText := title row ])]
       scrolledWindowAddWithViewport scrwin view

       widgetShowAll window

       mainGUI
