module Main(main) where

import Graphics.UI.Gtk

main =
    do initGUI

       window <- windowNew
       onDestroy window mainQuit

       mainBox <- vBoxNew False 0

       label <- labelNew (Just "Testing ...")

       let list = [ containerAdd window
                  , \x -> boxPackStart mainBox x PackNatural 0
                  ] :: [ WidgetClass a => a -> IO () ]

       (head list) mainBox
       (head (tail list)) label

       widgetShowAll window
       mainGUI
