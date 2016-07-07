module Rating where

import Graphics.UI.Gtk
import Data.List (intersperse)
import Data.List.Split (wordsBy)

import Util (fst3, snd3, trd3, trim)
import Component (mkW, mkWA, mkSortCol, myScrolledWindowNew, myBoxPackStart, myBoxPackEnd, myBoxPackStartWithMnemonic, myNotebookAppendPage, AnyComponent(AnyComponent), Component(getText,setText))


ratingsToString ratings =
  unlines $ map (\(x,y,z) -> x ++ ":" ++ y ++ ":" ++ z) ratings


parseRatings text =
  map
    (\rat -> case wordsBy (==':') rat of
               [a,b,c] -> (a,b,c)
               [a,b]   -> (a,b,"")
               [a]     -> ("",a,"")
               _       -> ("",rat,""))
    (lines text)

data RatingsView = RatingsView (ListStore (String,String,String)) TreeView Button

ratingsViewNew text pack =
  do let ratings = parseRatings text
     mainBox <- mkWA (vBoxNew False 2) pack [containerBorderWidth := 0]
     box <- mkWA (hBoxNew False 2) (myBoxPackStart mainBox PackNatural 2) [containerBorderWidth := 2]
     label <- mkWA (labelNewWithMnemonic "_Ratings") (myBoxPackStart box PackNatural 2) [miscXalign := 0]
     bRem <- mkWA (buttonNewFromStock stockRemove) (myBoxPackEnd box PackNatural 2) [widgetSensitive := not (null ratings)]
     bAdd <- mkW (buttonNewFromStock stockAdd) (myBoxPackEnd box PackNatural 2)
     scr <- mkWA (myScrolledWindowNew PolicyAutomatic PolicyAutomatic) (myBoxPackStart mainBox PackGrow 2) [containerBorderWidth := 2, widgetWidthRequest := 128]
     model <- listStoreNew ratings
     smodel <- treeModelSortNewWithModel model
     view <- mkWA (treeViewNewWithModel smodel) (containerAdd scr) [treeViewHeadersVisible := True]
     labelSetMnemonicWidget label view
     r1 <- mkSortCol view smodel model (Just (1,fst3)) "Rater"  cellRendererTextNew [cellTextEditable := True] $ \(x,_,_) -> [cellText := x]
     r2 <- mkSortCol view smodel model (Just (2,snd3)) "Rating" cellRendererTextNew [cellTextEditable := True] $ \(_,x,_) -> [cellText := x]
     r3 <- mkSortCol view smodel model (Just (3,trd3)) "Votes"  cellRendererTextNew [cellTextEditable := True] $ \(_,_,x) -> [cellText := x]
     --
     on r1 edited $ \path x -> do [i] <- treeModelSortConvertPathToChildPath smodel path
                                  (a,b,c) <- listStoreGetValue model i
                                  listStoreSetValue model i (x,b,c)
     --
     on r2 edited $ \path x -> do [i] <- treeModelSortConvertPathToChildPath smodel path
                                  (a,b,c) <- listStoreGetValue model i
                                  listStoreSetValue model i (a,x,c)
     --
     on r3 edited $ \path x -> do [i] <- treeModelSortConvertPathToChildPath smodel path
                                  (a,b,c) <- listStoreGetValue model i
                                  listStoreSetValue model i (a,b,x)
     --
     on bAdd buttonActivated $
       do widgetGrabFocus view
          Just c1 <- treeViewGetColumn view 0
          (path,_) <- treeViewGetCursor view
          cPath <- treeModelSortConvertPathToChildPath smodel path
          case cPath of
            [i] -> do listStoreInsert model (i+1) ("","","")
                      path' <- treeModelSortConvertChildPathToPath smodel [i+1]
                      treeViewSetCursor view path' (Just (c1, True))
            _   -> do j <- listStoreAppend model ("","","")
                      path' <- treeModelSortConvertChildPathToPath smodel [j]
                      treeViewSetCursor view path' (Just (c1, True))
          widgetSetSensitive bRem True
     --
     on bRem buttonActivated $
       do (path@[pos],_) <- treeViewGetCursor view
          [i] <- treeModelSortConvertPathToChildPath smodel path
          listStoreRemove model i
          size <- listStoreGetSize model
          if size > 0
            then treeViewSetCursor view [if pos == size then pos-1 else pos] Nothing
            else widgetSetSensitive bRem False
     --
     return (AnyComponent (RatingsView model view bRem))

instance Component RatingsView where
  getText (RatingsView model _ _) =
    do xs <- listStoreToList model
       return (ratingsToString xs)
  --
  setText (RatingsView model view bRem) text =
    ratingsViewSetText model view bRem text

ratingsViewSetText model view bRem text =
  do listStoreClear model
     case parseRatings text of
       []   -> widgetSetSensitive bRem False
       ratings -> do mapM_ (listStoreAppend model) ratings
                     treeViewSetCursor view [0] Nothing
                     widgetSetSensitive bRem True

