{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}

module CastGUI where

import Text.Parsec (Parsec, parse, try, sepBy, oneOf, anyChar, noneOf, char, string, many, between, manyTill, (<|>))
import Graphics.UI.Gtk
import Control.Monad (when)
import Data.List (intersperse)

import Util (trim)
import Component (mkW, mkWA, mkSortCol, myScrolledWindowNew, myBoxPackStart, myBoxPackEnd, myBoxPackStartWithMnemonic, myNotebookAppendPage, AnyComponent(AnyComponent), Component(getText,setText))


castToString cast =
  concat $ intersperse "\n" $ map (\(x,y) -> x ++ " (" ++ y ++ ")") cast


parseCast text =
  either (error . show) id $ parse parser "cast parser" text


parser :: Parsec String () [(String,String)]
parser =
  sepBy (try parser1 <|> try parser2 <|> parser3) (oneOf ",;\n")

parser1 =
  do xs <- manyTill (noneOf ",;\n") (char '(')
     ys <- rparen 1
     return (trim xs, trim ys)

parser2 =
  do xs <- manyTill (noneOf ",;\n") (string "...")
     ys <- many (noneOf ",;\n")
     return (trim xs, trim ys)

parser3 =
  do xs <- many (noneOf ",;\n")
     return (trim xs,"")

rparen n =
  do x <- anyChar
     case x of
       '(' -> rparen (n+1) >>= return . ('(':)
       ')' | n == 1 -> return ""
           | otherwise -> rparen (n-1) >>= return . (')':)
       _ -> rparen n >>= return . (x:)



data CastView = CastView (ListStore (String,String)) TreeView Button

castViewNew text pack =
  do let cast = parseCast text
     mainBox <- mkWA (vBoxNew False 2) pack [containerBorderWidth := 0]
     box <- mkWA (hBoxNew False 2) (myBoxPackStart mainBox PackNatural 2) [containerBorderWidth := 2]
     label <- mkWA (labelNewWithMnemonic "_Cast") (myBoxPackStart box PackNatural 2) [miscXalign := 0]
     bRem <- mkWA (buttonNewFromStock stockRemove) (myBoxPackEnd box PackNatural 2) [widgetSensitive := not (null cast)]
     bAdd <- mkW (buttonNewFromStock stockAdd) (myBoxPackEnd box PackNatural 2)
     bParse <- mkW (buttonNewWithMnemonic "_Parse") (myBoxPackEnd box PackNatural 2)
     scr <- mkWA (myScrolledWindowNew PolicyAutomatic PolicyAutomatic) (myBoxPackStart mainBox PackGrow 2) [widgetWidthRequest := 256, containerBorderWidth := 2]
     model <- listStoreNew cast
     smodel <- treeModelSortNewWithModel model
     view <- mkWA (treeViewNewWithModel smodel) (containerAdd scr) [treeViewHeadersVisible := True]
     labelSetMnemonicWidget label view
     r1 <- mkSortCol view smodel model (Just (1,fst)) "Actor" cellRendererTextNew [cellTextEditable := True] $ \(x,_) -> [cellText := x]
     r2 <- mkSortCol view smodel model (Just (2,snd)) "Role"  cellRendererTextNew [cellTextEditable := True] $ \(_,x) -> [cellText := x]
     --
     on r1 edited $ \path x -> do [i] <- treeModelSortConvertPathToChildPath smodel path
                                  (a,b) <- listStoreGetValue model i
                                  listStoreSetValue model i (x,b)
     --
     on r2 edited $ \path x -> do [i] <- treeModelSortConvertPathToChildPath smodel path
                                  (a,b) <- listStoreGetValue model i
                                  listStoreSetValue model i (a,x)
     --
     on bAdd buttonActivated $
       do widgetGrabFocus view
          Just c1 <- treeViewGetColumn view 0
          (path,_) <- treeViewGetCursor view
          cPath <- treeModelSortConvertPathToChildPath smodel path
          case cPath of
            [i] -> do listStoreInsert model (i+1) ("","")
                      path' <- treeModelSortConvertChildPathToPath smodel [i+1]
                      treeViewSetCursor view path' (Just (c1, True))
            _   -> do j <- listStoreAppend model ("","")
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
     on bParse buttonActivated $
       do cast <- listStoreToList model
          d <- dialogNew
          -- set d [windowTransientFor := parent]
          windowSetTitle d "Parse Cast Text"
          dialogAddButton d stockCancel ResponseReject
          ok <- dialogAddButton d stockOk ResponseAccept
          widgetGrabDefault ok
          u <- dialogGetUpper d
          scr <- mkWA (myScrolledWindowNew PolicyAutomatic PolicyAutomatic) (myBoxPackStart u PackGrow 0) [containerBorderWidth := 2]
          textView <- mkWA textViewNew (containerAdd scr)
                           [ widgetWidthRequest := 512
                           , widgetHeightRequest := 256
                           , textViewWrapMode := WrapWord
                           ]
          buffer <- textViewGetBuffer textView
          textBufferSetText buffer (castToString cast)
          widgetShowAll u
          response <- dialogRun d
          when (response == ResponseAccept) $
            do start <- textBufferGetStartIter buffer
               end <- textBufferGetEndIter buffer
               newText <- textBufferGetText buffer start end True
               castViewSetText model view bRem newText
          widgetDestroy d
     --
     return (AnyComponent (CastView model view bRem))

instance Component CastView where
  getText (CastView model _ _) =
    do xs <- listStoreToList model
       return (castToString xs)
  --
  setText (CastView model view bRem) text =
    castViewSetText model view bRem text

castViewSetText model view bRem text =
  do listStoreClear model
     case parseCast text of
       []   -> widgetSetSensitive bRem False
       cast -> do mapM_ (listStoreAppend model) cast
                  treeViewSetCursor view [0] Nothing
                  widgetSetSensitive bRem True

