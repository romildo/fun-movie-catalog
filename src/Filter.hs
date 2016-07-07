{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Filter where

import Control.Monad (when, forM)
import Graphics.UI.Gtk hiding (Release)
import Data.Char (toLower)
import Data.Array.IArray ((!))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust, fromJust, catMaybes)
import Data.List (isPrefixOf, isInfixOf, findIndex, sort)
import Text.Regex.PCRE ((=~))
import Text.Printf (printf)
import qualified Data.Text as T

import Movie (Info(Title,OriginalTitle,Length), Movie(M,info))
import Component (mkW, mkWA, myBoxPackStart, myTableAttach, myBoxPackEnd)



filterMovies parent model fModel visCol view savedCriteriaRef =
  do d <- dialogNew
     set d [windowTitle := "Movie Filter", windowTransientFor := parent, containerBorderWidth := 2]
     dialogAddButton d stockCancel ResponseReject
     ok <- dialogAddButton d stockOk ResponseAccept
     widgetGrabDefault ok
     u <- dialogGetUpper d
     label <- mkWA (labelNew (Nothing :: Maybe String)) (myBoxPackStart u PackNatural 0) [miscXalign := 0]
     labelSetMarkupWithMnemonic label "<b>Criteria</b>"
     box1 <- mkWA (hBoxNew False 0) (myBoxPackStart u PackGrow 0) [containerBorderWidth := 2]
     frame <- mkWA frameNew (myBoxPackStart box1 PackGrow 0) [containerBorderWidth := 2]
     table <- mkWA (tableNew 1 3 False) (containerAdd frame) [containerBorderWidth := 2]
     mkWA (labelNew (Just "Field")) (myTableAttach table 0 1 0 1 [Fill] [Fill] 2 2) [miscXalign := 0]
     mkWA (labelNew (Just "Criteria")) (myTableAttach table 1 2 0 1 [Fill] [Fill] 2 2) [miscXalign := 0]
     mkWA (labelNew (Just "Value")) (myTableAttach table 2 3 0 1 [Fill] [Fill] 2 2) [miscXalign := 0]
     box2 <- mkWA (vBoxNew False 0) (myBoxPackStart box1 PackNatural 0) [containerBorderWidth := 4]
     bAdd <- mkW (buttonNewFromStock stockAdd) (myBoxPackStart box2 PackNatural 2)
     bClear <- mkWA (buttonNewFromStock stockClear) (myBoxPackEnd box2 PackNatural 2) [widgetSensitive := True]
     --
     let fields = sort $ map (\x -> (show x, x)) [minBound::Info ..]
     let criteria = [ ( "is equal to"        , caseInsensitive (==) :: String -> String -> Bool )
                    , ( "is not equal to"    , caseInsensitive (/=)                             )
                    , ( "starts with"        , caseInsensitive isPrefixOf                       )
                    , ( "does not start with", caseInsensitive $ (not .) . isPrefixOf           )
                    , ( "contains"           , caseInsensitive isInfixOf                        )
                    , ( "does not contain"   , caseInsensitive $ (not .) . isInfixOf            )
                    , ( "match regex"        , (flip (=~))::String->String->Bool                )
                    , ( "<"                  , caseInsensitive $ flip (<)                       )
                    , ( "<="                 , caseInsensitive $ flip (<=)                      )
                    , ( ">"                  , caseInsensitive $ flip (>)                       )
                    , ( ">="                 , caseInsensitive $ flip (>=)                      )
                    ]

     filterCriteriaRef <- newIORef []
     --
     let addCriterion (field,criterion,value) =
           do cs <- readIORef filterCriteriaRef
              let len = length cs
              let r = len + 1
              cmbField <- mkW comboBoxNewText (myTableAttach table 0 1 r (r+1) [Fill] [Fill] 2 2)
              mapM_ (comboBoxAppendText cmbField . T.pack . fst) fields
              comboBoxSetActive cmbField (fromJust (findIndex ((== field) . fst) fields))
              cmbCriteria <- mkW comboBoxNewText (myTableAttach table 1 2 r (r+1) [Fill] [Fill] 2 2)
              mapM_ (comboBoxAppendText cmbCriteria . T.pack . fst) criteria
              comboBoxSetActive cmbCriteria (fromJust (findIndex ((== criterion) . fst) criteria))
              entry <- mkW entryNew (myTableAttach table 2 3 r (r+1) [Fill, Expand] [Fill] 2 2)
              entrySetText entry value
              bRem <- mkW (buttonNewFromStock stockRemove) (myTableAttach table 3 4 r (r+1) [Fill] [Fill] 2 2)
              onClicked bRem $
                do containerRemove table cmbField
                   containerRemove table cmbCriteria
                   containerRemove table entry
                   containerRemove table bRem
                   cs <- readIORef filterCriteriaRef
                   writeIORef filterCriteriaRef $
                     let (xs,_:ys) = splitAt len cs
                     in xs ++ Nothing : ys
                   widgetSetSensitive bClear (length (catMaybes cs) > 1)
              writeIORef filterCriteriaRef (cs ++ [Just (cmbField, cmbCriteria, entry, bRem)])
              widgetSetSensitive bClear True
              containerResizeChildren table
              widgetShowAll table

     readIORef savedCriteriaRef >>= mapM_ addCriterion

     onClicked bAdd $ addCriterion ("Title","is equal to","")
     --
     onClicked bClear $
       do writeIORef filterCriteriaRef []
          containerForeach table (containerRemove table)
          widgetSetSensitive bClear False
     --
     widgetShowAll u
     response <- dialogRun d
     when (response == ResponseAccept) $
       do cs1 <- readIORef filterCriteriaRef
          cs2 <- fmap catMaybes $ forM (catMaybes cs1) $ \(cmbField,cmbCriteria,entry,_) ->
                   do f <- comboBoxGetActiveText cmbField
                      c <- comboBoxGetActiveText cmbCriteria
                      x <- entryGetText entry
                      if isJust f && isJust c && not (null x)
                      then return $ Just (T.unpack $ fromJust f, T.unpack $ fromJust c, x)
                      else return $ Nothing
          let tests = flip map cs2 $ \(fieldName,criteriaName,value) ->
                        (fromJust (lookup fieldName fields),
                         fromJust (lookup criteriaName criteria),
                         value
                        )
          let filterFunc M{info} = all (\(i,g,x) -> g x (info ! i)) tests
          customStoreSetColumn model visCol filterFunc
          treeModelFilterRefilter fModel
          (fPath,_) <- treeViewGetCursor view
          when (null fPath) $
            treeViewSetCursor view [0] Nothing
          writeIORef savedCriteriaRef cs2
     widgetDestroy d


caseInsensitive f x y =
  f (map toLower x) (map toLower y)
