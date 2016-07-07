{-# LANGUAGE FlexibleContexts #-}

module Comments where

import Control.Monad (forM, forM_, when)
import Graphics.UI.Gtk
import Data.Array.IArray (Array, array, (!), (//))
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Movie (CommentInfo(..), commentInfoBounds, emptyCommentInfo)
import Component (mkW, mkWA, mkCol, myBoxPackStart, myBoxPackEnd, myNotebookAppendPage, packInTable, Component(getText), myEntryNewWithText, myTextViewNew)


drawComments comments pack =
  do boxM <- mkWA (vBoxNew False 2) pack [containerBorderWidth := 2]
     boxB <- mkWA (hBoxNew False 2) (myBoxPackEnd boxM PackNatural 2) [containerBorderWidth := 2]
     ntbk <- mkWA notebookNew (myBoxPackEnd boxM PackGrow 2) [containerBorderWidth := 2]
     --
     buttonAdd <- mkW (buttonNewFromStock stockAdd) (myBoxPackEnd boxB PackNatural 0)
     buttonRemove <- mkWA (buttonNewFromStock stockRemove) (myBoxPackEnd boxB PackNatural 0) [widgetSensitive := not (null comments)]
     --
     let commentNew comment =
           do t <- mkWA (tableNew 0 0 False) (myNotebookAppendPage ntbk (comment!CommentAuthor)) [containerBorderWidth := 2]
              let controlDescriptions =
                    [ (CommentAuthor     , myEntryNewWithText []  , packInTable t 0 0 2 [Fill] [] "_Author"     )
                    , (CommentTitle      , myEntryNewWithText []  , packInTable t 1 0 2 [Fill] [] "_Title"      )
                    , (CommentDescription, myEntryNewWithText []  , packInTable t 2 0 2 [Fill] [] "_Description")
                    , (CommentContact    , myEntryNewWithText []  , packInTable t 3 0 2 [Fill] [] "_Contact"    )
                    , (CommentDate       , myEntryNewWithText []  , packInTable t 4 0 2 [Fill] [] "Dat_e"       )
                    , (CommentScore      , myEntryNewWithText []  , packInTable t 5 0 2 [Fill] [] "_Score"      )
                    , (CommentBody       , myTextViewNew (-1) (-1)  , packInTable t 6 0 2 [Fill,Expand] [Fill,Expand] "Co_mment"    )
                    -- , (CommentBody       , myTextViewNew 600 400  , boxPackStartDefaults u             )
                    ]
              ws <- forM controlDescriptions $ \(i,mk,pack) ->
                      do w <- mk (comment!i) pack
                         return (i,w)
              widgetShowAll ntbk
              widgetSetSensitive buttonRemove True
              return ws

     ref <- newIORef []
     --
     onClicked buttonRemove $
       do pos <- notebookGetCurrentPage ntbk
          when (pos >= 0) $
            do notebookRemovePage ntbk pos
               widgetShowAll ntbk
               modifyIORef ref $ \xs -> let (as,_:bs) = splitAt pos xs in (as ++ bs)
               n <- notebookGetNPages ntbk
               when (n == 0) $
                 widgetSetSensitive buttonRemove False

     let add cs = do wss <- forM cs commentNew
                     notebookSetCurrentPage ntbk (-1)
                     modifyIORef ref (++wss)

     onClicked buttonAdd $ add [emptyCommentInfo // [(CommentAuthor,"Author")]]

     let set cs = do n <- notebookGetNPages ntbk
                     forM_ [1..n] $ \_ -> notebookRemovePage ntbk 0
                     wss <- forM cs commentNew
                     writeIORef ref wss

     let get    = do ws <- readIORef ref
                     forM ws $ \ks ->
                       do zs <- forM ks $ \(i,w) ->
                                  do x <- getText w
                                     return (i,x)
                          return (array commentInfoBounds zs)

     return (set,get,error "not implemented")


{-
editComment comment =
  do d <- dialogNew
     windowSetTitle d "Comment Edition"
     dialogAddButton d stockOk ResponseOk
     dialogAddButton d stockCancel ResponseCancel
     u <- dialogGetUpper d
     t <- mkWA (tableNew 0 0 False) (boxPackStartDefaults u) [containerBorderWidth := 2]

     let controlDescriptions =
           [ (CommentTitle      , myEntryNewWithText []  , packInTableD t 0 0 2 "_Title"      )
           , (CommentDescription, myEntryNewWithText []  , packInTableD t 1 0 2 "_Description")
           , (CommentAuthor     , myEntryNewWithText []  , packInTableD t 2 0 2 "_Author"     )
           , (CommentContact    , myEntryNewWithText []  , packInTableD t 3 0 2 "_Contact"    )
           , (CommentDate       , myEntryNewWithText []  , packInTableD t 4 0 2 "_Date"       )
           , (CommentScore      , myEntryNewWithText []  , packInTableD t 5 0 2 "_Score"      )
           , (CommentBody       , myTextViewNew 600 400, boxPackStartDefaults u           )
           ]

     widgets <- mapM (\(i,mk,pack) -> do w <- mk (comment!i) pack; return (i,w)) controlDescriptions

     button <- mkW (buttonNewWithMnemonic "_Parse") (boxPackStartDefaults u)

     onClicked button $
       do let Just w = lookup CommentBody widgets
          text <- getText w
          let text' = trim text
          let patterns =
               [                               -- cineclick:
      	   ( "[\\d\\D]*\n"            ++ -- lixo
      	     "(.+)\\s*\n"             ++ -- title
      	     "Por\\s+(.+)\n"          ++ -- Por author
      	     "(.*@.*)\n\n"            ++ -- email
      	     "([\\d\\D]+?)\n"         ++ -- body
      	     "(?:\\s+\n){3}[\\d\\D]*"    -- lixo
                 , \ [title,autor,contact,body] -> [title,"",autor,contact,"","",body]
                 )
               ]
          case mapMaybe (\(pat,cons) -> fmap cons (matchRegex (makeRegex pat :: Regex) text')) patterns of
            values:_ -> mapM_ (\((_,w),v) -> setText w v) (zip widgets values)
            _ -> return ()

          return ()

     widgetShowAll u

     let loop =
          do resp <- dialogRun d
             case resp of
               ResponseOk ->
                   do values <- mapM (\(i,w) -> do x <- getText w; return (i,trim x)) widgets
                      case lookup CommentAuthor values of
                        Just (_:_) -> do widgetDestroy d
                                         return (Just (array commentInfoBounds values))
                        _ -> do errorMsg (toWindow d) "The author of the comment should be specified."
                                loop
               ResponseCancel ->
                   do widgetDestroy d
                      return Nothing
     loop
-}
