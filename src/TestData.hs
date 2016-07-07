{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Main (main) where

import Data.Array.IArray
import Graphics.UI.Gtk



data Store = S  String
           | SS [String]

store :: Array Int Store
store = array (1,2) [ (1,S "jose"), (2,SS ["a", "b"]) ]



class AnyComponent w where
    getValue :: w -> IO Store
    setValue :: w -> Store -> IO ()

instance AnyComponent Entry where
    getValue w = entryGetText w >>= \x -> return (S x)
    setValue w (S x) = entrySetText w x

instance AnyComponent Label where
    getValue w = labelGetText w >>= \x -> return (S x)
    setValue w (S x) = labelSetText w x



data Comp = forall w . (AnyComponent w) => Comp w Int

instance AnyComponent Comp where
    getValue (Comp w _) = getValue w
    setValue (Comp w _) = setValue w

componentsNew =
    do l <- labelNewWithMnemonic "Test"
       e <- entryNew
       return [ Comp l 1, Comp e 2 ]

drawComponents [] = return ()
drawComponents (Comp w i : xs) = do setValue w (store ! i)
                                    drawComponents xs

saveData [] ys = return (array (1,2) ys)
saveData (Comp w i : xs) ys = do x <- getValue w
                                 saveData xs ((i,x):ys)


main =
    do print "Testing"
