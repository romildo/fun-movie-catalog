module Main where

newtype Pair = P (String,String)

instance Show Pair where
    showsPrec _ (P (a,b)) = shows a . showChar '/' . shows b

p1 :: (String,String)
p1 = ("inglês","english")

p2 :: Pair
p2 = P ("Inglês","English")

main = do putStrLn (show p1)
          putStrLn (show p2)
