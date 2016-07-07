module EditDist where

import Util (tracefun)

editDistance :: String -> String -> Int
editDistance [] s2 = length s2
editDistance s1 [] = length s1
editDistance s1@(x:r1) s2@(y:r2) = min (editDistance r1 s2 + 1)
                                       (min (editDistance s1 r2 + 1)
                                            (editDistance r1 r2 + cost))
  where
    cost | x == y    = 0
         | otherwise = 1

