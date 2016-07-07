module Main where

import Util (openURIString)

main =
  do src <- openURIString "http://www.adorocinema.com/filmes/ultimato-bourne/ficha-tecnica-e-premios/"
     case src of
       Left err -> print err
       Right doc -> do writeFile "test.html" doc
                       putStrLn doc
