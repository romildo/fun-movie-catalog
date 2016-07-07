module Writer ( writeCollection ) where

-- import Prelude hiding (writeFile)
import System.IO (openFile, hClose, hPutStrLn, IOMode(..), writeFile)
-- import System.IO.UTF8 (writeFile)

import Movie (Col(..))


writeCollection fileName collection =
    writeFile fileName (show collection)
