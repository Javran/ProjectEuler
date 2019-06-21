module ProjectEuler.GetData
  ( getDataFile
  , getDataDir
  ) where

import Paths_ProjectEuler
import System.FilePath

import qualified System.IO.Strict as SIO

-- TODO: can we use file-embed here to completely get rid of
-- runtime IO overhead?

getDataFile :: String -> IO String
getDataFile fName = do
    dir <- getDataDir
    SIO.readFile (dir </> "data" </> fName)
