module ProjectEuler.GetData (getDataFile) where

import Paths_ProjectEuler
import System.FilePath

import qualified System.IO.Strict as SIO

getDataFile :: String -> IO String
getDataFile fName = do
    dir <- getDataDir
    SIO.readFile (dir </> "data" </> fName)
