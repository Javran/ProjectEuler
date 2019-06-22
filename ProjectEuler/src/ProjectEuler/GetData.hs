{-# LANGUAGE TemplateHaskell #-}
module ProjectEuler.GetData
  ( getDataFile
  , getDataDir
  , rawDirContents
  ) where

import Data.FileEmbed
import Paths_ProjectEuler
import System.FilePath

import qualified Data.ByteString as BS
import qualified System.IO.Strict as SIO

-- TODO: can we use file-embed here to completely get rid of
-- runtime IO overhead?

getDataFile :: String -> IO String
getDataFile fName = do
    dir <- getDataDir
    SIO.readFile (dir </> "data" </> fName)

rawDirContents :: [(FilePath, BS.ByteString)]
rawDirContents = $(embedDir "data")
