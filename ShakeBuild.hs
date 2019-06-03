import Development.Shake
import Development.Shake.FilePath
import Data.List
import Data.Function
import Data.Time.Clock
import System.Exit

prettyPrintReport :: [(String, ExitCode, Integer)] -> String
prettyPrintReport = unlines . concatMap lineToStr
  where
    lineToStr (binFile, ec, time) = [ "Target: " ++ binFile
                                    , "  ExitCode: " ++ show ec
                                    , "  Time: " ++ show time ++ "ms"
                                    , ""]

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
    let finalReport = "_build/report.txt"
    want [ finalReport ]

    finalReport %> \_ -> do
        need ["compile"]
        bins <- getDirectoryFiles "" ["_build/Problem-*.bin"]
        let getNumber :: FilePath -> (FilePath, Int)
            getNumber binFile = (binFile, n)
              where
                n = read . drop (length "Problem-") . takeBaseName $ binFile
            sortedBins = sortBy (compare `on` snd)
                       . map getNumber
                       $ bins
            measure (binFile,_) = do
                putNormal $ "Measuring " ++ (binFile :: String)
                t1 <- liftIO getCurrentTime
                Exit ec <- cmd binFile
                t2 <- liftIO getCurrentTime
                return (binFile,ec, round (1000 * t2 `diffUTCTime` t1))

        reportLines <- mapM measure sortedBins
        liftIO $ writeFile finalReport (prettyPrintReport reportLines)

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

    phony "compile" $ do
        sources <- getDirectoryFiles "" ["solutions/Problem-*.hs"]
        let srcToTarget :: FilePath -> (FilePath, Int)
            srcToTarget src = ("_build" </> takeFileName src -<.> "bin",n)
              where
                n = read . drop (length "Problem-") . takeBaseName $ src
            targets = sortBy (flip compare `on` snd) -- compile using ascending order
                    . map srcToTarget
                    $ sources
        need (map fst targets)

    "_build/Problem-*.bin" %> \out -> do
        let srcFileName = "solutions" </> takeFileName out -<.> "hs"
        need [srcFileName]

        cmd "ghc -O2 -fforce-recomp" srcFileName
            "-o" out "-outputdir" "_build" :: Action ()

        removeFilesAfter "_build" ["//*.hi", "//*.o"]
