module ProjectEuler.CommandLine.CmdBenchmark
  ( cmdBenchmark
  ) where

{-
  TODO: GHC is being too smart here.
  We'll need a script that runs the binary several times
  to get an average, rather than having GHC figure it out
  that it can share the result (which is exactly what
  we don't want for benchmarking)
 -}

cmdBenchmark :: [String] -> IO ()
cmdBenchmark _ = pure ()
