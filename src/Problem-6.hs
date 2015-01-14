main :: IO ()
main = print
     $ sq (sum [1..100]) - sum(map sq [1..100])
  where
    sq :: Int -> Int
    sq x = x * x
