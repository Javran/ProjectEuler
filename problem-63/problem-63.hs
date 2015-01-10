import Control.Arrow
-- it's a pity that we can't use the formula l = 1 + floor( log10( n ) ) here
-- because of the precision error. try n = 1000 to find out
-- but I think this approach is good enough,
-- since it's really just a property about a "string"
digitLength :: (Show i, Integral i) => i -> Int
digitLength = length . show

-- the problem is, how many pairs of (a,n) satisfies that
--   digitLength (a^n) == n
--
-- fixing n >= 1, digitLength(a^n) is a monotonic function of a,
-- so we know that we can stop searching a solution if a is greater than some number
-- luckily we know that digitLength(10^n) == n+1, which limits the search space
-- for each fixed n to [0..9] (we include 0 because 0^1 is a valid solution)
--
-- then we need to check all possible n, the observation is,
-- as n keeps increasing, the number of solutions decreases and never bounces back
-- so we can stop searching immediately when no solution for a certain n can be found.
solutions :: [ (Int, Integer) ]
solutions = concat . takeWhile (not . null) . map solve $ [1..]
    where
      solve :: Int -> [ (Int, Integer) ]
      solve n = takeWhile ((== n) . digitLength . snd)
              . dropWhile ((<  n) . digitLength . snd)
              . map (\x -> (x,fromIntegral x^n))
              $ [1..9]

main :: IO ()
main = do
    print solutions
    print . length $ solutions
