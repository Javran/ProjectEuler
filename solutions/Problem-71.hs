import Data.Ratio

{-
  see: http://en.wikipedia.org/wiki/Farey_sequence

  if two fractions are Farey neighbors, a/b < c/d,
  then we have:

    b*c - a*d = 1

  the converse is true: if b*c - a*d = 1, then
  a/b and c/d must be neighbors in Farey sequence of order max(b,d)

  therefore we can search all possible d <= 1,000,000 to find those pairs

  the closest neighbor must have the maximum value.

  in our problem, we know that: n/d < 3/7, so the target is:

  3d - 7n = 1, therefore:  n = (3*d-1) / 7, n is an integer

-}

solutions :: [Ratio Int]
solutions = map (\d -> (3*d-1) `div` 7 % d)
          . filter (\d -> (3*d-1) `mod` 7 == 0)
          $ [1..1000000]

main :: IO ()
main = print
     . maximum
     $ solutions
