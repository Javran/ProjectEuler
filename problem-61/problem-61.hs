import Control.Arrow
import Control.Monad

triangles   :: [Int]
squares     :: [Int]
pentagonals :: [Int]
hexagonals  :: [Int]
heptagonals :: [Int]
octagonals  :: [Int]

triangles   = map (\n -> n*(  n+1) `div` 2) [1..]
squares     = map (\n -> n*   n           ) [1..]
pentagonals = map (\n -> n*(3*n-1) `div` 2) [1..]
hexagonals  = map (\n -> n*(2*n-1)        ) [1..]
heptagonals = map (\n -> n*(5*n-3) `div` 2) [1..]
octagonals  = map (\n -> n*(3*n-2)        ) [1..]

-- | assuming the seq is always increasing,
-- we take only those that have exactly 4 digests,
-- namely those x, s.t. 1000 <= x < 10000
fourDigits :: [Int] -> [Int]
fourDigits = dropWhile (<  1000)
           . checkIncreasing
           . takeWhile (< 10000)
    where
      checkIncreasing xs
          -- well then we enforce i<=j -> a[i] <= a[j]
          | and (zipWith (<=) xs (tail xs)) = xs
          -- or get doomed
          | otherwise                       =
              error "Assertion failed: seq not increasing"

splitNum :: Int -> (Int, Int)
splitNum = (`divMod` 100)

unsplitNum :: (Int, Int) -> Int
unsplitNum (x,y) = 100*x+y

-- | pick up one element from the list, with list monad
chooseOne :: [a] -> [(a,[a])]
chooseOne xs = map (head &&& tail)
             -- and cut out those unintended parts
             . take l
             . map (take l)
             -- cycle through the list
             -- so that evey element has a chance
             -- of being the head
             . iterate tail
             . cycle
             $ xs
    where
      l = length xs

solve :: [Int] -> [ [(Int,Int)] ] -> [ [Int] ]
solve theCycle lists
    | null lists    = [theCycle]
    | null theCycle = do
        -- the first round, we need to pick up a starting pair
        (aa,bb) <- head lists
        solve [aa,bb] (tail lists)
    | otherwise     = do
        -- randomly pick up one of the remaining lists
        (nextList,remainingLists) <- chooseOne lists
            -- recover the chosen numbers,
            -- and get the last 2-digit number
            -- (the end of the current chain)
        let chosenNums = zipWith (curry unsplitNum)
                                 theCycle
                                 (tail theCycle)
            lastNode   = last theCycle
            candidates = filter ((`notElem` chosenNums) . unsplitNum)
                       . filter ((== lastNode) . fst)
                       $ nextList
        (_,next) <- candidates
        -- if this is the last candidate list
        -- we also require that our choice equal to the first 2-digit number chosen
        -- so that we can form the chain
        guard $ not (null remainingLists)
             || next == head theCycle
        solve (theCycle ++ [next]) remainingLists

main :: IO ()
main = do
    let seqs = map (map splitNum . fourDigits)
                   [ triangles
                   , squares
                   , pentagonals
                   , hexagonals
                   , heptagonals
                   , octagonals]
        solution = head (solve [] seqs)
        chosenNums = zipWith (curry unsplitNum)
                             solution
                             (tail solution)
    print chosenNums
    print (sum chosenNums)
