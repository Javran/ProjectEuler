{-# LANGUAGE TupleSections #-}
import Data.List
import Data.Function
import Data.Tuple
import Control.Arrow
import qualified Data.Map.Strict as M

-- get a list of (num, num^3 (broken into digits)),
-- and then group by cubic number length
sameCubicLengthGroups :: [ [(Int,[Int])] ]
sameCubicLengthGroups = groupBy ((==) `on` (length . snd))
                      . map (id &&& (toDigits . (^(3 :: Int))))
                      $ [1..]

-- | assumes n > 0
toDigits :: Int -> [Int]
toDigits = reverse
         . map snd
         . takeWhile (\(x,y) ->
                      x /= 0 || y /= 0)
         . tail
         . iterate ((`divMod` 10) . fst)
         . (,0)

permSet :: [(Int,[Int])] -> [ [Int] ]
permSet = M.elems
        . M.fromListWith (++)
        . map (swap . second sort . first (:[]))

main :: IO ()
main = do
    let cubicPermSets = map permSet sameCubicLengthGroups
        -- this is the permset we interested in:
        -- a group containing exactly 5 elements
        cubicPermSets1 = map (filter ((== 5) . length)) cubicPermSets
        cubicPermSets2 = concat cubicPermSets1
        answerGroup = head cubicPermSets2
        answerGroupCubic = map (^(3 :: Int)) answerGroup
    print answerGroup
    print answerGroupCubic
    print (minimum answerGroupCubic)
