import Data.Char
import Prelude hiding ((^))
import qualified Prelude ((^))

infixr 8 ^
(^) :: Num a => a -> Int -> a
x ^ n = x Prelude.^ n

main :: IO ()
main = print $ sum $ map digitToInt $ show (2 ^ 1000 :: Integer)
