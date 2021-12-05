import Data.Ord
import Control.Monad

window3 :: [Int] -> [Int]
window3 = fmap (\(a, b, c) -> a + b + c) . (zip3 <*> tail <*> tail . tail)

diffs :: [Int] -> [Ordering]
diffs = zipWith compare <$> tail <*> id

countIncreasing :: [Int] -> Int
countIncreasing = length . filter (GT ==) . diffs

main = do
    nums <- fmap read <$> words <$> readFile "input01.txt"
    print $ countIncreasing nums
    print $ countIncreasing $ window3 nums
