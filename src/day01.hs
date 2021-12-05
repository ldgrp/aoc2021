import Data.Ord
import Control.Monad

window3 :: [Int] -> [Int]
window3 = zipWith3 (\a b c -> a + b + c) <$> id <*> tail <*> tail . tail

diffs :: [Int] -> [Ordering]
diffs = zipWith compare <$> id <*> tail

countLT :: [Ordering] -> Int
countLT = length . filter (LT ==)

part1 :: [Int] -> Int
part1 = countLT . diffs

part2 :: [Int] -> Int
part2 = countLT . diffs . window3

main = do
    nums <- fmap read <$> words <$> readFile "input01.txt"
    print (part1 nums)
    print (part2 nums)
