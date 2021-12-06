import Data.List
import Data.List.Split

parse :: String -> [Int]
parse = fmap read . splitOn ","

nextDay :: [Int] -> [Int]
nextDay fish = fmap step fish ++ replicate (length (filter (== 0) fish)) 8
    where fish' = fmap step fish
          newFish = replicate (length (filter (== 0) fish)) 8

step :: Int -> Int
step 0 = 6
step x = x-1

nextDay' :: (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int, Int, Int, Int, Int)
nextDay' (a, b, c, d, e, f, g, h, i) = (b, c, d, e, f, g, h+a, i, a)

part1 :: [Int] -> Int
part1 = sum . head . drop 80 . iterate nextDay

part2 :: [Int] -> Int
part2 fish = sum' (head (drop 256 (iterate nextDay' fish')))
    where (a:b:c:d:e:f:g:h:i:_) = [length (filter (== l) fish) | l <- [0 .. 8]]
          fish' = (a, b, c, d, e, f, g, h, i)
          sum' (r, s, t, u, v, w, x, y, z) = r + s + t + u + v + w + x + y + z

main = do
    fish <- parse <$> readFile "input06.txt"
    print (part1 fish)
    print (part2 fish)