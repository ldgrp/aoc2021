import Data.List.Split
import qualified Data.Map as M

type Point = (Int, Int)
type Line = (Point, Point)
type Board = M.Map Point Int

parseLine :: String -> Line
parseLine s = ((x1, y1), (x2, y2))
    where
        ((x1:y1:_):(x2:y2:_):_) = (fmap read) <$> splitOn "," <$> splitOn "->" s

drawLine :: Line -> Board -> Board
drawLine ((x1, y1), (x2, y2)) board = foldr drawPoint board [(x1 + dx * i, y1 + dy * i) | i <- [0..d]]
    where d = max (abs (x1 - x2)) (abs (y1 - y2))
          dx = signum (x2 - x1)
          dy = signum (y2 - y1)

drawPoint :: Point -> Board -> Board
drawPoint = flip (M.insertWith (const (+ 1))) 1

isDiagonal :: Line -> Bool
isDiagonal ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

part1 :: [Line] -> Int
part1 = length . M.filter (>= 2) . foldr drawLine M.empty . filter isDiagonal

part2 :: [Line] -> Int
part2 = length . M.filter (>= 2) . foldr drawLine M.empty

main = do
    vents <- fmap parseLine <$> lines <$> readFile "input05.txt"
    print (part1 vents)
    print (part2 vents)
