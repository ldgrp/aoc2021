import Data.List
import Data.List.Split
import Data.Maybe

type Board = [[Maybe Int]]

parse :: [String] -> ([Int], [Board])
parse (x:xs) = (parseDraws x, parseBoard <$> splitOn [""] xs)
    where parseDraws = fmap read . splitOn ","
          parseBoard = fmap (fmap (Just . read) . words)

markBoard :: Int -> Board -> Board 
markBoard = fmap . fmap . markCell
    where markCell n (Just c) | c == n = Nothing
          markCell _ c = c

stepBingo :: ([(Board, Int)], [Board]) -> Int -> ([(Board, Int)], [Board])
stepBingo (wonBoards, boards) i = (wonBoards' ++ wonBoards, boards')
    where (wonBoards', boards') = stepBingo' boards i

stepBingo' :: [Board] -> Int -> ([(Board, Int)], [Board])
stepBingo' boards i = (fmap (flip (,) i) wonBoards, boards')
    where (wonBoards, boards') = partition isBingo markedBoards
          markedBoards = fmap (markBoard i) boards

isBingo :: Board -> Bool
isBingo ns = or (any (and . fmap isNothing) <$> [transpose ns, ns]) 

playBingo :: [Int] -> [Board] -> [(Board, Int)]
playBingo draws boards = fst $ foldl stepBingo ([], boards) draws

score :: Board -> Int -> Int
score bs i = i * (sum $ fmap (sum . catMaybes) bs)

part1 :: [Int] -> [Board] -> Int
part1 = ((uncurry score . last) .) . playBingo


part2 :: [Int] -> [Board] -> Int
part2 = ((uncurry score . head) .) . playBingo

main = do
    (draws, boards) <- parse <$> lines <$> readFile "input04.txt"
    print (part1 draws boards)
    print (part2 draws boards)
