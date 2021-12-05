import Data.List
import Data.List.Split

type Board = [[Int]]

parse :: [String] -> ([Int], [Board])
parse (x:xs) = (parseDraws x, parseBoard <$> splitOn [""] xs)
    where parseDraws = fmap read . splitOn ","
          parseBoard = fmap (fmap read . words)

markBoard :: Int -> Board -> Board 
markBoard = fmap . fmap . markCell
    where markCell n c = if c == n then 0 else c

stepBingo :: ([(Board, Int)], [Board]) -> Int -> ([(Board, Int)], [Board])
stepBingo (wonBoards, boards) i = (wonBoards' ++ wonBoards, boards')
    where (wonBoards', boards') = stepBingo' boards i

stepBingo' :: [Board] -> Int -> ([(Board, Int)], [Board])
stepBingo' boards i = (fmap (flip (,) i) wonBoards, boards')
    where (wonBoards, boards') = partition isBingo markedBoards
          markedBoards = fmap (markBoard i) boards

isBingo :: Board -> Bool
isBingo ns = or (any (== 0) . (fmap sum) <$> [transpose ns, ns]) 

playBingo :: [Int] -> [Board] -> [(Board, Int)]
playBingo draws boards = fst $ foldl stepBingo ([], boards) draws

score :: Board -> Int -> Int
score = (*) . sum . fmap sum

main = do
    wonBoards <- uncurry playBingo <$> parse <$> lines <$> readFile "input04.txt"
    print $ (uncurry score) (last wonBoards)
    print $ (uncurry score) (head wonBoards)
