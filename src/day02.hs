import Control.Monad

data Instr = Forward Int | Down Int | Up Int deriving (Show, Eq)

toInstr :: [String] -> Instr
toInstr (d:x:_) 
    | d == "forward" = Forward (read x)
    | d == "up" = Up (read x)
    | d == "down" = Down (read x)

getPos :: (Int, Int) -> Instr -> (Int, Int)
getPos (x, y) (Forward n) = (x+n, y)
getPos (x, y) (Up n) = (x, y-n)
getPos (x, y) (Down n) = (x, y+n)

getPosWithAim :: (Int, Int, Int) -> Instr -> (Int, Int, Int)
getPosWithAim (x, y, a) (Forward n) = (x+n, y+(n*a), a)
getPosWithAim (x, y, a) (Up n) = (x, y, a-n)
getPosWithAim (x, y, a) (Down n) = (x, y, a+n)

part1 :: [Instr] -> Int
part1 = uncurry (*) . foldl getPos (0, 0)

part2 :: [Instr] -> Int
part2 =  (\(x, y, _) -> x * y) . foldl getPosWithAim (0, 0, 0)

main = do 
    instrs <- fmap (toInstr . words) <$> lines <$> readFile "input02.txt"
    print (part1 instrs)
    print (part2 instrs)
    