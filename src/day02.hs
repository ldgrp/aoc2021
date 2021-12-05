import Control.Monad

data Instr = Forward Integer | Down Integer | Up Integer deriving (Show, Eq)

toInstr :: [String] -> Instr
toInstr (d:x:_) 
    | d == "forward" = Forward (read x)
    | d == "up" = Up (read x)
    | d == "down" = Down (read x)

getPos :: (Integer, Integer) -> Instr -> (Integer, Integer)
getPos (x, y) (Forward n) = (x+n, y)
getPos (x, y) (Up n) = (x, y-n)
getPos (x, y) (Down n) = (x, y+n)

getPosWithAim :: (Integer, Integer, Integer) -> Instr -> (Integer, Integer, Integer)
getPosWithAim (x, y, a) (Forward n) = (x+n, y+(n*a), a)
getPosWithAim (x, y, a) (Up n) = (x, y, a-n)
getPosWithAim (x, y, a) (Down n) = (x, y, a+n)

main = do 
    instrs <- fmap (toInstr . words) <$> lines <$> readFile "input02.txt"
    print $ (\(x, y) -> x * y) $ foldl getPos (0, 0) instrs
    print $ (\(x, y, _a) -> x * y) $ foldl getPosWithAim (0, 0, 0) instrs
    