import Data.Char
import Data.List
import Data.Ord

mcb :: [Int] -> Int
mcb = head . maximumBy (comparing length) . group . sort

lcb :: [Int] -> Int
lcb = head . minimumBy (comparing length) . group . sort

gamma :: [[Int]] -> [Int]
gamma = fmap mcb . transpose

epsilon :: [[Int]] -> [Int]
epsilon = fmap lcb . transpose

oxygen :: [[Int]] -> [Int]
oxygen o@(_:_:_) = b : oxygen next
    where 
        next = tail <$> filter (\x -> head x == b) o
        b = mcb $ head $ transpose o
oxygen (x:_) = x
oxygen [] = []

co2 :: [[Int]] -> [Int]
co2 c@(_:_:_) = b : co2 next
    where
        next = tail <$> filter (\x -> head x == b) c
        b = lcb $ head $ transpose c
co2 (x:_) = x
co2 [] = []

binToNum :: [Int] -> Int
binToNum = foldl1 ((+) . (2*))
    
main = do
    numbers <- (fmap $ fmap digitToInt) <$> lines <$> readFile "input03.txt"
    print $ (binToNum $ epsilon numbers) * (binToNum $ gamma numbers)
    print $ (binToNum $ co2 numbers) * (binToNum $ oxygen numbers)
