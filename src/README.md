# Advent of Code 2021 Solutions

## Day 1 - Sonar Sweep
[Prompt][prompt01] - [Solution][day01src]

### Part 1

We can pair consecutive numbers by `zip`-ing the given list, and the list 
minus the first element. However, instead of tupling the elements, we wish
to `compare` them. `zipWith` is a generalisation of `zip` which replaces the
tupling function with any function of your choosing.

```haskell
-- zip :: [a] -> [b] -> [(a, b)]
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

diffs :: [Int] -> [Ordering]
diffs = zipWith compare <$> id <*> tail
-- diffs xs = zipWith compare xs (tail xs)
-- diffs xs = uncurry compare <$> zip xs (tail xs)
```

Then we have to count the pairs where the first element is *less than* the second 
element.

```haskell
countLT :: [Ordering] -> Int
countLT = length . filter (LT ==)
-- countLT os = length (filter (LT ==) os)
```

Now we have
```haskell
part1 :: [Int] -> Int
part1 = countLT . diffs
--part1 xs = countLT (diffs xs)
```

### Part 2

Now we have to compare sums of sliding windows of size 3.
`zipWith`'s cousin, `zipWith3` is here to save the day! 
We will now create a 3 tuple of 
`(the list, the list minus the first element, the list minus the first two elements)`
while summing them together.

```haskell
window3 :: [Int] -> [Int]
window3 = zipWith3 (\a b c -> a + b + c) <$> id <*> tail <*> tail . tail
--window3 xs = zipWith3 (\a b c -> a + b + c) xs (tail xs) (tail (tail xs))
```

Putting it all together,
```haskell
part2 :: [Int] -> Int
part2 = countLT . diffs . window3
-- part1 xs = countLT (diffs (window3 xs))
```

Now we have our first two gold stars ^_^
## Day 2 - Dive!
[Prompt][prompt02] - [Solution][day02src]

### Part 1

It's useful to parse the input into our own `Instr` data type.
There are three different instructions.

```haskell
data Instr = Forward Int | Down Int | Up Int
```

We can create a function which, given our current coordinates and
an instruction, returns our *next* coordinates. 

```haskell
getPos :: (Int, Int)  -- current coords
       ->  Instr      -- an instruction
       -> (Int, Int)  -- next coords
getPos (x, y) (Forward n) = (x+n, y)
getPos (x, y) (Up n) = (x, y-n)
getPos (x, y) (Down n) = (x, y+n)
```

Now it's just a matter of iterating through the list of instructions. 
We can use `foldl` with `(0, 0)` as our starting value and apply each 
instruction from left to right to obtain the final coordinates. 
Then we can multiply the x and y coordinates.

```haskell
-- foldl :: (b -> a -> b) -> b -> [a] -> b

part1 :: [Instr] -> Int
part1 = uncurry (*) . foldl getPos (0, 0)
-- part1 instrs = (\(x, y) -> x * y) (foldl getPos (0, 0) instrs)
```
### Part 2

Now we have to keep track of aim. We can modify the function above and
simply turn our 2-tuple into a 3-tuple.

```haskell
getPosWithAim :: (Int, Int, Int)  -- current coords
              ->  Instr           -- an instruction
              -> (Int, Int, Int)  -- next coords
getPosWithAim (x, y, a) (Forward n) = (x+n, y+(n*a), a)
getPosWithAim (x, y, a) (Up n) = (x, y, a-n)
getPosWithAim (x, y, a) (Down n) = (x, y, a+n)
```

Again, we multiply the x and y coordinates (ignoring aim) to get the final answer,

```haskell
part2 :: [Instr] -> Int
part2 =  (\(x, y, _) -> x * y) . foldl getPosWithAim (0, 0, 0)
```

## Day 3 - Binary Diagnostic
[Prompt][prompt02] - [Solution][day02src]


Let's start by declaring some useful utility functions. First, we need to 
calculate the most-common bit. Note that I am using the `Int` type to
encode boolean values `0` or `1`.

```haskell
mcb :: [Int] -- A list of bits
    ->  Int  -- The most common bit (Either 0 or 1)
mcb = head . maximumBy (comparing length) . group . sort
--mcb bs = head $ maximumBy (comparing length) $ group $ sort bs
```

whatdidyousay?! Let's take it step by step.

```haskell
ghci> sort [0, 1, 1, 0, 1]
[0,0,1,1,1]
ghci> group $ sort [0, 1, 1, 0, 1]
[[0,0],[1,1,1]]
ghci> maximumBy (comparing length) $ group $ sort [0, 1, 1, 0, 1]
[1,1,1]
ghci> head $ maximumBy (comparing length) $ group $ sort [0, 1, 1, 0, 1]
1
```

First, we sort the bits so that all 0s (and 1s) are next to each other.
Then we partition the array into two and compare the lengths of each array.
We are finding the most-common bit so we are interested in the longest array.
Finding the least-common bit is just a matter of looking for the shortest array.

```haskell
lcb :: [Int] -> Int
lcb = head . minimumBy (comparing length) . group . sort
```

The last thing is a utility function that lets us turn binary numbers into numbers. Hold your breath

```haskell
binToNum :: [Int] -- A list of bits
         ->  Int  -- The base10 representation of the input bits
binToNum = foldl1 ((+) . (2*))
--binToNum = foldl1 (\acc x -> x + 2*acc)
```

Once again, `fold` is the star of the show. Here, we consider each bit from left to right adding the current value (1) to the current accumulated value raised to the power of two. This is exactly how you would turn a big-endian binary number
to base10.

### Part 1

To calculate `gamma`, we need to consider the most-common bit of each column 
formed by the binary numbers. If only we could turn our list of list of bits `[[Int]]` around 90 degrees!

Fortunately we have `transpose`. We can use `fmap` to apply `transpose` to each
element (column) of the list. `epsilon` is calculated the same way but with `lcb`.

```haskell 
gamma :: [[Int]] -> [Int]
gamma = fmap mcb . transpose
-- gamma bs = fmap mcb (transpose bs)

epsilon :: [[Int]] -> [Int]
epsilon = fmap lcb . transpose
-- epsilon bs = fmap lcb (transpose bs)

part1 :: [[Int]] -> Int
part1 bs = (binToNum (epsilon bs)) * (binToNum (gamma bs))
```

### Part 2

Now it gets slightly more challenging. Fortunately thanks to the semantics of
`maximumBy` and `minimumBy` we don't have to change anything to comply with the
tie-breaker scenario for calculating `oxygen` and `co2`.

We can calculate `oxygen` recursively.

```haskell
oxygen :: [[Int]] -> [Int]
oxygen o@(_:_:_) = b : oxygen next                    -- 5
    where 
        next = tail <$> filter (\x -> head x == b) o  -- 2
        b = mcb $ head $ transpose o                  -- 1
oxygen (x:_) = x                                      -- 3
oxygen [] = []                                        -- 4
```

Step by step,

```haskell
b = mcb $ head $ transpose o                          -- 1
```

If `o` is a list of binary numbers, we can consider the columns by 
`transpose`-ing it. We can take the *first* column by taking the `head` of the list.
And finally, we can compute the most-common bit for this set of values.

```haskell
next = tail <$> filter (\x -> head x == b) o          -- 2
```

The candidate binary numbers for the next iteration can be found by
filtering our current list of binary numbers, keeping only numbers that have `b`
in the current bit position `head x`. We are able to do this (and step 1) because
in the same step, we chop off the "already processed" parts of the list with `tail`.

```haskell
oxygen (x:_) = x                                      -- 3
oxygen [] = []                                        -- 4
```

If we are left with a list of binary numbers containing exactly 1 element, 
then that element *is* the remaining part of the `oxygen` score. The constraints
of this problem mean (4) should never be hit but is kept in place to keep `ghc`
happy.

```haskell
oxygen o@(_:_:_) = b : oxygen next                    -- 5
```

Finally, we have the recursive step. If the list has two or more elements,
then we calculate `b` as described in (1) and recurse to get the remaining
bits. 

`co2` is calculated similarly, but with `lcb` instead of `mcb`.

```haskell
co2 :: [[Int]] -> [Int]
co2 c@(_:_:_) = b : co2 next
    where
        next = tail <$> filter (\x -> head x == b) c
        b = lcb $ head $ transpose c
co2 (x:_) = x
co2 [] = []
```

Now we just have to multiply the two values,

```haskell
part2 :: [[Int]] -> Int
part2 bs = (binToNum (co2 bs)) * (binToNum (oxygen bs))
```

## Day 4 - Giant Squid
[Prompt][prompt02] - [Solution][day02src]

### Part 1

It's time for Bingo! Let's start with some useful definitions.

```haskell
type Board = [[Maybe Int]]
type Draws = [Int]
```

A `Board` is a 5x5 grid of `Maybe Int`s and `Draws` are a list of `Int`.
Marking a bingo board can be implemented by iterating through each
cell of each row, and setting the cell to `Nothing` if it has been drawn
in this round.

```haskell
markBoard :: Int    -- the drawn number
          -> Board  -- the current board
          -> Board  -- the next board
markBoard = fmap . fmap . markCell
--markBoard bs = fmap (fmap markCell) bs

markCell :: Int        -- the drawn number
         -> Maybe Int  -- the current value of the cell
         -> Maybe Int  -- the next value of the cell
markCell n (Just c) | c == n = Nothing
markCell _ c = c
```

`markCell` uses [`guards`](https://en.wikibooks.org/wiki/Haskell/Control_structures) which is a control structure similar to if-then-else
expressions.

A winning board has either a complete row or column of
marked numbers.

```haskell
isBingo :: Board -> Bool
isBingo ns = or (any (and . fmap isNothing) <$> [transpose ns, ns])
-- isBingo ns = hasCompleteRow || hasCompleteColumn
--      where hasCompleteRow    = any (and (fmap isNothing)) ns
--            hasCompleteColumn = any (and (fmap isNothing)) (transpose ns)
```

We consider the `transpose` of the board to rotate the board 90 degrees and
pretend the columns are rows. Now we can run the same logic on the `transpose`d 
and original orientation and `or` them at the end.

And now for the fun part. Note that the next function can be thought of as 
"an action applied to a state to get the next state".

```haskell
stepBingo :: ([(Board, Int)], [Board])  -- The current state (a tuple of winning boards and current boards in play)
          ->    Int                     -- The action (the current drawn number)
          -> ([(Board, Int)], [Board])  -- The next state
stepBingo (wonBoards, boards) i = (wonBoards' ++ wonBoards, boards')  -- 5
    where (wonBoards', boards') = stepBingo' boards i                 -- 4

-- A helper function
stepBingo' :: [Board]                    -- The current boards in play
           ->  Int                       -- The current drawn number
           -> ([(Board, Int)], [Board])  -- A tuple of winning boards as a result of drawing the number, and the next boards in play
stepBingo' boards i = (fmap (flip (,) i) wonBoards, boards')          -- 3
    where (wonBoards, boards') = partition isBingo markedBoards       -- 2
          markedBoards = fmap (markBoard i) boards                    -- 1
```

In plain English,

```haskell
markedBoards = fmap (markBoard i) boards                              -- 1
```
`i` is the current drawn number. Go through each board and mark off any `i`s.

```haskell
(wonBoards, boards') = partition isBingo markedBoards                 -- 2
```
Now that we have marked off the boards, `partition` the list into `wonBoards`
and the next boards in play `boards'` (Note the apostrophe).

```haskell
stepBingo' boards i = (fmap (flip (,) i) wonBoards, boards')          -- 3
```
Recall the return value of our helper function `([(Board, Int)], [Board])`.
This is a tuple where the first part is the winning boards annotated with
the drawn number in the round where it won.
We do this by iterating through each board in `wonBoards` and tupling it with `i`.

The second part of the tuple is the next boards in play, which we simply pass
through.

```haskell
(wonBoards', boards') = stepBingo' boards i                           -- 4
```
This is the bit which calls our helper function. Nothing special :)

```haskell
stepBingo (wonBoards, boards) i = (wonBoards' ++ wonBoards, boards')  -- 5
```
All we need to do is append the winning boards this round with the winning boards
so far and return the next boards in play for the next round.

This is all nice and pretty but how does this solve our problem? I assume you've
already met my friend `fold`. If you haven't, here they are again.

```haskell
-- foldl :: (b -> a -> b) ->  b -> [a] ->  b

playBingo :: Draws           -- The drawn numbers in order
          -> [Board]         -- The starting boards in play
          -> [(Board, Int)]  -- A list of winning boards
playBingo draws boards = fst $ foldl stepBingo ([], boards) draws
```

Here we start with the initial state `([], boards)`, which means there are no
winning boards and all the (initial) boards are in play. 
We wish to `fold` over the list of `draws` taking each `draw` from the list
and applying it to the initial state with `stepBingo`.

To make it more obvious,
```haskell
-- :t foldl
foldl :: (b -> a -> b)  -- any binary operator
      ->  b             -- initial value      
      -> [a]            -- a list             
      ->  b             -- final value

-- :t stepBingo
stepBingo :: ([(Board, Int)], [Board])  -- b
          ->    Int                     -- a
          -> ([(Board, Int)], [Board])  -- b

-- :t ([], boards)
([], boards) :: ([(Board, Int)], [Board]) -- b

-- :t draws
draws :: [Int]
```

Almost there! We just need to calculate the score of the winning board.
Ignore the `Nothings` with `catMaybes`, take the sum of the board and multiply
it with the drawn number.

```haskell
score :: Board  -- The winning board
      -> Int    -- The drawn number when the board won
      -> Int    -- score
score = (*) . sum . fmap (sum . catMaybes)
score bs i = i * sum (fmap (sum . catMaybes) bs)
```

Now all we need to do is play bingo! Remember that `stepBingo` appends
the most recent winning boards to the `head` of the list. So the first
winning board of the game can be found in `last`.

```haskell
part1 :: [Int] -> [Board] -> Int
part1 = ((uncurry score . last) .) . playBingo
-- part1 draws boards = uncurry score (last (playBingo draws boards))
```

### Part 2

I swear this part won't be as long as part 1. The last board to have
won can be found in the `head` of the list. I know it's confusing.

```haskell
part2 :: [Int] -> [Board] -> Int
part2 = ((uncurry score . head) .) . playBingo
-- part2 draws boards = uncurry score (head (playBingo draws boards))
```

See, that wasn't that bad. Anyway two more gold stars :)

## Day 5 - Hydrothermal Venture
[Prompt][prompt02] - [Solution][day02src]

### Part 1

Let's start with some synonyms.

```haskell
type Point = (Int, Int)
type Line = (Point, Point)
type Board = M.Map Point Int
```

I'm using `Data.Map` from the [containers](https://hackage.haskell.org/package/containers-0.4.0.0) package to make my life easier. `Board`s are mappings from
points on a grid to integers, representing how many times they have been drawn
over.

Consider drawing a point on the board.

```haskell
drawPoint :: Point  -- the point
          -> Board  -- the current board
          -> Board  -- the resulting board
drawPoint = flip (M.insertWith (const (+ 1))) 1
-- drawPoint p b = M.insertWith (const (+ 1)) p 1 b
```

This is a combined insert and update operation that
- sets the value to `1` if the key (point) does not exist and
- increments the value by `1` if the key already exists

Now we need to draw lines on the board. Recall that a `Line` is defined by two `Point`s.

```haskell
drawLine :: Line   -- the line
         -> Board  -- the current board
         -> Board  -- the resulting board
drawLine ((x1, y1), (x2, y2)) board = foldr drawPoint board [(x1 + dx * i, y1 + dy * i) | i <- [0..d]]
    where d = max (abs (x1 - x2)) (abs (y1 - y2))
          dx = signum (x2 - x1)
          dy = signum (y2 - y1)
```

As you can tell, I **really** like `fold`s. Here we are taking advantage of a
nice trick. Since we are limited to vertical and horizontal lines, either `dx`
or `dy` must be zero. `d` is simply the [chessboard distance](https://en.wikipedia.org/wiki/Chebyshev_distance) between the two points.

We need to generate exactly `d` points to get from one point to the other and the
direction we need to go is determined by `dx` and `dy`.

Then it is just a matter of iterating (`fold`) through the points and
drawing (`drawPoint`) them on the `board`.

```haskell
isDiagonal :: Line -> Bool
isDiagonal ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

part1 :: [Line] -> Int
part1 = length . M.filter (>= 2) . foldr drawLine M.empty . filter isDiagonal
-- part1 vents = length $ M.filter (>= 2) $ foldr drawLine M.empty $ filter isDiagonal vents 
```

How do we iteratively draw lines on the board? With `fold` of course!

But first we need to filter out diagonal lines. Then we query the map for points
where at least 2 lines intersect and we're done!

### Part 2

We actually have to remove code for this one.

```haskell
part2 :: [Line] -> Int
part2 = length . M.filter (>= 2) . foldr drawLine M.empty
-- part2 vents = length $ M.filter (>= 2) $ foldr drawLine M.empty vents 
```

## Day 6 - Lanternfish

### Part 1

Fortunately we aren't dealing with the [Anglerfish from Outer Wilds](https://outerwilds.fandom.com/wiki/Anglerfish).

We are given a `[Int]` representing the internal timers of each lanternfish.
The timers decrease at each step. Once a timer reaches 0, it resets its timer
to 6 and a *new* laternfish spawns with a timer of 8.

```haskell
nextDay :: [Int]  -- the current generation of fish
        -> [Int]  -- the next generation of fish
nextDay fish = fish' ++ newFish
    where fish' = fmap step fish
          newFish = replicate (length (filter (== 0) fish)) 8

step :: Int  -- the current state of an internal timer
     -> Int  -- the next state of an internal timer
step 0 = 6
step x = x-1
```

We can run the simulation with `iterate` which repeatedly applies a function
to an initial value. We ignore the first 80 results (the resulting list
includes the initial state) to get the state of the fish at the 80th day.

```haskell
-- iterate :: (a -> a) -> a -> [a]
part1 :: [Int] -> Int
part1 = sum . head . drop 80 . iterate nextDay
```

### Part 2

Our naive solution won't work anymore! The sample input produces 27 billion
fish after 256 days and `fmap`ing over lists that big would take forever.

Let's instead keep track of the number of fish with the same internal timer value.
We can implicitly store the timer value by their position in the list, or in this
case, a tuple.

```haskell
nextDay' :: (Int, Int, Int, Int, Int, Int, Int, Int, Int) 
         -> (Int, Int, Int, Int, Int, Int, Int, Int, Int)
nextDay' (a, b, c, d, e, f, g, h, i) = (b, c, d, e, f, g, h+a, i, a)
```

Unfortunately, the next function isn't as nice but it does the job. 

```haskell
part2 :: [Int] -> Int
part2 fish = sum' (head (drop 256 (iterate nextDay' fish')))                     -- 4
    where (a:b:c:d:e:f:g:h:i:_) = [length (filter (== l) fish) | l <- [0 .. 8]]  -- 1
          fish' = (a, b, c, d, e, f, g, h, i)                                    -- 2
          sum' (s, t, u, v, w, x, y, z) = s + t + u + v + w + x + y + z          -- 3
```

Most of the mess comes from packing and dealing with tuples, but it is mostly 
similar to `part1`. In (1), we count how many fish of every possible internal timer value exists,
and destructure the resulting list. The references to elements of this list is used in (2) to construct
`fish'` which is a 9-tuple of the shape required by `nextDay'`. (3) declares `sum'`: a way
to sum 9-tuples. Finally, we put everything together in (4) and ohwow that's a lot of lanternfish.


[day01src]: day01.hs
[day02src]: day02.hs
[day03src]: day03.hs
[day04src]: day04.hs
[day05src]: day05.hs
[day06src]: day06.hs

[aoc]: https://adventofcode.com/2021
[prompt01]: https://adventofcode.com/2021/day/1
[prompt02]: https://adventofcode.com/2021/day/2
[prompt03]: https://adventofcode.com/2021/day/3
[prompt04]: https://adventofcode.com/2021/day/4
[prompt05]: https://adventofcode.com/2021/day/5
[prompt06]: https://adventofcode.com/2021/day/6
