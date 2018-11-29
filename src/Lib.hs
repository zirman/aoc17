module Lib
    ( day1
    , day1p2
    , day2
    ) where

-- day1 [1,1,2,2] == 1
-- day1 [1,1,1,1] == 4
-- day1 [1,2,3,4] == 0
-- day1 [9,1,2,1,2,1,2,9] == 9
day1 :: Num a => Eq a => [a] -> a
day1 xs = sum (fmap (\(x, y) -> if x == y then x else 0) (zip xs (drop 1 (cycle xs))))

-- day1p2 [1,2,1,2] == 6
-- day1p2 [1,2,2,1] == 0
-- day1p2 [1,2,3,4,2,5] == 4
-- day1p2 [1,2,3,1,2,3] == 12
-- day1p2 [1,2,1,3,1,4,1,5] == 4
day1p2 :: Num a => Eq a => [a] -> a
day1p2 xs = sum (fmap (\(x, y) -> if x == y then x else 0) (zip xs (drop (div (length xs) 2) (cycle xs))))

-- day2 [[5,1,9,5],[7,5,3],[2,4,6,8]]
-- day2 :: Num a => [[a]] -> a
day2 :: Num a => Ord a => [[a]] -> a
day2 xss = sum (fmap (\xs -> maximum xs - minimum xs) xss)

-- day3 1 = 0
-- day3 n = f 1 where
--     f s | n <= (s + 1) * (s + 1) = g ((s - 1) * (s - 1) - n)
--     f s = f (s + 2)
--     g s =
