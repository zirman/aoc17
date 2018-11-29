module Lib
    ( day1
    , day2
    ) where

-- day1 [1,1,2,2] == 1
-- day1 [1,1,1,1] == 4
-- day1 [1,2,3,4] == 0
-- day1 [9,1,2,1,2,1,2,9] == 9
day1 :: Num a => Eq a => [a] -> a
day1 xs = sum (fmap (\(x, y) -> if x == y then x else 0) (zip xs (drop 1 (cycle xs))))

-- day2 [[5,1,9,5],[7,5,3],[2,4,6,8]]
-- day2 :: Num a => [[a]] -> a
day2 :: Num a => Ord a => [[a]] -> a
day2 xss = sum (fmap (\xs -> maximum xs - minimum xs) xss)
