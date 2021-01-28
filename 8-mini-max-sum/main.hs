import Data.List

solve :: [Int] -> [Int]
solve xs = [minimum sums, maximum sums]
    where
        sums = map sum $ map tail $ permutations xs

main = interact $ unwords . map show . solve . map read . words