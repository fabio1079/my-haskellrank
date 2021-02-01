import Data.List


solve :: [Int] -> Int
solve xs = count tallest xs
  where
    tallest = head $ reverse $ sort xs
    count x = length . filter (x==)


main = interact $ show . solve . map read . words . tail