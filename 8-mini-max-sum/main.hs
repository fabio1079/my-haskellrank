solve :: [Int] -> [Int]
solve xs = [min, max]
    where
        s = sum xs
        min = s - (maximum xs)
        max = s - (minimum xs)

main = interact $ unwords . map show . solve . map read . words