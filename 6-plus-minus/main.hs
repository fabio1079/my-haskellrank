import Text.Printf


precision6 :: Double -> String
precision6 = printf "%.6f"


sep :: Int -> [Int] -> [Int]
sep cur (a:b:c:_)
  | cur < 0   = [a + 1, b, c]
  | cur == 0  = [a, b + 1, c]
  | otherwise = [a, b, c + 1]


solve :: [Int] -> [String]
solve (x:xs) = [precision6 plus, precision6 negs, precision6 zeros]
  where
    divit n = ((fromIntegral n) /  (fromIntegral x))::Double
    [negs, zeros, plus] = map divit $ foldr sep [0, 0, 0] xs


main = interact $ unlines . solve . map read . words
