sep :: [Int] -> [Int] -> Int -> Int -> (Int, Int)
sep [] _ ca cb = (ca, cb)
sep _ [] ca cb = (ca, cb)
sep (a:as) (b:bs) ca cb
  | a > b     = sep as bs (ca+1) cb
  | b > a     = sep as bs ca (cb+1)
  | otherwise = sep as bs ca cb


solve (a1:a2:a3:b1:b2:b3:_) = (show res_a) ++ " " ++ (show res_b)
  where
    (res_a, res_b) = sep [a1, a2, a3] [b1, b2, b3] 0 0


main = interact $ solve . map read . words
