format :: [String] -> [[Int]]
format [] = []
format (x:xs) = lst:format xs
  where
    lst = (map read $ words x)::[Int]


solve xs = abs (dia1 - dia2)
  where
    -- zips = zip [0..] xs -- ex: [(0, [1,2,3]), (1, [4,5,6]), (2, [7,8,9])]
    dia1 = sum $ map (uncurry (!!)) $ zip xs [0..]
    dia2 = sum $ map (uncurry (!!)) $ zip (map reverse xs) [0..]


main = interact $ show . solve . format . tail . lines
