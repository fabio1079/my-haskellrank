chars :: Int -> Char -> [Char]
chars n c
  | n <= 0 = []
  | otherwise = c:chars (n-1) c

--   #: 2 sp ++ 1 sy
--  ##: 1 sp ++ 2 sy
-- ###: 0 sp ++ 3 sy
staircase :: Int -> Int -> [[Char]]
staircase sp sy
  | sp == 0   = []
  | otherwise = line:staircase (sp-1) (sy+1)
    where
      line = chars (sp-1) ' ' ++ chars sy '#'

solve :: Int -> [[Char]]
solve n = staircase n 1

main = interact $ unlines . solve . read