symbols :: Int -> [Char]
symbols n
  | n <= 0 = []
  | otherwise = '#':symbols (n-1)

spaces :: Int -> [Char]
spaces n
  | n <= 0    = []
  | otherwise = ' ':spaces (n-1)

--   #: 2 sp ++ 1 sy
--  ##: 1 sp ++ 2 sy
-- ###: 0 sp ++ 3 sy
staircase :: Int -> Int -> [[Char]]
staircase sp sy
  | sp == 0   = []
  | otherwise = line:staircase (sp-1) (sy+1)
    where
      line = spaces (sp-1) ++ symbols sy

solve :: Int -> [[Char]]
solve n = staircase n 1

main = interact $ unlines . solve . read