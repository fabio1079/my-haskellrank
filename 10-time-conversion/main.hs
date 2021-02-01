plus12 :: [Char] -> [Char]
plus12 time
  | hours == 12 = time 
  | (hours + 12) == 24 = "00" ++ (drop 2 time)
  | otherwise = (show (hours + 12)) ++ (drop 2 time)
  where
    hours = (read $ take 2 time)::Int

solve :: [Char] -> [Char]
solve time
  | ampm == "PM" = plus12 $ take 8 time
  | otherwise = check12 $ take 8 time
  where
    ampm = drop 8 time
    check12 time = if (take 2 time) == "12" then
                      "00" ++ (drop 2 time)
                   else
                      time

main = interact $ solve . head . words