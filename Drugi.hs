ocjena :: Int -> Int

ocjena x | x < 50 = 1
         | x < 60 && x >= 50 = 2
         | x < 75 && x >= 60 = 3
         | x < 90 && x >= 75 = 4
         | x <= 100 && x>= 90 = 5
         | otherwise = 0