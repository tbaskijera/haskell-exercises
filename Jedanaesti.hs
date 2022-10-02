dot :: [Int] -> [Int] -> Int
dot xs ys = sum (zipWith (*) xs ys)