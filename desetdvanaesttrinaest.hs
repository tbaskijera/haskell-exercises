pitagorineTrojke :: Int -> [(Int, Int, Int)]
pitagorineTrojke broj = [(x, y, z) | x <- [1 .. broj], y <- [1 .. broj], z <- [1 .. broj], x ^ 2 + y ^ 2 == z ^ 2]

parovi :: [Int] -> [(Int, Int)]
parovi xs = []

sortiranaLista :: [Int] -> Bool
sortiranaLista xs = True