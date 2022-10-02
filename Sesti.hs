import Data.List (nub, sort)  

posloziListu :: Ord a => [a] -> [a]

posloziListu xs = nub (sort xs)



