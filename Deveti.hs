import Osmi

prostiDo_n :: Int -> [Int]

prostiDo_n n = filter (\x-> prostBroj x == True) [1..n]