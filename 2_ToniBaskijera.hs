import Data.List

--2.3.1
cijeliBrojevi :: [Int] -> [Int]
cijeliBrojevi = filter(\x -> x `mod` 10 == 0 && x<100)

cijeliBrojevi':: [Int] -> [Int]
cijeliBrojevi' lista = [x| x<-lista, x`mod`10 ==0, x<100]

-- 2.3.2
pozElementComp  :: [Int] -> Int
pozElementComp xs = sum [x| x <- xs, x>0]

pozElementRecursive :: [Int] -> Int 
pozElementRecursive [] = 0
pozElementRecursive (x:xs) = if x > 0 then x + pozElementRecursive xs else pozElementRecursive xs

-- 2.3.3
produkt :: [Int] -> Int 
produkt [] = 1
produkt (x:xs) = x * produkt(xs)

-- 2.3.4
elementi :: Ord a => [a] -> [a]
elementi l = if length l > 3 then take 3 (reverse(sort (nub l))) else reverse (sort (nub l))

-- 2.3.5
sumEven :: Num a => [a] -> a
sumEven [] = 0
sumEven [x] = x
sumEven (x:_:xs) = x + sumEven xs

-- 2.3.6
dodajA :: [String] -> [String]
dodajA = map(\x -> "a" ++ x)

-- 2.3.7
wordToList :: [a] -> [[a]]
wordToList a = map (\c -> [c]) a

listTouple :: [a1] -> [a2] -> [([a1], [a2])]
listTouple w1 w2 = zip (wordToList w1) (wordToList w2)

lengthWord :: [a] -> Int
lengthWord = length

equalLength :: [a1] -> [a2] -> Int
equalLength w1 w2 = abs(lengthWord w1 - lengthWord w2)

equalCharTouple :: Eq a1 => [a1] -> [a1] -> [([a1], [a1])]
equalCharTouple w1 w2 = [(a,b) | (a,b)<-listTouple w1 w2, a==b]

wordHamming :: Eq a2 => [a2] -> [a2] -> Int
wordHamming w1 w2 = length(listTouple w1 w2 \\ equalCharTouple w1 w2) + equalLength w1 w2

-- 2.3.8
fib 0 = 0
fib 1 = 1
fib n = fib (n -1) + fib (n -2)

nfib :: Int -> [Integer]
nfib n = take n (map fib [0..])