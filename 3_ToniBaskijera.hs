--3.3.1
--prva varijanta, primjenimo na prvi element i vraćamo samo taj promijenjeni element a ne listu
primjeniNaPrvim :: (Int -> Int) -> [Int] -> Int
primjeniNaPrvim f xs = f (xs!!0)
--druga varijanta, primjenimo na prvi element i vraćamo cijelu listu s promjenjenim prvim elementom
primjeniNaPrvim' :: (Int-> Int) -> [Int] -> [Int]
primjeniNaPrvim' f xs = f(head xs) : tail xs


--3.3.2
--prva varijanta, primjenimo na zadani element i vraćamo samo taj promijenjeni element a ne listu
primijeniNaZadani :: (Int -> Int) -> [Int] -> Int -> Int
primijeniNaZadani f s n = f (s!!(n-1))
--druga varijanta, primjenimo na zadani element i vraćamo cijelu listu s promjenjenim zadanim elementom
doSomething :: (Int -> Int) -> Int -> [Int] -> [Int]
doSomething f i l = [if p == i+1 then f v else v | (p, v) <- zip [1..] l]

--3.3.3
zipp :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipp _ [] _ = []  
zipp _ _ [] = []  
zipp f (x:xs) (y:ys) = f x y : zipp f xs ys  


--3.3.4
polinom :: (RealFrac a, Integral b, Floating a) => [a] -> [b]
polinom xs = map (\x -> round ((7*x)**2 + (2*x)**3 -13)) xs

--3.3.5
zbrojKorijena :: [Float] -> Int 
zbrojKorijena xs = round(sum (map sqrt xs))


--3.3.6
dvoznamenkastiPozitivni :: [Int] -> [Int]
dvoznamenkastiPozitivni xs = filter(>9) xs

--3.3.7
povecajPoz :: [Int] -> [Int]
povecajPoz xs = map(+1) (filter (>0) xs)

--3.3.8
--- iterate stvara beskonacnu listu gdje se prvi sljedeci element izracunava primjenjujuci funkciju (prvi argument) na drugi argument, a svaki sljedeci
--- element primjenjujuci funkciju na prosli element, a na kraju uzmemo n-ti element beskonance liste(koja se ubiti izracuna do n-te pozicije zbog
--- lijene evaluacije)
zavrsiRecenicu :: String -> String
zavrsiRecenicu recenica = recenica ++ "."

primijeniNputa :: Int -> (a -> a) -> a -> a
primijeniNputa n f x = (iterate f x)!!n

