import Data.List
import Data.Char (isAlpha)

--4.4.1
brojiRazl :: Eq a => [a] -> Int
brojiRazl xs = (length . nub) xs

--4.4.2
brojSlova :: [Char] -> Int
brojSlova niz = (length . filter isAlpha) niz

--4.4.3
zadnji :: [[Char]] -> [Char]
zadnji  = map last


--4.4.4
duljinaListe :: Foldable t0 => t0 b0 -> Integer
duljinaListe = foldl (const . (1 +)) 0

--4.4.5
concat' :: Foldable t => t [a] -> [a]
concat' = foldr (++) []

--4.4.6
jePalindrom :: [Char] -> Bool
jePalindrom xs = lista == reverse lista
    where lista = filter isAlpha xs

--4.4.7
zamijeniZnak :: [Char] -> [Char]
zamijeniZnak = map (\x -> if x == ' ' then '_' else x)

--4.4.8
jednakeTrojke' :: Eq a => [(a, a, a)] -> [(a, a, a)]
jednakeTrojke' = filter (\(a,b,c)-> a==b && a==c && b==c)
