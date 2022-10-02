-- 6.2.1 (prvi nacin)
import Data.Char (isUpper)

isTitleCased :: [Char] -> Bool
isTitleCased rijec = filter (\x -> isUpper (x !! 0)) s == s
  where
    s = words (rijec)

-- 6.2.1. (drugi nacin)
isTitleCased' :: String -> Bool
isTitleCased' recenica = and (map (\x -> isUpper (head x)) (words recenica))

-- 6.2.2
filename :: [Char] -> [Char]
filename path = reverse (takeWhile (/= '/') (reverse path))