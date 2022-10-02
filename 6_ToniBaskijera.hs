import Data.Char (isUpper)

-- 6.2.1
isTitleCased :: [Char] -> Bool
isTitleCased recenica = filter (\x -> isUpper (x !! 0)) s == s
  where
    s = words (recenica)

-- ili
isTitleCased' :: String -> Bool
isTitleCased' recenica = and (map (\x -> isUpper (head x)) (words recenica))

-- 6.2.2

filename :: [Char] -> [Char]
filename path = reverse (takeWhile (/= '/') (reverse path))

-- 6.2.3
