palindrom :: Eq a => [a] -> Bool

palindrom xs = xs == (reverse xs)