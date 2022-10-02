import Data.List
import System.IO

median :: (Fractional a, Ord a) => [a] -> a
median xs = if length niz `mod` 2 == 0 then (niz !! pozicija + niz !! pozicija - 1) / 2 else niz !! pozicija
  where
    niz = sort xs
    pozicija = length niz `div` 2

calcMatrix f m = map (map f) m

printMatrix m = mapM_ print [x | x <- m]

zarezi xs = unwords ([if (elem (last x) [',', '.', '?', '!']) then x else x ++ "," | x <- words (xs)])

obrada = do
  contents <- readFile "text.txt"
  writeFile "izlaz.txt" (zarezi contents)
