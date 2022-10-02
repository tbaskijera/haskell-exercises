import Data.Char
import System.IO

-- 5.2.1

{-

Prelude> :t putStrLn
putStrLn :: String -> IO ()

Prelude> :t getLine
getLine :: IO String

Prelude> :t putChar
putChar :: Char -> IO ()

Prelude> :t putStrLn "abcd"
putStrLn "abcd" :: IO ()

Prelude> prvi <- getLine
123
Prelude> :t prvi
prvi :: String

Prelude> drugi <- putStrLn "456"
456
Prelude> :t drugi
drugi :: ()

-}

-- 5.2.2
funkcija x = do
  line <- getChar
  if line == '\n'
    then do
      putStr (space (abs (razmak x)) (" " ++) "O")
      funkcija (x + 0.1)
    else putStrLn (" ")

zbroji = 1

razmak x = round (10 * (sin (3 * x - 1)))

space :: Int -> (a -> a) -> a -> a
space n f a = iterate f a !! n

-- 5.2.3
read = do
  contents <- readFile "citaj.txt"
  putStr contents

write = do
  contents <- readFile "citaj.txt"
  writeFile "write.txt" (map toLower contents)

add = do
  todoItem <- getLine
  appendFile "write.txt" (todoItem ++ "\n")

readHandler = do
  withFile
    "write.txt"
    ReadMode
    ( \handle -> do
        contents <- hGetContents handle
        putStr contents
    )

writeHandler = do
  withFile
    "write.txt"
    ReadWriteMode
    ( \handle -> do
        contents <- hGetContents handle
        writeFile "novi3.txt" contents
    )

addHandler = do
  withFile
    "read.txt"
    ReadWriteMode
    ( \handle -> do
        contents <- hGetContents handle
        appendFile "write.txt" contents
    )

-- 5.2.4
read_write = do
  contents <- readFile "old.txt"
  let convert = map (\x -> if isLower x == True then toUpper x else toLower x)
  writeFile "new.txt" (convert contents)
