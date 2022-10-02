module Sedmi where

djelitelji :: Int -> [Int]
djelitelji broj = filter (\x -> broj `mod` x == 0) [1 .. broj]

--djelitelji y = filter(\x -> y `mod`x == 0)[1..y]