module Main (main) where

-- Excercise 3.7 Rewrite the definition of length non-recursively

my_length :: [Int] -> Int
my_length xs = sum (map (const 1) xs)

list_length :: Int
list_length = my_length[2,4,3,2,5,6]

main = do
  print $ list_length
