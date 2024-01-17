module Main (main) where

import Euterpea

-- Excercise 1.4: Modify the definitions of hNote and hList so that each take an extra argument that specifies the interval of harmonization

hNote      :: Int -> Dur -> Pitch -> Music Pitch
hNote h d p = note d p :=: note d (trans (-h) p)

hList           :: Int -> Dur -> [Pitch] -> Music Pitch
hList h d []     = rest 0
hList h d (p:ps) = hNote h d p :+: hList h d ps

mel :: Int -> Music Pitch
mel h = hList h qn [(F,4), (A,4), (F,4)]

main :: IO()
main = do
  play (mel 3)
