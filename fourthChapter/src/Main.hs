module Main where

import Euterpea

pcToQN    :: PitchClass -> Music Pitch
pcToQN pc = note qn (pc, 4)

twinkle =
  let m1 = line(map pcToQN [C,C,G,G,A,A]) :+: g 4 hn
      m2 = line(map pcToQN [F,F,E,E,D,D]) :+: c 4 hn
      m3 = line(map pcToQN [G,G,F,F,E,E]) :+: d 4 hn
  in line [m1,m2,m3,m3,m1,m2]

my_length :: [Int] -> Int
my_length xs = sum (map (const 1) xs)

my_length_do :: Int
my_length_do = my_length[2,4,3,2,5,6]

-- Pink Panther theme

pcToEN :: PitchClass -> Music Pitch
pcToEN pc = note en (pc, 4)

pink_panther_voice =
  let m0 = line(map pcToQN [C])
      m1 = cs 4 en :+: d 4 qn :+: ds 4 en :+: e 4 bn
      m2 = ds 4 en :+: d 4 qn :+: cs 4 en :+: c 4 bn
  in line [m0,m1,m1,m2,m1]

main = do
  play pink_panther_voice
