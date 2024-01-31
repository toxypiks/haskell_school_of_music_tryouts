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
      p1 = rest qn :+: rest en :+: gs 3 en
      m3 = a 3 qn :+: rest en :+: b 3 en :+: c 4 qn :+: rest en :+: gs 3 en
      m4 = a 3 qn :+: rest en :+: b 3 en :+: c 4 qn :+: rest en :+: f 4 en :+: e 4 en :+: a 3 en :+: c 4 en :+: e 4 en
      m5 = tempo (3/2)(line [ef 4 qn :+: e 4 en :+: d 4 en]) :+: tempo (3/2) (line [c 4 en :+: a 3 en :+: g 3 en]) :+: a 3 wn
  in line  [m0,p1,m3,m4,m5]--[m0,m1,m1,m2,m1,p1,m3,m4,m5]

main = do
  play pink_panther_voice
