module Main where

import Euterpea

pcToQN    :: PitchClass -> Music Pitch
pcToQN pc = note qn (pc, 4)

twinkle =
  let m1 = line(map pcToQN [C,C,G,G,A,A]) :+: g 4 hn
      m2 = line(map pcToQN [F,F,E,E,D,D]) :+: c 4 hn
      m3 = line(map pcToQN [G,G,F,F,E,E]) :+: d 4 hn
  in line [m1,m2,m3,m3,m1,m2]

main = do
  play twinkle
