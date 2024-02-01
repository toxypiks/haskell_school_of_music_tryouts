module Main where

import Euterpea

pcToQN    :: PitchClass -> Music Pitch
pcToQN pc = note qn (pc, 4)

twinkle =
  let m1 = line(map pcToQN [C,C,G,G,A,A]) :+: g 4 hn
      m2 = line(map pcToQN [F,F,E,E,D,D]) :+: c 4 hn
      m3 = line(map pcToQN [G,G,F,F,E,E]) :+: d 4 hn
  in line [m1,m2,m3,m3,m1,m2]

-- Exercise 4.1 Find a simple piece of music and transcribe it into Euterpea
-- Pink Panther theme

pcToOC4 :: PitchClass -> Dur -> Music Pitch
pcToOC4 pc d = note d (pc, 4)

pink_panther_voice =
  let m0 = line(map pcToQN [C])
      m1 = cs 4 en :+: d 4 qn :+: ds 4 en :+: e 4 bn
      m2 = ds 4 en :+: d 4 qn :+: cs 4 en :+: c 4 bn
      p1 = rest qn :+: rest en
      m3 = gs 3 en :+: a 3 qn :+: rest en :+: b 3 en :+: c 4 qn :+: rest en
      m4 = gs 3 en :+: a 3 qn :+: b 3 en :+: c 4 qn :+: f 4 en :+: e 4 qn :+: a 3 en :+: c 4 qn :+: e 4 en :+: ef 4 dhn
      m5 = tempo (3/2)(line [ef 4 qn :+: e 4 en :+: d 4 en]) :+: tempo (3/2) (line [c 4 en :+: a 3 en :+: g 3 en]) :+: a 3 wn
  in line [m0,m1,m1,m2,m1,p1,m3,m4,m5]

-- Exercise 4.2 Try using prefix on your own melody

prefixes :: [a] -> [[a]]
prefixes [] = []
prefixes (x:xs) = let f pf = x : pf
                  in [x] : map f (prefixes xs)

prefix :: [Music a] -> Music a
prefix mel = let m1 = line(concat(prefixes mel))
                 m2 = transpose 12 (line(concat(prefixes(reverse mel))))
                 m = instrument Flute m1 :=: instrument VoiceOohs m2
             in m :+: transpose 5 m :+: m

mel1 = [c 5 en,e 5 sn, g 5 en,b 5 sn,a 5 en,f 5 sn,d 5 en,b 4 sn,c 5 en]

mel1_pref = prefix mel1

main = do
  play mel1_pref
