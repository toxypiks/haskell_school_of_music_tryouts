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

-- Exercise 1.

t251 :: Music Pitch
t251 = let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
           gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
           cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
       in  dMinor :+: gMajor :+: cMajor

twoFiveOne     :: Pitch -> Dur -> Music Pitch
twoFiveOne p d = let dMinor = note d (trans(2)p) :=: note d (trans(5)p) :=: note d (trans(9)p)
                     gMajor = note d (trans(7)p) :=: note d (trans(11)p) :=: note d (trans(14)p)
                     cMajor = note (d*2) p :=: note (d*2) (trans(4)p) :=: note (d*2) (trans(7)p)
                 in  dMinor :+: dMinor :+: gMajor :+: cMajor

play_twoFiveOne ::  Music Pitch
play_twoFiveOne = twoFiveOne (C,4) wn

data BluesPitchClass = Ro | MT | Fo | Fi | MS

type BluesPitch = (BluesPitchClass, Octave)

ro, mt, fo, fi, ms :: Octave -> Dur -> Music BluesPitch
ro o d = note d (Ro, o)
mt o d = note d (MT, o)
fo o d = note d (Fo, o)
fi o d = note d (Fi, o)
ms o d = note d (MS, o)

-- convert type Music BluesPitch to type Music for MIDI output

fromBlues :: Music BluesPitch -> Music Pitch
fromBlues (Prim(Note d (Ro, o))) = note d (C, o)
fromBlues (Prim(Note d (MT, o))) = note d (Ef, o)
fromBlues (Prim(Note d (Fo, o))) = note d (F, o)
fromBlues (Prim(Note d (Fi, o))) = note d (G, o)
fromBlues (Prim(Note d (MS, o))) = note d (Bf, o)
fromBlues (Prim (Rest d)) = rest d
fromBlues (m1 :+: m2) = (fromBlues m1) :+: (fromBlues m2)
fromBlues (m1 :=: m2) = (fromBlues m1) :=: (fromBlues m2)
fromBlues (Modify control m) = Modify control (fromBlues m)

myblues :: Music BluesPitch
myblues = rest wn :+: ro 4 hn :+: fi 4 hn :+: rest wn :+: ms 4 hn :+: fo 4 hn

play_myblues :: Music Pitch
play_myblues = fromBlues myblues

-- chapter 3
-- map function

f1      :: Int -> [Pitch] -> [Pitch]
f1 x ps = map (trans x) ps

f2 :: [Dur] -> [Music a]
f2 d_list = map rest d_list

f3         :: [Music Pitch] -> [Music Pitch]
f3 mp_list = let f (Prim(Note d p)) = note (d/2) p :+: rest (d/2)
             in map f mp_list

my_notes = [c 4 qn, d 4 en, e 4 hn]

modify_my_notes :: [Music Pitch]
modify_my_notes = f3 my_notes


main :: IO()
main = do
  play (mel 3)
