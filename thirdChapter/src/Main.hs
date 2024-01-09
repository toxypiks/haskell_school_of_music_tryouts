import Euterpea

-- Excercise 3.7 Rewrite the definition of length non-recursively.

my_length :: [Int] -> Int
my_length xs = sum (map (const 1) xs)

list_length :: Int
list_length = my_length[2,4,3,2,5,6]

-- Implementation of fold function due to learning purposes

my_fold :: (a -> b -> a) -> a -> [b] -> a
my_fold func start_val [] = start_val
my_fold func start_val (x:xs) = my_fold func (func start_val x) xs

-- my_fold add 0 [1 ,3 , 4 ,6] = my_fold add (add 0 1) [3, 4 ,6]
-- -> my_fold add 1 [3, 4, 6] = my_fold add (add 1 3) [4,6]
-- -> my_fold add 4 [4, 6] = my_fold add (add 4 4) [6]
-- -> my_fold add 8 [6] = my_fold add (add 8 + 6) []
-- -> my_fold add 14 [] = 14

my_add :: (Num a) => a -> a -> a
my_add x y = x + y

my_add_aux :: (Num a) => a -> b -> a
my_add_aux x _ = x + 1

list_length_two :: (Num a) => [a] -> Int
list_length_two xs = my_fold my_add_aux 0 xs

-- Excercise 3.8 - caution! I'm using simplification through currying
-- a) Double each number in a list.
doubleInt  :: Int -> Int
doubleInt x = x * 2

doubleEach :: [Int] -> [Int]
doubleEach = map doubleInt

-- b) Pair each element in a list with that number and one plus that number.
oneAndOne :: Int -> (Int, Int)
oneAndOne x = (x, x + 1)

pairAndOne :: [Int] -> [(Int,Int)]
pairAndOne = map (oneAndOne)

-- c) Add together each pair in a list.
sumTuple :: (Int, Int) -> Int
sumTuple (x,y) = x + y

addEachPair :: [(Int, Int)] -> [Int]
addEachPair = map (sumTuple)

-- d) Add "pointwise" the element of a list of pairs.
my_add_tuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
my_add_tuple (x, y) (a, b) = (x + a, y + b)

my_add_tuple_list :: [(Int, Int)] -> (Int, Int)
my_add_tuple_list xs = my_fold my_add_tuple (0, 0) xs

-- Excercise 3.9
-- Define a function that combines a list of durations with a list of notes to create complete notes
fuse :: [Dur] -> [Dur -> Music a] -> [Music a]
fuse _ [] = []
fuse [] _ = []
fuse (d:dl) (n:nl) = (n d) : fuse dl nl

-- Exercise 3.10

max_abspitch :: [AbsPitch] -> AbsPitch
max_abspitch [] = error "empty list"
max_abspitch [x] = x
max_abspitch (x1:x2:xs) = max_abspitch ((if x1 >= x2 then x1 else x2) :xs)

min_abspitch :: [AbsPitch] -> AbsPitch
min_abspitch [] = error "empty list"
min_abspitch [x] = x
min_abspitch (x1:x2:xs) = min_abspitch ((if x1 <= x2 then x1 else x2) :xs)


my_max :: [Int] -> Int
my_max [] = error "empty list"
my_max [x] = x
my_max (x1:x2:xs) | x1 >= x2 = my_max (x1:xs)
                  | x1 < x2 = my_max (x2:xs)

my_fold_max :: [Int] -> Int
my_fold_max (x:xs) = foldr max (x) xs

-- Exercise 3.11

reverse_list :: [Int] -> [Int]
reverse_list [] = []
reverse_list (x:xs) = reverse_list xs ++ [x]


make_list :: Int -> Int -> [Int]
make_list x y | x < y = [x .. y]
              | x > y = reverse_list [y .. x]
              | x == y = [x]

-- without list comprehension
create_list :: Int -> [Int]
create_list 0 = []
create_list length = (0 : create_list(length -1))

-- Exercise 3.12

count_up :: Int -> [Int] -> [Int]
count_up x (y:ys) = (x+y :(y:ys))

count_up_list x list = reverse_list $ foldr (count_up) [x] (reverse_list list)

chrom :: Pitch -> Pitch -> [Music Pitch]
chrom p1 p2 = map (note qn) $ map pitch $ make_list (absPitch p1) (absPitch p2)

-- Exercise 3.13

data MajorScale = MyIonian | MyDorian | MyPhrygian | MyLydian | MyMixolydian | MyAeolian | MyLocrian deriving Eq

genScale :: MajorScale -> [Music Pitch]
genScale ms
  | ms == MyIonian = map (note qn) $ map pitch (count_up_list (absPitch (C,4)) [2,2,1,2,2,2,1])
  | ms == MyDorian = map (note qn) $ map pitch (count_up_list (absPitch (D,4)) [2,1,2,2,2,1,2])
  | ms == MyPhrygian = map (note qn) $ map pitch (count_up_list (absPitch (E,4)) [1,2,2,2,1,2,2])
  | ms == MyLydian = map (note qn) $ map pitch (count_up_list (absPitch (F,4)) [2,2,2,1,2,2,1])
  | ms == MyMixolydian = map (note qn) $ map pitch (count_up_list (absPitch (G,4)) [2,2,1,2,2,1,2])
  | ms == MyAeolian = map (note qn) $ map pitch (count_up_list (absPitch (A,4)) [2,1,2,2,1,2,2])
  | ms == MyLocrian = map (note qn) $ map pitch (count_up_list (absPitch (B,4)) [1,2,2,1,2,2,2])

-- Exercise 3.14

pcToQN :: PitchClass -> Music Pitch
pcToQN p = note qn (p, 4)

pcToEN :: PitchClass -> Music Pitch
pcToEN p = note en (p, 4)

are_u_sleeping =
  let m1 = line (map pcToQN [C,D,E,C])
      m2 = line (map pcToQN [E,F]) :+: g 4 hn
      m3 = g 4 qn :+: line (map pcToEN [A,G,F]) :+: line (map pcToQN [E,C])
      m4 = c 4 qn :+: g 3 qn :+: c 4 hn
  in [m1,m1,m2,m2, m3, m3, m4, m4]


-- Exercise 3.15

decrypt :: String -> String
decrypt [] = []
decrypt (x:xs) = toEnum((fromEnum x) + 1) : decrypt xs

encrypt :: String -> String
encrypt [] = []
encrypt (x:xs) = toEnum((fromEnum x) -1) : encrypt xs
