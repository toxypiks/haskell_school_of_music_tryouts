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
